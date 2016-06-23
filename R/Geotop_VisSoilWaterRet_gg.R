# 
# library(ggplot2)
# library(soilwater)
# 
# observedLaimburg <- read.csv("/media/alpenv/Projekte/MONALISA/04_Daten & Ergebnisse/09_Pedotranfer_Function/Data_for_Johannes/data/Arduino_Laimburg_Joined_27072014-12102015_BrJ.csv", header=T)
# observed_20 <- observedLaimburg[,c(2,4)]
# # SWP in hPa
# observed_20[,2] <- (-1) * observed_20[,2]
# observed_20[,2] <- ifelse(observed_20[,2]<=1, NA, observed_20[,2])
# observed_40 <- observedLaimburg[,c(3,5)]
# 
# observed <- observed_20
# names(observed) <- c("SWC_20","SWP_20")

Geotop_VisSoilWaterRet_gg <- function(alpha = 0.94, n = 1.5, theta_sat = 0.50, theta_res = 0.05, accurate = 10,
                                      add_ref_curves = T, observed, colors = "red")
{
  # soil water pressure head in centimeter
  psi <- seq(1,10000000,accurate)
  
  # volumetric soil water content in vol%
  swc <- list()
  for (i in 1:length(alpha))
    swc[[i]] <- swc(psi = -psi, alpha = alpha[i], n = n[i], theta_sat = theta_sat[i], theta_res = theta_res[i]) *100
  
  datapoly1 <- data.frame(id = rep(1,4), x = c(1.8,1.8,2.5,2.5), y = c(0,60,60,0))
  datapoly2 <- data.frame(id = rep(1,4), x = c(2.5,2.5,4.2,4.2), y = c(0,60,60,0))
  
  observed$depth <- as.factor(observed$depth)
  
  p <- ggplot(observed, aes(x = log10(SWP), y = SWC, group = depth), log="x") + 
    #stat_density2d(aes(alpha=..level..), geom="polygon") +
    #geom_density_2d(col="black", alpha=.5, lineend="round", linejoin="round", na.rm=TRUE) +
    #scale_alpha_continuous(limits=c(0,0.2), breaks=seq(0,0.2,by=0.025)) +
    geom_vline(xintercept = c(1.8,2.5,4.2), col="grey") +
    geom_polygon(datapoly1, mapping = aes(x = x, y = y), fill=rgb(0,1,0,.15), col="grey") +
    geom_polygon(datapoly2, mapping = aes(x = x, y = y), fill=rgb(0,1,0,.25), col="grey") +
    geom_text(mapping = aes(label="FieldCapacity", x=2.15, y=57), alpha=.6, col="darkgreen", size=4) +
    geom_text(mapping = aes(label="PermanentWiltingPoint", x=4.2, y=45), alpha=.6, col="darkgreen", size=4, angle=90) +
    geom_point(colour="darkgrey", alpha=0.1, size=1) +

    #coord_trans(x = "log10") +
    # scale_x_log10(
    #    breaks = scales::trans_breaks("log10", function(x) 10^x),
    #    labels = scales::trans_format("log10", scales::math_format(10^.x))
    #  ) +
    ylim(0,60) + xlim(0,7) + 
    guides(alpha=FALSE, color=FALSE) + 
    ylab("SWC in vol.%") + xlab("pF | log10(SWP in hPa)") + 
    theme(axis.text=element_text(size=14), axis.title=element_text(size=16,face="bold")) +
    #scale_x_log10(name = "hPa", breaks = c(0,10,10^2,10^3,10^4,10^5,10^6,10^7), labels = scales::math_format(.x)) + 
    theme_bw() +
    #theme(panel.grid.minor = element_blank())
    annotation_logticks(base = 10, sides = "b")
  
  if (add_ref_curves)
  {
    # add reference curves for clay, loam and sand
    # source for vanGenuchten parameters: ROSETTA manual
    swc_clay <- swc(psi = -psi, alpha = 0.015, n = 1.25, theta_sat = .459, theta_res = .098) *100
    swc_loam <- swc(psi = -psi, alpha = 0.011, n = 1.47, theta_sat = .399, theta_res = .061) *100
    swc_sand <- swc(psi = -psi, alpha = 0.035, n = 3.17, theta_sat = .375, theta_res = .029) *100
    
    swc_clay <- as.data.frame(cbind(swc_clay, psi))
    swc_loam <- as.data.frame(cbind(swc_loam, psi))
    swc_sand <- as.data.frame(cbind(swc_sand, psi))
    
    txt <- as.data.frame(c("clay","loam","sand")); names(txt) <- "lab"
    txt$x <- c(0.1,0.1,0.1); txt$y <- c(47,41,37)
    
    p <- p + geom_line(data = swc_clay, mapping = aes(x = log10(psi), y= swc_clay), alpha = .25, size=1.25) +
      geom_line(data = swc_loam, mapping = aes(x = log10(psi), y= swc_loam), alpha = .25, size=1.25) +
      geom_line(data = swc_sand, mapping = aes(x = log10(psi), y= swc_sand), alpha = .25, size=1.25) + 
      geom_text(data = txt, mapping = aes(label=lab, x=x, y=y), alpha=.6, size=3.5)
  }
  
  for (i in 1:length(swc))
  {
    pFdata <- as.data.frame(cbind(swc=swc[[i]], psi=psi))
    p <- p + 
      geom_line(data = pFdata, mapping = aes(x = log10(psi), y = swc), alpha = .5, col=colors[i], size=2)
  }
  
  p + scale_color_manual(values = colors)
  
  # Marginal density plot of x (top panel)
  if (!all(is.na(observed$SWP))) 
  {
    xdensity <- ggplot(observed, aes(x = log10(SWP), fill = depth)) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = colors) + 
      theme(legend.position = "none") +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
  }
  
  # Marginal density plot of y (right panel)
  if (!all(is.na(observed$SWC))) 
  {
    ydensity <- ggplot(observed, aes(x = SWC, fill = depth)) + 
      geom_density(alpha=.5) + 
      scale_fill_manual(values = colors) + 
      theme(legend.position = "none") + 
      xlim(0,60) + 
      coord_flip() + 
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank())
  }
  
  if (exists("ydensity") & exists("xdensity")) {
    
    blankPlot <- ggplot() + geom_blank(aes(1,1)) +
      theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
      )
    
    p <- grid.arrange(xdensity, blankPlot, p, ydensity, 
                 ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
  } else {
    if (exists("xdensity"))
    p <- grid.arrange(xdensity, p, 
                      ncol=1, nrow=2, widths=c(10), heights=c(2.5,7.5))
    
    if (exists("ydensity"))
    p <- grid.arrange(p, ydensity, 
                       ncol=2, nrow=1, widths=c(7.5, 2.5), heights=c(10)) 
  }
  
  
  # if (all(is.na(observed$SWC))) {
  #   gg <- ggExtra::ggMarginal(p, type = c("density"), margins = "x",  xparams = list(size=2))
  # } else if (all(is.na(observed$SWP))) {
  #   gg <- ggExtra::ggMarginal(p, type = c("boxplot"), margins = "y",  yparams = list(size=2))
  # } else {
  #   gg <- ggExtra::ggMarginal(p, type = c("density"), margins = "both",  xparams = list(size=2),  yparams = list(size=2))
  # }
  
  return(p)
  
}
