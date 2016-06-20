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
# names(observed) <- c("SWC","SWP")

Geotop_VisSoilWaterRet_gg <- function(alpha = 0.94, n = 1.5, theta_sat = 0.50, theta_res = 0.05, accurate = 10,
                                      add_ref_curves = T, observed)
{
  # soil water pressure head in centimeter
  psi <- seq(1,10000000,accurate)
  
  # volumetric soil water content in vol%
  swc <- swc(psi = -psi, alpha = alpha, n = n, theta_sat = theta_sat, theta_res = theta_res) *100
  
  pFdata <- as.data.frame(cbind(swc, psi))
  
  p <- ggplot(observed, aes(x = log10(SWP), y = SWC), log="x") + 
    #stat_density2d(aes(alpha=..level..), geom="polygon") +
    #geom_density_2d(col="black", alpha=.5, lineend="round", linejoin="round", na.rm=TRUE) +
    #scale_alpha_continuous(limits=c(0,0.2), breaks=seq(0,0.2,by=0.025)) +
    geom_point(colour="red", alpha=0.15, size=.5) +
    geom_line(data = pFdata, mapping = aes(x = log10(psi), y= swc), alpha = .5, col="red") +
    ylim(0,60) + xlim(0,7) + 
    guides(alpha=FALSE, color=FALSE) + 
    ylab("SWC") + xlab("pF") + 
    theme(legend.position="bottom")
  
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
    
    p <- p + geom_line(data = swc_clay, mapping = aes(x = log10(psi), y= swc_clay), alpha = .25) +
      geom_line(data = swc_loam, mapping = aes(x = log10(psi), y= swc_loam), alpha = .25) +
      geom_line(data = swc_sand, mapping = aes(x = log10(psi), y= swc_sand), alpha = .25) + 
      geom_text(data = txt, mapping = aes(label=lab, x=x, y=y), alpha=.6)
  }
  
  if (all(is.na(observed$SWC))) {
    gg <- ggExtra::ggMarginal(p, type = c("density"), margins = "x")
  } else if (all(is.na(observed$SWP))) {
    gg <- ggExtra::ggMarginal(p, type = c("density"), margins = "y")
  } else {
    gg <- ggExtra::ggMarginal(p, type = c("density"), margins = "both")
  }
  
  return(gg)
  
}
