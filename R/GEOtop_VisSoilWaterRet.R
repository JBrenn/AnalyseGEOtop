# visualize soil water retention curve and GEOtop model parameterization
# using swc function from soilwater

# data from point.RData (GEOtop_ReadMultiPoint)

# wpath="/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_B2_003"
# 
# wpath="/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_P2_002/"
# 
# library(geotopbricks)
# library(soilwater)
# 
# soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE)
# 
# alpha <- soil_input$alpha[1]
# n <- soil_input$n[1]
# theta_sat <- soil_input$vwc_s[1]
# theta_res <- soil_input$vwc_r[1]
# theta_pwp <- soil_input$vwc_w[1]
# theta_fc <- soil_input$vwc_fc[1]

GEOtop_VisSoilWaterRet <- function(alpha, n, theta_sat, theta_res, theta_pwp, theta_fc, observed=FALSE, add_ref_curves=TRUE, pdf=TRUE, accurate=10) 
{
  # soil water pressure head in centimeter
  psi <- seq(1,10000000,accurate)
   
  # volumetric soil water content in %
  swc <- swc(psi = -psi, alpha = alpha, n = n, theta_sat = theta_sat, theta_res = theta_res) *100
  
  if (pdf) pdf("./SoilWaterRetentionCurve.pdf")
  
    op <- par(las=1, pty="s")
    # swc vs. log(psi) = pF
  if (!is.null(observed))
  {
    plot(x = observed[,2], y = observed[,1] *100, log="x", col = grey.colors(n = 1, start = .6, end = .6, alpha = .3), 
           ylab="", xlab="", bty="n", xaxt="n", yaxt="n", 
         ylim=c(0,60), xlim=c(1,10000000))
    
    par(new=TRUE)
    
    plot(psi, swc, type="l", xlab="pF", ylab="Soil Water Content  [volume %]", xaxt="n", log="x",
         main=paste("SoilWaterRetentionCurve | alpha=", alpha, ", n=", n, ", res=", theta_res, ", sat=", theta_sat, sep=""), lwd=2,
         ylim=c(0,60), xlim=c(1,10000000))
  } else {
    plot(psi, swc, type="l", xlab="pF", ylab="Soil Water Content  [volume %]", log="x", xaxt="n", 
         main=paste("SoilWaterRetentionCurve | alpha=", alpha, ", n=", n, sep=""),
         ylim=c(0,60), xlim=c(1,10000000) )
  }
  
  if (add_ref_curves)
  {
    # add reference curves for clay, loam and sand
    # source for vanGenuchten parameters: ROSETTA manual
    swc_clay <- swc(psi = -psi, alpha = 0.015, n = 1.25, theta_sat = .459, theta_res = .098) *100
    swc_loam <- swc(psi = -psi, alpha = 0.011, n = 1.47, theta_sat = .399, theta_res = .061) *100
    swc_sand <- swc(psi = -psi, alpha = 0.035, n = 3.17, theta_sat = .375, theta_res = .029) *100
    
    par(new=TRUE)
    
    plot(psi, swc_clay, type="l", xlab="", ylab="", log="x", xaxt="n", yaxt="n", bty="n",
         col =  grey.colors(n = 1, start = .1, end = .3, alpha = .6), lty="dashed", lwd=1.2,
         ylim=c(0,60), xlim=c(1,10000000) )
    
    par(new=TRUE)
    
    plot(psi, swc_loam, type="l", xlab="", ylab="", log="x", xaxt="n", yaxt="n", bty="n",
         col =  grey.colors(n = 1, start = .1, end = .3, alpha = .6), lty="dashed", lwd=1.2,
         ylim=c(0,60), xlim=c(1,10000000) )
  
    par(new=TRUE)
    
    plot(psi, swc_sand, type="l", xlab="", ylab="", log="x", xaxt="n", yaxt="n", bty="n",
         col =  grey.colors(n = 1, start = .1, end = .3, alpha = .6), lty="dashed", lwd=1.2,
         ylim=c(0,60), xlim=c(1,10000000) ) 
    
    text(x = 1 , y = c(45.9,39.9,37.5)+.75, labels = c("clay","loam","sand"), 
         col=grey.colors(n = 1, start = .1, end = .3, alpha = .6))
    
    #labels = c("clay (0.015,9.54)","loam (0.011,1.47)","sand (0.35,3.17)")
  }
  
    ticks <- c(0,1,2,3,4,5,7) 
    labels <- sapply(ticks, function(i) as.expression(i))
    axis(1, at=c(1, 10, 100, 1000, 10000, 100000, 10000000), labels=labels)
    
    abline(v=c(10^1.8,10^2.5,10^4.2), lty="dashed", col=grey.colors(n = 1, .8, .8, .5))
    abline(h=c(theta_fc,theta_pwp,theta_res,theta_sat)*100, 
           col=c(rgb(1,0,0,.5),rgb(1,0,0,.75),grey.colors(n = 1, .8, .8, .5),grey.colors(n = 1, .8, .8, .5)), lty="dashed")
    
    # text model soil parameter
    text(x=max(psi), y=c(theta_fc,theta_pwp,theta_res,theta_sat)*100, labels = c("fc","pwp","res","sat"), 
         col=c(rgb(1,0,0,.75),rgb(1,0,0,.75),"grey30","grey30"))
    
    # text theortical field capacity and permanent wilting point
    text(x = c(10^2.15,10^4.2), y=61, labels = c("FC","PWP"), col="grey30")
    text(x = c(10^1.8,10^2.5,10^4.2), y=-1, labels = c("1.8","2.5","4.2"), col="grey30")
  
    legend(x=150000, y=60, legend = "estimation", col="black", lwd=3, bty="n")  
  
    if(!is.null(observed))
    {
      legend(x=260000, y=58, legend = "observed", col = grey.colors(n = 1, start = .6, end = .6, alpha = .3), bty = "n", pch = 1)
    }
  
    par(op)
  
  if (pdf) dev.off()
  
  return(NULL)
}
