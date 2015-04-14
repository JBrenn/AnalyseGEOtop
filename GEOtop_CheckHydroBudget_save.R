# check hydrological budget GEOtop 3d simulation
# P = Q + E + dS
# P - Precipitation, Q - Runoff, E - Evapotranspiration, dS - Change in Storage
# 
# # test
#  wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Discharge/WG1_005/"
# # Q observed
#  library(zoo)
#  library(chron)
#  Q_obs <- read.csv2("H:/Projekte/HiResAlp/06_Workspace/BrJ/02_data/discharge/WG1_Saldur.csv",header=T)
#  datetime <- chron(dates. = substr(Q_obs$Time,1,10),times. = paste(substr(Q_obs$Time,12,17),":00",sep=""),
#                    format = c(dates="d.m.y", times="h:m:s"), out.format = c(dates="d/m/y", times="h:m:s"))
#  datetime <- as.POSIXct(datetime)
#  Q_obs_data <- zoo(x = as.numeric(as.character((Q_obs$Q.m3.s))), order.by = datetime)  
#  Q_obs <- "hour"
#  soil_files <- FALSE
# wpath       working path
# Q_obs       observed discharge available in time step "hour" or "day"; not available "n"
# Q_obs_data  zoo-object, observed discharge; m^3/s

# # # geotopbricks version
# # # run with new / github version
# library(devtools)
# remove.packages("geotopbricks")
# install_github(repo = "ecor/geotopbricks")
 
GEOtop_CheckHydroBudget <- function(wpath, Q_obs, Q_obs_data, soil_files)
{
# load libraries 
#  require(geotopbricks)

#  require(zoo)
#  require(chron)
#  require(hydroGOF)
#  require(raster)


# get simulation domain - area [m^2] from lancover map
  lc_map <- get.geotop.inpts.keyword.value(keyword = "LandCoverMapFile", raster = T, wpath=wpath)
  # spatial resolution of raster
  res <- res(x = lc_map)
  # valid grid cells
  validVAL  <- sum(!is.na(lc_map@data@values))
  # calculate basin area in m^2
  basinArea <- validVAL*res[1]*res[2]

# extract start and end of simulation
  start <- get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,
                                          tz="UTC")
  end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="UTC")
  
# get x- , y-coordinates of output points
  if (file.exists(file.path(wpath,"listpoints.txt")))
  {
    listpoints <- read.csv(file.path(wpath,"listpoints.txt"), header = T)
    xpoints <- listpoints$xcoord; ypoints <- listpoints$ycoord
  } else {
    xpoints <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=T)
    ypoints <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=T)
  }

# get levels, number of output points
  level <- 1:length(xpoints)
  
  # read basin file 
  # basin time scale is daily
  basin_name <- get.geotop.inpts.keyword.value("BasinOutputFile", wpath=wpath)
  basin_data <- read.csv(file = paste(wpath, basin_name, ".txt", sep=""), header=T)

  date <- as.Date(x = basin_data$Date12.DDMMYYYYhhmm., format = "%d/%m/%Y %H:%M")
 
  basin_xts  <- zoo(x = basin_data[,-c(1:5)], order.by = date)

#1 Areal Precipitation (P)
  
  # extract from basin file
  P_rain <- basin_xts$Prain_above_canopy.mm.
  P_snow <- basin_xts$Prain_above_canopy.mm..1
  
  P <- P_rain + P_snow
  
  # Intercept Storage = (P_above-P_below) - Transpiration
  # not true... 
  S_Intercept <- (basin_xts$Prain_above_canopy.mm.-basin_xts$Prain_below_canopy.mm.) + 
    (basin_xts$Prain_above_canopy.mm..1-basin_xts$Psnow_below_canopy.mm.) - basin_xts$Transpiration_canopy.mm.
  
  meteo <- merge(basin_xts$Tair.C., P, P_snow, S_Intercept)
  
  YsumsP <- aggregate(P, by = format(time(P),"%Y"), FUN = sum, na.rm=T)
  YsumsS <- aggregate(P_snow, by = format(time(P_snow),"%Y"), FUN = sum, na.rm=T)
  
#2 Discharge (Q)
  
  # read discharge data, aggregate daily
  discharge_name <- get.geotop.inpts.keyword.value("DischargeFile", wpath=wpath)
  discharge_data <- read.csv(file = paste(wpath,discharge_name,".txt",sep=""), header=T)
  datetime <- chron(dates. = substr(discharge_data$DATE.day.month.year.hour.min.,1,10),
                    times. = paste(substr(discharge_data$DATE.day.month.year.hour.min.,12,17),":00",sep=""),
                    format = c(dates="d/m/y", times="h:m:s"), out.format = c(dates="d/m/y", times="h:m:s"))
  datetime <- as.POSIXct(datetime)

  discharge_hourly <- zoo(x = discharge_data[,-c(1:4)], order.by = datetime)

  discharge_daily  <- aggregate(x = discharge_hourly,
                                by = as.Date(time(discharge_hourly)), FUN = mean)

  v_channel <- zoo(x = discharge_data$Vchannel.m3., order.by = datetime)
  v_channel_mm <- v_channel / basinArea * 1000
  v_channel_mm_day <- aggregate(v_channel_mm, by = as.Date(datetime), FUN = mean)

  Q_out <- discharge_data$Qoutlandsup.m3.s. + discharge_data$Qoutlandsub.m3.s. + discharge_data$Qoutbottom.m3.s.
  Q_out <- zoo(x = Q_out, order.by = datetime)
  Q_out_day <- aggregate(Q_out, by = as.Date(datetime), FUN = mean)
  Q_out_mm_day <- Q_out_day * (60*60*24) / basinArea * 1000
  

  # read observed discharge
  if (Q_obs=="hour")
  {
    discharge_hourly <- merge(Q_obs_data, discharge_hourly)      
    discharge_daily  <- aggregate(x = discharge_hourly,
                                  by = as.Date(time(discharge_hourly)), FUN = mean)
  } else if (Q_obs=="day")
  {
    discharge_daily <- merge(Q_obs_data, discharge_daily)  
  } else {
    print("no observed discharge data available")
  }

  discharge_month  <- aggregate(x = discharge_hourly, 
                                by = as.yearmon(time(discharge_hourly)), FUN=mean)

# total discharge at outlet in m^3/s

  if (Q_obs=="hour" | Q_obs=="day") {
    Q_tot <- discharge_daily[,c(1,2)]
    # total basin discharge in mm
    # m^3/s -> m^3/d -> m -> mm
    Q_tot_mm <- Q_tot * (60*60*24) / basinArea * 1000
    Q_sim_mm <- Q_tot_mm$Qtot.m3.s.
    Q_obs_mm <- Q_tot_mm$Q_obs_data
  } else {
    Q_tot <- discharge_daily$Qtot.m3.s.
    # total basin discharge in mm
    # m^3/s -> m^3/d -> m -> mm
    Q_tot_mm <- Q_tot * (60*60*24) / basinArea * 1000
    Q_sim_mm <- Q_tot_mm
  }

#   rat_Q_P  <- Q_tot_mm/P 
#   rat_Q_P[is.infinite(rat_Q_P)] <- NA
#   rat_Q_P[rat_Q_P>100] <- 100

#3 Evapotranspiration E
    Evap_surface <- basin_xts$Evap_surface.mm.
    Trans_canopy <- basin_xts$Transpiration_canopy.mm.

    ET <- Evap_surface + Trans_canopy

#4 Storage (not cumulated!) S

  Intercept_dS <- zoo(c(0,diff(S_Intercept)), date)
  totalIntercept_S <- S_Intercept

# soil water storage
  if (soil_files) {
    soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE)
    soil_thickness <- soil_input[,1]
  # soil_saturation from soil input 
  } else {
    soil_saturation <- get.geotop.inpts.keyword.value(keyword="ThetaSat", wpath=wpath, numeric=T)
    soil_thickness <- get.geotop.inpts.keyword.value("SoilLayerThicknesses", numeric = T, wpath=wpath)
  }

  nlayers <- length(soil_thickness)

#   # output depth in mm
#   soil_head <- diff(c(0,cumsum(soil_thickness)))/2 + c(0,cumsum(soil_thickness))[-length(soil_thickness)-1]
# 
#   soil_header <- c()
#   for (i in 1:length(soil_head))
#   {
#     if (ceiling(soil_head[i])==soil_head[i]) {
#       soil_header[i] <- paste("X", soil_head[i], ".000000", sep="")
#     } else {
#       soil_header[i] <- paste("X", soil_head[i], "00000", sep="")  }     
#   }

# soil liquid water storage
  name_soilliq <- get.geotop.inpts.keyword.value(keyword = "SoilLiqContentTensorFile", wpath=wpath)
  
  if (!is.null(name_soilliq))
  {
    pointerMAPS_soilliq <- pointer.to.maps.xyz.time(wpath, map.prefix = name_soilliq, 
                                                    suffix = "L%04dN%04d.asc", 
                                                    zoo.index = NULL, ntime=length(date), 
                                                    nlayers=nlayers)
    
    SMCliq_mm_0 <- sapply(X = pointerMAPS_soilliq, FUN = function(x) {
      map1 <- read.asciigrid(x[1])
      mean_total_storage <- mean(map1@data[,1], na.rm=T)
    }) * soil_thickness

    diffSMCliq <- sapply( X = pointerMAPS_soilliq, FUN = function(x) {
      map1 <- read.asciigrid(x[1])
      out  <- c()
      for (i in 2:length(x)) {
        map2 <- read.asciigrid(x[i])
        diff <- map2@data[,1] - map1@data[,1]
        # mean basin change in soil liquid water content
        out[i] <- mean(diff, na.rm=T)
        map1 <- map2
      }
      out[1] <- 0
      return(out)
    } )
#    colnames(diffSMCliq) <- paste("Layer", 1:nlayers, sep="")
    diffSMCliq_mm <- t(t(diffSMCliq)*soil_thickness)
    
    totalSMCliq_mm <- diffSMCliq_mm
    totalSMCliq_mm[1,] <- SMCliq_mm_0
    totalSMCliq_mm <- apply(totalSMCliq_mm,2,cumsum)
    
    diffSMCliq_mm <- zoo(diffSMCliq_mm, date)
    totalSMCliq_mm <- zoo(totalSMCliq_mm, date)
    
    diffSMCliq_mm_column <- zoo(rowSums(diffSMCliq_mm),date)
    totalSMCliq_mm_column <- zoo(rowSums(totalSMCliq_mm),date)
  }

  soil_column <- cumsum(c(0, soil_thickness))
  soil_spez   <- c()

  for (i in 1:length(soil_thickness))
  {
    soil_spez[i] <- paste(soil_column[i],"-",soil_column[i+1],"mm",sep=" ")
  }

  layer_saturation <- soil_thickness * soil_saturation

# soil ice storage
  name_soilice <- get.geotop.inpts.keyword.value(keyword = "SoilIceContentTensorFile", wpath=wpath)

  if (!is.null(name_soilice))
  {
    pointerMAPS_soilice <- pointer.to.maps.xyz.time(wpath, map.prefix = name_soilice, 
                                                   suffix = "L%04dN%04d.asc", 
                                                   zoo.index = NULL, ntime=length(date), 
                                                   nlayers=nlayers)
    
    SMCice_mm_0 <- sapply(X = pointerMAPS_soilice, FUN = function(x) {
      map1 <- read.asciigrid(x[1])
      mean_total_storage <- mean(map1@data[,1], na.rm=T)
    }) * soil_thickness
    
    diffSMCice <- sapply( X = pointerMAPS_soilice, FUN = function(x) {
      map1 <- read.asciigrid(x[1])
      out  <- c()
      for (i in 2:length(x)) {
        map2 <- read.asciigrid(x[i])
        diff <- map2@data[,1] - map1@data[,1]
        # mean basin change in soil ice content
        out[i] <- mean(diff, na.rm=T)
        map1 <- map2
      }
      out[1] <- 0
      return(out)
    } )
#    colnames(diffSMCice) <- paste("Layer", 1:nlayers, sep="")
    diffSMCice_mm <- t(t(diffSMCice)*soil_thickness)
    
    totalSMCice_mm <- diffSMCice_mm
    totalSMCice_mm[1,] <- SMCice_mm_0
    totalSMCice_mm <- apply(totalSMCice_mm,2,cumsum)
    
    diffSMCice_mm <- zoo(diffSMCice_mm, date)
    totalSMCice_mm <- zoo(totalSMCice_mm, date)
    
    diffSMCice_mm_column <- zoo(rowSums(diffSMCice_mm),date)
    totalSMCice_mm_column <- zoo(rowSums(totalSMCice_mm),date)
  }

  # total SMC
  if (!is.null(name_soilice) & !is.null(name_soilliq))
  {
    totalSMC_mm <- totalSMCice_mm + totalSMCliq_mm
    totalSMC_mm_column <- totalSMCice_mm_column + totalSMCliq_mm_column
    
    diffSMC_mm <- diffSMCice_mm + diffSMCliq_mm
    diffSMC_mm_column <- diffSMCice_mm_column + diffSMCliq_mm_column
    
    absSMC_mm <- diffSMC_mm 
    
    Intercept_dS <- merge(Intercept_dS, SMC_dS=diffSMC_mm_column)
    totalIntercept_S <- merge(totalIntercept_S, SMC_S = totalSMC_mm_column)
  }

# snow and ice storage
  name_swe     <- get.geotop.inpts.keyword.value(keyword = "SWEMapFile", wpath=wpath)
  
  if (!is.null(name_swe))
  {
    
    glacier <- get.geotop.inpts.keyword.value(keyword = "InitGlacierDepthMapFile", wpath=wpath, raster=T)
    if (!is.null(glacier)) {
      glacier_cells <- which(glacier@data$values!=0)
    } else if (!is.null(get.geotop.inpts.keyword.value(keyword = "InitSWEMapFile", wpath=wpath))) {
      glacier <- get.geotop.inpts.keyword.value(keyword = "InitSWEMapFile", wpath=wpath, raster=T)
      glacier_cells <- which(glacier@data@values!=0)
    } else {
      print("No glacier input provided. Not possible to distinguish between snow and ice SWE")
    }
    
    pointerMAPS_swe <- pointer.to.maps.xyz.time(wpath, map.prefix = name_swe, 
                                                suffix = "N%04d.asc", 
                                                zoo.index = NULL, ntime=1,
                                                nlayers=length(date))
    
    pointerMAPS_swe <- unlist(pointerMAPS_swe)
    
    map1 <- read.asciigrid(pointerMAPS_swe[1])
    SWE_mm_0 <- mean(map1@data[,1], na.rm=T)
    
    if (!is.null(glacier))
    {
      SWE_mm_0_glacier <- mean(map1@data[glacier_cells,1], na.rm=T)
      SWE_mm_0_snow    <- mean(map1@data[-glacier_cells,1], na.rm=T)
      
      diffSWE_mm_glacier  <- c()
      diffSWE_mm_snow  <- c()
      
      diffSWE_mm_glacier[1] <- 0
      diffSWE_mm_snow[1] <- 0
    }
    
    diffSWE_mm  <- c()
    diffSWE_mm[1] <- 0
    
    for (i in 2:length(pointerMAPS_swe)) 
    {
      map2 <- read.asciigrid(pointerMAPS_swe[i])
      diff <- map2@data[,1] - map1@data[,1]
      # mean basin change in snow water equivalent (including glaciers --> old snow)
      diffSWE_mm[i] <- mean(diff, na.rm=T)
      
      if (!is.null(glacier)) 
      {
      # sum of glacier / snow melt in the whole basin and normalized/meaned over basin area
        diffSWE_mm_glacier[i] <- sum(diff[glacier_cells], na.rm=T) / validVAL
        diffSWE_mm_snow[i]    <- sum(diff[-glacier_cells], na.rm=T) / validVAL
      }
      
      map1 <- map2
    }
    
    #total SWE storage (glacier + snow)
    totalSWE_mm <- diffSWE_mm
    totalSWE_mm[1] <- SWE_mm_0
    totalSWE_mm <- cumsum(totalSWE_mm)
    
    diffSWE_mm <- zoo(diffSWE_mm, date)
    totalSWE_mm <- zoo(totalSWE_mm, date)

    Intercept_dS <- merge(Intercept_dS, SWE_dS=diffSWE_mm)
    totalIntercept_S <- merge(totalIntercept_S, SWE_S = totalSWE_mm)
    
    if (!is.null(glacier))
    {
      # glacier
      totalSWE_mm_glacier <- diffSWE_mm_glacier
      totalSWE_mm_glacier[1] <- SWE_mm_0_glacier
      totalSWE_mm_glacier <- cumsum(totalSWE_mm_glacier)
      
      diffSWE_mm_glacier <- zoo(diffSWE_mm_glacier, date)
      totalSWE_mm_glacier <- zoo(totalSWE_mm_glacier, date)
      
      # snow
      totalSWE_mm_snow <- diffSWE_mm_snow
      totalSWE_mm_snow[1] <- SWE_mm_0_snow
      totalSWE_mm_snow <- cumsum(totalSWE_mm_snow)
      
      diffSWE_mm_snow <- zoo(diffSWE_mm_snow, date)
      totalSWE_mm_snow <- zoo(totalSWE_mm_snow, date)
    }
  }

# Land Surface Storage
  name_hsup <- get.geotop.inpts.keyword.value(keyword = "LandSurfaceWaterDepthMapFile", wpath=wpath)
  
  if (!is.null(name_hsup))
  {
    pointerMAPS_hsup <- pointer.to.maps.xyz.time(wpath, map.prefix = name_hsup, 
                                                 suffix = "N%04d.asc", 
                                                 zoo.index = NULL, ntime=1,
                                                 nlayers=length(date))
    
    pointerMAPS_hsup <- unlist(pointerMAPS_hsup)
    
    map1 <- read.asciigrid(pointerMAPS_hsup[1])
    HSUP_mm_0 <- mean(map2@data[,1], na.rm=T)
    diffHSUP_mm  <- c()
    diffHSUP_mm[1] <- 0
    for (i in 2:length(pointerMAPS_hsup)) 
    {
      map2 <- read.asciigrid(pointerMAPS_hsup[i])
      diff <- map2@data[,1] - map1@data[,1]
      # mean basin change in snow water equivalent (including glaciers --> old snow)
      diffHSUP_mm[i] <- mean(diff, na.rm=T)
      map1 <- map2
    }
    diffHSUP_mm <- zoo(diffHSUP_mm, date)
    totalHSUP_mm <- diffHSUP_mm
    totalHSUP_mm[1] <- HSUP_mm_0
    totalHSUP_mm <- cumsum(totalHSUP_mm)
    
    Intercept_dS <- merge(Intercept_dS, HSUP_dS=diffHSUP_mm)
    totalIntercept_S <- merge(totalIntercept_S, HSUP_S = totalHSUP_mm)
  }

# 

  Intercept_dS <- merge(Intercept_dS, Hchannel_dS=diff(v_channel_mm_day))
  
  totalIntercept_S <- merge(totalIntercept_S, Hchannel_S = v_channel_mm_day)
  totalIntercept_S <- totalIntercept_S[!is.na(totalIntercept_S),]
                                       
  dS <- Intercept_dS[,-1]
  totalStorage <- totalIntercept_S[,-1]

  delta_S <- zoo(rowSums(dS),date)
  S <- zoo(rowSums(totalStorage),date)

#------------------------
# plotting

  text <- c("YEAR: Precip (mm) , Snow (%)", paste(time(YsumsS), ": ", round(YsumsP), ",", round(YsumsS/YsumsP*100), sep=""))
  
  if (Q_obs=="hour" | Q_obs=="day") {
    meteo_Q <- merge(meteo[,c(1,2,3)], discharge_daily[,c(1,2)])
  } else {
    meteo_Q <- merge(meteo[,c(1,2)], discharge_daily[,c(1)])
  }


  pdf(paste(wpath,"Ppartitioning.pdf",sep=""), width = 21)

#1 meteo data | partitioning rain / snow
  plot.zoo(meteo_Q, ylab=c("Tair [°C]", "P [mm]", "Q [m^3/s]"), screens = c(1,2,2,3,3), main="Basin Mean Meteo , Discharge", 
           type=c("l","h","h","l", "l"), col=c(grey(.2,.5), rgb(0,0,1,.75), rgb(1,1,0,.5), "black", rgb(1,0,0,.75)),
           xlab="", sub="x", lwd=c(1.2,1.2,1.2,1.5,2))
  text(x=0.25, y=0.69, labels = text[1], cex = 0.8)
  for (t in 2:length(text)) text(x=0.15*(t-1), y=0.65, labels = text[t], cex = 0.8) 
  legend(x = .6, y = .63, legend = c("RAIN", "SNOW"), col=c(rgb(0,0,1,.75), rgb(1,1,0,.5)), lwd=3, bty = "n", horiz = T, )
  
  meteo_month <- aggregate(x = meteo, by = as.yearmon(time(meteo)))
  
  Psnow_month <- meteo_month$P_snow
  Prain_month <- meteo_month$P - meteo_month$P_snow

  Psnow_perc  <- Psnow_month / meteo_month$P *100
  Prain_perc  <- Prain_month / meteo_month$P *100
  
  op <- par(mfrow=c(2,1))  
  
  barplot(merge(Prain_month,Psnow_month), col=c(rgb(0,0,1,.75), rgb(1,1,0,.5)), beside = F, ylab="mm/month", 
          main="Monthly Partitioning Precipitation")
  legend("topleft", legend = c( "Snow", "Rainfall"), col=c(rgb(1,1,0,.5), rgb(0,0,1,.75)), 
         pch=15, horiz=F, bty="n")
  barplot(merge(Prain_perc,Psnow_perc), col=c(rgb(0,0,1,.75), rgb(1,1,0,.5)), beside = F, ylab="%")
  legend("topleft", legend = c( "Snow", "Rainfall"), col=c(rgb(1,1,0,.5), rgb(0,0,1,.75)), 
         pch=15, horiz=F, bty="n")
  
  par(op)

  dev.off()

#2 discharge observed vs. simulated
  if (Q_obs=="day" | Q_obs=="hour")
  {
    pdf(paste(wpath,"QsimVSQobs.pdf",sep=""), width = 21)
    
    if (Q_obs=="hour")
    {
      plot.zoo(discharge_hourly[,c(1,2)], screens = c(1,1), lwd=c(1.5,2), col=c("black", rgb(1,0,0,.75)),
               ylab="Q [m^3/s]", main="time resolution , hour")
      legend("topright", legend = c("observerd", "simulated"), lwd=3, bty="n", col=c("black", rgb(1,0,0,.75)))
      
      ggof(sim = discharge_hourly[,2], obs = discharge_hourly[,1],   
           gofs = c("RMSE", "NSE", "R2"),
           legend = c("simulated", "observed"))
    }
    
    plot.zoo(discharge_daily[,c(1,2)], screens = c(1,1), lwd=c(1.5,2), col=c("black", rgb(1,0,0,.75)),
             ylab="Q [m^3/s]", main="time resolution , day")
    legend("topright", legend = c("observerd", "simulated"), lwd=3, bty="n", col=c("black", rgb(1,0,0,.75)))
    plot.zoo(discharge_month[,c(1,2)], screens = c(1,1), lwd=c(1.5,2), col=c("black", rgb(1,0,0,.75)),
             ylab="Q [m^3/s]", main="time resolution , month")
    legend("topright", legend = c("observerd", "simulated"), lwd=3, bty="n", col=c("black", rgb(1,0,0,.75)))
    
    ggof(sim = discharge_daily[,2], obs = discharge_daily[,1], ftype = "dm", FUN=mean, 
         gofs = c("RMSE", "NSE", "R2"),
         legend = c("simulated", "observed"))
    
#     # analysis and visualisation of residuals (simulated - observed)
#     r <- discharge_hourly[,2] - discharge_hourly[,1]
#     r <- r[!is.na(coredata(r)),]
#     names(r) <- "residuals Q"
#     hydroplot(r, FUN=mean)
    
    dev.off()
    
  }

#3.1 basin water badget / mass balance

# visualize dS/dt = P - ET - Q
  delta_S1 <- P - Q_sim_mm - ET - Q_out_mm_day

  delta <- merge(dBudget=delta_S1, dStorage=delta_S)

  pdf(paste(wpath,"WaterBudget.pdf",sep=""), width = 21)

  plot.zoo(delta, screens = c(1,1), col=c("black", rgb(1,0,0,.5)), lwd=c(2,2), type = c("l","l"), 
           ylab="mm", main="dS/dt = P - ET - Q")
  abline(h=seq(from = -100,100,by = 10), col=grey.colors(n = 1,.5,.5,alpha = .3), lwd=.5)
  abline(h=0, col=rgb(0,1,0,.5), lwd=2, lty="dashed")
  legend("topleft", legend = c("P - ET - Q", "dS/dt"), col=c("black", rgb(1,0,0,.5)), lwd=2)

# visualize cumulated main components and total storage
  if (Q_obs=="hour"  | Q_obs=="day") {
    mainComponents <- merge(P=P, ET=-ET, Q=-Q_sim_mm, Q_obs=-Q_obs_mm)
  } else {
    mainComponents <- merge(P=P, ET=-ET, Q=-Q_sim_mm)
  }
  mainComponents <- mainComponents[-which(is.na(mainComponents[,1])),]
  cumsumMainComp <- cumsum(mainComponents)

  cumsumMainComp_Budget <- cumsumMainComp$P + cumsumMainComp$Q + cumsumMainComp$ET
  
  Storage <- S-coredata(S[1,1])

  forplot <- merge(cumsumMainComp, Budget=cumsumMainComp_Budget, Storage=Storage)
  
  col <- rainbow(n = dim(forplot)[2])

  plot.zoo(x = forplot, screens = rep(1,dim(forplot)[2]), col=col, 
           main="dS/dt = P - ET - Q , integral", ylab="mm")
  legend("topleft", legend = names(forplot), col=col, lwd=3, ncol=3, bty="n")
  abline(h=seq(-10000,10000,1000), col=grey.colors(n = 1, start = .5, end = .5, alpha = .25))
  abline(h=0,  col=grey.colors(n = 1, start = .25, end = .25, alpha = .25))

# 3.2 budget/storage residuals

  res_total <- forplot$Storage - forplot$Budget
  res_dt    <- delta$dStorage - delta$dBudget
  residuals <- as.xts(merge(res_dt, res_total))
  plot.zoo(residuals, panel = function(x, y, col, lty, ...) {
    lines(x, y, ...)
    abline(h=0, lty="dashed", col=rgb(1,0,0,.5))
  }, ylab=c("dt [mm]", "integral [mm]"), main="Residuals , Storage - Budget")

# 3.3 storage terms

# dynamics of total storage
  plot.zoo(x = totalStorage, main="Main Storage Components , mm", lwd=1.5)
  plot.zoo(x = dS, main="dS/dt , Change in Storage Conponents , mm/day", lwd=1.5)

# soil water storage in soil layers
  plot.zoo(totalSMC_mm, ylab=soil_spez, main="Soil Water Storage per Layer , mm", panel = function(x,y,...)
    {
      panel.number <- parent.frame()$panel.number
      lines(x, y, ylim=c(0, layer_saturation[panel.number]+.1*layer_saturation[panel.number]))
      abline(h=layer_saturation[panel.number], col=rgb(0,0,1,.5), lwd=2, lty="dashed")
    })
  legend("bottomleft", legend = "saturated", lty="dashed", lwd=3, col=rgb(0,0,1,.5))

# 3.5 Snow and Ice melt dynamics, Snow and Ice storage partitioning
  SWE_comp <- merge(diffSWE_mm, diffSWE_mm_glacier, diffSWE_mm_snow, totalSWE_mm, totalSWE_mm_glacier, totalSWE_mm_snow)

# dynamics
  plot.zoo(SWE_comp[,c(1:3)], screens=c(1,1,1), col=c("grey30", rgb(0,1,0,.75), rgb(1,0,0,.5)), lwd=c(3,2,2), ylab="mm")
  abline(h = 0, col=grey.colors(n = 1, start = .5, end = .5, alpha = .5), lwd=2, lty="dashed")
  legend("topleft", legend = c("mean change SWE storage", "mean change ice/glacier storage", "mean change snow storage"), 
           lwd=3, col=c("grey", rgb(0,1,0,.75), rgb(1,0,0,.5)))

# melt partitioning storage
  SWE_comp_month <- aggregate(x = SWE_comp, by = as.yearmon(time(SWE_comp)), FUN = sum)

  SWE_melt <- zoo(apply(X = SWE_comp[,1:3], MARGIN = 2, FUN = function(x) ifelse(SWE_comp[,1]>=0, NA, x)), time(SWE_comp))
  SWE_stor <- zoo(apply(X = SWE_comp[,1:3], MARGIN = 2, FUN = function(x) ifelse(SWE_comp[,1]<0, NA, x)), time(SWE_comp))

  SWE_melt_month <- aggregate(x = SWE_melt, by = as.yearmon(time(SWE_comp)), FUN = sum, na.rm=T)
  SWE_stor_month <- aggregate(x = SWE_stor, by = as.yearmon(time(SWE_comp)), FUN = sum, na.rm=T)

  SWE_melt_glac_perc <- SWE_melt_month[,2] / SWE_melt_month[,1] *100
  SWE_melt_snow_perc <- SWE_melt_month[,3] / SWE_melt_month[,1] *100

  barplot(SWE_comp_month[,c(2,3)], col=c(rgb(0,1,0,.5), rgb(1,0,0,.5)), beside = T, ylab="mm/month",
          main="Monthly Partitioning Melt / Buildup , Contribution snow and ice")
  legend("topleft", legend = c( "Snow", "Ice"), col=c(rgb(1,0,0,.5), rgb(0,1,0,.5)), 
         pch=15, horiz=F)
  
  barplot(merge(SWE_melt_glac_perc, SWE_melt_snow_perc), col=c(rgb(0,1,0,.5),rgb(1,0,0,.5)), 
          pch=15, bty="n", horiz=F, ylab="%", beside=F, main = "Contribution to melt water production")
  legend("topleft", legend = c( "Snow", "Ice"), col=c(rgb(1,0,0,.5),rgb(0,1,0,.5)), 
         pch=15, horiz=F)
    
# 3.4 ET partitioning
  ET_part <- merge(ET=ET, Evaporation=Evap_surface, Transpiration=Trans_canopy)
  ET_part_cumsum <- cumsum(ET_part)

  ET_part_month <- aggregate(x = ET_part, by = as.yearmon(time(ET_part)), FUN = sum)
  ET_part_T_perc <- ET_part_month$Transpiration / ET_part_month$ET *100
  ET_part_E_perc <- ET_part_month$Evaporation / ET_part_month$ET *100

#   ET_part_perc <- merge( E = ET_part_cumsum$Evaporation / ET_part_cumsum$ET *100, 
#                          T = ET_part_cumsum$Transpiration / ET_part_cumsum$ET *100)

  plot.zoo(ET_part_cumsum, screens=c(1,1,1), main="Partitioning Evapotranspiration",
           col=c("grey10", rgb(1,0,0,.75), rgb(0,1,0,.75)), ylab="mm")
  legend("topleft", legend = c("Evapotranspiration", "Evaporation", "Transpration"), 
         col=c("grey10", rgb(1,0,0,.75), rgb(0,1,0,.75)), lwd=3, bty="n")

  op <- par(mfrow=c(2,1))  

  barplot(ET_part_month[,c(2,3)], col=c(rgb(1,0,0,.5), rgb(0,1,0,.5)), beside = F, ylab="mm/month", 
          main="Monthly Partitioning Evapotranspiration")
  legend("topleft", legend = c( "Transpiration", "Evaporation"), col=c(rgb(0,1,0,.5), rgb(1,0,0,.5)), 
         pch=15, horiz=F)
  barplot(merge(ET_part_E_perc, ET_part_T_perc), col=c(rgb(1,0,0,.5), rgb(0,1,0,.5)), 
          pch=15, bty="n", horiz=F, ylab="Percent")
  legend("topleft", legend = c( "Transpiration", "Evaporation"), col=c(rgb(0,1,0,.5), rgb(1,0,0,.5)), 
         pch=15, horiz=F)

  par(op)

dev.off()
}