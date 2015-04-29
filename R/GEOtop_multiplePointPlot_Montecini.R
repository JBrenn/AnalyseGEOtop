# Analysing GEOtop model output  
# run Analysis of GEOtop simulation for multiple point output
# plus validation on ET, SMC, Soil Temperature for specific stations (Mazia)
# specific skript for simulations on Montecini stations (B, I & P)

# ARGUMENTS
  # path        path to simulation
  # model_run   simulation folder
  # station     output station names (Mazia)
  # val_aggr    time aggregation 
  # read_data   should GEOtop output data be read? ELSE workspace points.RData
  # calibration should calibration function for SMC data be applied
  # use_swc_liq only use liquid SWC for validation
  # lc_classes  names of landcover classes in simulation
  # linux       TRUE: working on linux laptop, FALSE: working on windows laptop; for path retrieval
  # soil_files

GEOtop_multiplePointPlot_Montecini <- function(path, model_run, stations, 
                                              val_aggr, read_data, calibrate, use_swc_liq, soil_files,
                                              lc_classes, linux)
{
  # load libraries
#   require(zoo)
#   require(geotopbricks)
#   require(hydroGOF)
#   require(hydroTSM)
#   require(ggplot2)
#   require(soilwater)
  
  #  source functions 1
  # source("H:/Projekte/HydroAlp/06_Workspace/BrJ/03_R/GEOtopAnalyse/FUN_GEOtopReadPoint.R")
  
  # created path to model output  
  wpath <- paste(path, model_run, sep="")  
  
  #---------------------------------------------------------------------------------------------
  
#   #---- GEOtop .inpts  
  fc_classes <- get.geotop.inpts.keyword.value(wpath=wpath, keyword = "CanopyFraction", numeric = T)
#   
#   # BUG only works with less than 10 output points
# try
  #point_character <- GEOtop_ReadPointVar(wpath = wpath, coordinates_value_df = TRUE, landcover_classes = lc_classes)  

# get landcover class (either listpoint file or from point info)
  lc <- read.table(paste(wpath,"/listpoints.txt", sep=""), header=T, sep=",")$landcover  

  fc <- c()
  for (i in 1:length(lc)) fc[i] <- fc_classes[lc[i]]
  names(fc) <- stations
  
  if (read_data) {
    # reading data  
    print("start reading point output data for the stations")
    print(stations)
    
    #---- METEOROLOGICAL VARIABLES    
    print("-----METEOROLOGICAL VARIABLEs-----")
    # precipitation
    print("... precipitation = snow + rain (over canopy)") 
    snow_o_canopy <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                                     varOFint=c("Psnow_over_canopy.mm."))
    rain_o_canopy <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                     varOFint=c("Prain_over_canopy.mm."))
    precipitation <- zoo(x=coredata(snow_o_canopy) + coredata(rain_o_canopy), order.by=time(snow_o_canopy))
    
    #----
    # wind speed
    print("... wind speed")
    wind_speed <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                                  varOFint=c("Wind_speed.m.s."))
    #----
    # relative humidity
    print("... relative humidity")
    relative_humidity <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                                         varOFint=c("Relative_Humidity..."))
    #----
    # atmospheric pressure
    print("... atmospheric pressure")
    atm_pressure <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                                    varOFint=c("Pressure.mbar."))
    #----  
    # air temperature
    print("... air temperature")
    airT <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                            varOFint=c("Tair.C."))
    
    #---- 
    # energy fluxes
    print("-----ENERGY FLUXES-----")
    
    print("... radiation components: short & long wave")
    #short wave
    SWin <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                            varOFint=c("SWin.W.m2."))
    SWup <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                            varOFint=c("SWup.W.m2."))
    SWv <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                           varOFint=c("SWv.W.m2."))
    SWbeam <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                              varOFint=c("SWbeam.W.m2."))
    SWdiff <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                              varOFint=c("SWdiff.W.m2."))
    SWnet <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                             varOFint=c("SWnet.W.m2."))
    
    #long wave
    LWin <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                            varOFint=c("LWin.W.m2."))
    LWup <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                            varOFint=c("LWup.W.m2."))
    LWv <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                           varOFint=c("LWv.W.m2."))
    LWnet <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                             varOFint=c("LWnet.W.m2."))
    
    #soil heat fluxes
    print("... surface energy balance")
    SurfEB <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                              varOFint=c("Surface_Energy_balance.W.m2."))
    SoilHeat <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                varOFint=c("Soil_heat_flux.W.m2."))
    
    # latent heat LE
    print("... latent heat components")
    LEv <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                           varOFint=c("LEv.W.m2."))
    LEg_veg   <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                 varOFint=c("LEg_veg.W.m2."))
    LEg_unveg <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                 varOFint=c("LEg_unveg.W.m2."))
    
    LE_soil <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                               varOFint=c("LE.W.m2."))
    
    # sensible heat H
    print("... sensible heat")
    Hv <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                          varOFint=c("Hv.W.m2."))
    Hg_veg   <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                varOFint=c("Hg_veg.W.m2."))
    Hg_unveg <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                varOFint=c("Hg_unveg.W.m2."))
    
    H_soil  <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                               varOFint=c("H.W.m2."))  
    
    #---- WATER FLUXES
    # evapotranspiration
    print("-----WATER FLUXES-----")
    print("... evapotranspiration = evapo_surface + transpiration_canopy")
    evapo_surfac <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile", 
                                    varOFint=c("Evap_surface.mm."))
    trans_canopy <- GEOtop_ReadPointVar(wpath=wpath, keyword="PointOutputFile",
                                    varOFint=c("Trasp_canopy.mm."))  
    evapotranspiration <- evapo_surfac + trans_canopy
    
    #---- WATER STORAGE 
    print("-----WATER STORAGE-----")
    
    # soil water storage
    # theta liquid/ice content
    
    if (soil_files) {
      soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE)
      soil_thickness <- soil_input[,1]
    } else {
      Dz <- soil_thickness <- get.geotop.inpts.keyword.value("SoilLayerThicknesses", numeric = T, wpath=wpath)
      Kh <-     get.geotop.inpts.keyword.value("NormalHydrConductivity", numeric = T, wpath=wpath)
      Kv <-     get.geotop.inpts.keyword.value("LateralHydrConductivity", numeric = T, wpath=wpath)
      vwc_r <-  get.geotop.inpts.keyword.value("ThetaRes", numeric = T, wpath=wpath)
      vwc_w <-  get.geotop.inpts.keyword.value("WiltingPoint", numeric = T, wpath=wpath)
      vwc_fc <- get.geotop.inpts.keyword.value("FieldCapacity", numeric = T, wpath=wpath)
      vwc_s <-  get.geotop.inpts.keyword.value("ThetaSat", numeric = T, wpath=wpath)
      alpha <-  get.geotop.inpts.keyword.value("AlphaVanGenuchten", numeric = T, wpath=wpath) 
      n <-      get.geotop.inpts.keyword.value("NVanGenuchten", numeric = T, wpath=wpath)
      soil_input <- data.frame(Dz,Kh,Kv,vwc_r,vwc_w,vwc_fc,vwc_s,alpha,n)
    }

    # output depth in mm
    soil_head <- diff(c(0,cumsum(soil_thickness)))/2 + c(0,cumsum(soil_thickness))[-length(soil_thickness)-1]
    
    soil_file <- get.geotop.inpts.keyword.value(keyword="SoilLiqContentProfileFile", wpath=wpath, data.frame=TRUE)
    soil_header <- names(soil_file)[-c(1:6)]
    
#     soil_header <- c()
#     for (i in 1:length(soil_head))
#     {
#       if (ceiling(soil_head[i])==soil_head[i]) {
#         soil_header[i] <- paste("X", soil_head[i], ".000000", sep="")
#       } else {
#         soil_header[i] <- paste("X", soil_head[i], "00000", sep="")  }     
#     } 
    
    print("... soil liquid content for all layers")
    soil_liq <- list()
    for (i in soil_header)
      soil_liq[[i]] <- GEOtop_ReadPointVar(wpath=wpath, keyword="SoilLiqContentProfileFile", 
                                       varOFint=c(i))
    
    #time_soil <- time(soil_liq[[1]])
    #dummy <- matrix(unlist(soil_liq), nrow=length(time_soil))
    
    #coln <- c()
    #for (i in soil_header) coln <- c(coln,paste(i,1:dim(soil_liq[[1]])[[2]],sep="_"))
    #colnames(dummy) <- coln
    
    #soil_liqu <- zoo(dummy, time_soil)
    
    print("... soil ice content for all layers")
    soil_ice <- list()
    for (i in soil_header) 
      soil_ice[[i]] <- GEOtop_ReadPointVar(wpath=wpath, keyword="SoilIceContentProfileFile", 
                                       varOFint=c(i))
     
    #time_soil <- time(soil_ice[[1]])
    #dummy <- matrix(unlist(soil_ice), nrow=length(time_soil))
    
    #coln <- c()
    #for (i in soil_header) coln <- c(coln,paste(i,1:dim(soil_ice[[1]])[[2]],sep="_"))
    #colnames(dummy) <- coln
    
    #soil_ice <- zoo(dummy, time_soil)
    
    soil_moist <- list()
    for (i in soil_header) soil_moist[[i]] <- soil_liq[[i]] + soil_ice[[i]]
    #   
    #----
    # psi liquid
    print("... soil liquid water pressure for all layers")
    soil_liq_pressure <- list()
    for (i in soil_header)
      soil_liq_pressure[[i]] <- GEOtop_ReadPointVar(wpath=wpath, keyword="SoilLiqWaterPressProfileFile", 
                                                    varOFint=c(i))
    
    # psi total
    print("... soil total water pressure for all layers")
    soil_tot_pressure <- list()
    for (i in soil_header)
      soil_tot_pressure[[i]] <- GEOtop_ReadPointVar(wpath=wpath, keyword="SoilTotWaterPressProfileFile", 
                                                    varOFint=c(i))
    
    #-----
    # SOIL TEMPERATURE
    print("... soil temperature for all layers")
    soil_temp <- list()
    for (i in soil_header)
      soil_temp[[i]] <- GEOtop_ReadPointVar(wpath=wpath, keyword="SoilAveragedTempProfileFile", 
                                       varOFint=c(i))
    
    # all data in a list
    data_list <- list(snow_o_canopy=snow_o_canopy, rain_o_canopy=rain_o_canopy, P=precipitation,
                      airT=airT, WV=wind_speed, RH=relative_humidity, 
                      atmP=atm_pressure, SWin=SWin, LWin=LWin, SWnet=SWnet, LWnet=LWnet, LWin=LWin,
                      SWbeam=SWbeam, SWdiff=SWdiff, SWup=SWup, LWup=LWup,
                      LE_soil=LE_soil, H_soil=H_soil, SurfEB = SurfEB, SoilHeat = SoilHeat,
                      Hv=Hv, LEv=LEv, Hg_veg=Hg_veg, LEg_veg=LEg_veg, Hg_unveg=Hg_unveg, LEg_unveg=LEg_unveg,
                      ET=evapotranspiration, evapo_surfac=evapo_surfac, trans_canopy=trans_canopy)
    
    for (i in 1:length(soil_header)) 
    {
      data_list[[paste("soil_ice",soil_head[i],sep="_")]] <- soil_ice[[i]]
      data_list[[paste("soil_liq",soil_head[i],sep="_")]] <- soil_liq[[i]]
      data_list[[paste("SWC",soil_head[i],sep="_")]] <- soil_moist[[i]]
      data_list[[paste("PSI",soil_head[i],sep="_")]] <- soil_liq_pressure[[i]]
      data_list[[paste("PSI_tot",soil_head[i],sep="_")]] <- soil_tot_pressure[[i]]
      data_list[[paste("soil_temp",soil_head[i],sep="_")]] <- soil_temp[[i]]
    }
    
#     #----
#     # soil parametrisation | texture | suction curves
#     if (soil_files) {
#       nr_soiltypes <- get.geotop.inpts.keyword.value(keyword = "SoilLayerTypes", wpath=wpath, numeric = T)
#       soil_para <- get.geotop.inpts.keyword.value(keyword = "SoilParFile", wpath = wpath)
#       
#       soilfiles <- paste(soil_para, formatC(1:nr_soiltypes, width = 4, format = "d", flag = "0"), ".txt", sep="")
#       
#       soil_parameters <- list()
#       for (sfile in soilfiles)
#       {
#         soil_parameters[[sfile]] <- read.csv(file.path(wpath,sfile), header = T)
#       }
#     } else {
#       
#     }

    
    #----
    # merge data for each station
    
    #----
    # station 
    list_station <- list()
    for (i in 1:length(stations))
    {
      list_station[[stations[i]]] <- lapply( X = data_list, FUN = function(x) x[,i] )
    }
    
    # read info for SWC input (measured data)
    # SWCinfo <- read.csv(file = "H:/Projekte/HydroAlp/06_Workspace/BrJ/03_R/GEOtopAnalyse/SWCinfo.txt")
    # SWCinfo <- read.csv2(file = "validation_data/SWCinfo.txt")

    # save workspace
    save(list = c("list_station","soil_head","soil_header","wpath","fc","soil_input"), 
         file = paste(wpath,"/point.RData",sep=""))
  } else {
    print("start reading point output data from formerly saved workspace")
    load(file = paste(wpath,"/point.RData",sep=""))
  }
  
  print("all point data read in")

  # source functions 2
  #source("H:/Projekte/HiResAlp/06_Workspace/BrJ/04_R_data_analyses/data_base/getSWC.R")
  #source("H:/Projekte/HiResAlp/06_Workspace/BrJ/04_R_data_analyses/data_base/getSoilTemp.R")

  data(SWCinfo)
  if (linux) SWCinfo <- SWCinfoLIN else SWCinfo <- SWCinfoWIN

  for (i in stations)
  { 
    
    # latent and sensible heat over canopy
    LE_overcanopy <- fc[i] * (list_station[[i]]$LEg_veg + list_station[[i]]$LEv) + (1-fc[i]) * list_station[[i]]$LEg_unveg
    H_overcanopy  <- fc[i] * (list_station[[i]]$Hg_veg + list_station[[i]]$Hv) + (1-fc[i]) * list_station[[i]]$Hg_unveg
    
    meteo <- cbind(list_station[[i]]$airT, list_station[[i]]$P,  
                   #list_station[[i]]$rain_o_canopy, 
                   list_station[[i]]$snow_o_canopy)
    et <- cbind(list_station[[i]]$ET, list_station[[i]]$trans_canopy,
                list_station[[i]]$evapo_surfac)
    ETcumsum <- cbind(cumsum(list_station[[i]]$ET),cumsum(et[,2:3]))
    radiation <- cbind(list_station[[i]]$SWin, list_station[[i]]$LWin,  
                       list_station[[i]]$SWbeam, list_station[[i]]$SWdiff,
                       list_station[[i]]$SWup, list_station[[i]]$LWup,
                       list_station[[i]]$SWnet, list_station[[i]]$LWnet)
    energy <- cbind(list_station[[i]]$LE_soil, list_station[[i]]$H_soil,
                    list_station[[i]]$SurfEB, 
                    LE_overcanopy, H_overcanopy,
                    list_station[[i]]$SoilHeat)
    
    swc_soilliq_labs <- paste("soil_liq_", soil_head, sep="")
    swc_labs <- paste("SWC_", soil_head, sep="")
    if (use_swc_liq) {
      swc <- cbind(list_station[[i]][[swc_soilliq_labs[1]]], list_station[[i]][[swc_soilliq_labs[2]]])
      for (n in 3:length(swc_soilliq_labs)) {
        swc <- cbind(swc, list_station[[i]][[swc_soilliq_labs[n]]])
      }
      names(swc) <- swc_soilliq_labs
#       swc <- cbind(list_station[[i]]$soil_liq_10, list_station[[i]]$soil_liq_50,  list_station[[i]]$soil_liq_200,
#                        list_station[[i]]$soil_liq_560,list_station[[i]]$soil_liq_1400, list_station[[i]]$soil_liq_3500,
#                        list_station[[i]]$soil_liq_7500, list_station[[i]]$soil_liq_16000, list_station[[i]]$soil_liq_36000)
     } else {
       swc <- cbind(list_station[[i]][[swc_labs[1]]], list_station[[i]][[swc_labs[2]]])
       for (n in 3:length(swc_labs)) {
         swc <- cbind(swc, list_station[[i]][[swc_labs[n]]])
       }
       names(swc) <- swc_labs
#       swc <- cbind(list_station[[i]]$SWC_10, list_station[[i]]$SWC_50,  list_station[[i]]$SWC_200,
#                    list_station[[i]]$SWC_560,list_station[[i]]$SWC_1400, list_station[[i]]$SWC_3500,
#                    list_station[[i]]$SWC_7500, list_station[[i]]$SWC_16000, list_station[[i]]$SWC_36000)
    }

    soil_temp_labs <- paste("soil_temp_", soil_head, sep="")
    soil_temp <- cbind(list_station[[i]][[soil_temp_labs[1]]], list_station[[i]][[soil_temp_labs[2]]])
      for (n in 3:length(soil_temp_labs)) soil_temp <- cbind(soil_temp, list_station[[i]][[soil_temp_labs[n]]])  
    names(soil_temp) <- soil_temp_labs

    soil_psi_labs <- paste("PSI_tot_", soil_head, sep="")
    soil_psi <- cbind(list_station[[i]][[soil_psi_labs[1]]], list_station[[i]][[soil_psi_labs[2]]])
    for (n in 3:length(soil_psi_labs)) soil_psi <- cbind(soil_psi, list_station[[i]][[soil_psi_labs[n]]])  
    names(soil_psi) <- soil_psi_labs

    print(paste("start ploting station ", i, sep=""))
    
    pdf(paste(wpath,"/",i, "_", val_aggr, "_", model_run, ".pdf", sep=""))
    
    # Meteorological Data
    YsumsP <- aggregate(meteo[,"list_station[[i]]$P"], by = format(time(meteo),"%Y"), FUN = sum, na.rm=T)
    YsumsS <- aggregate(meteo[,"list_station[[i]]$snow_o_canopy"], by = format(time(meteo),"%Y"), FUN = sum, na.rm=T)
    
    text <- c("YEAR: Precip (mm) | Snow (%)", paste(time(YsumsS), ": ", round(YsumsP), "|", round(YsumsS/YsumsP*100), sep=""))
              
    plot.zoo(meteo, ylab=c("Tair [°C]", "P [mm]"), screens = c(1,2,2), main=paste(i, " | Meteo",sep=""), 
             type=c("l","h","h"), col=c(grey(.2,.5), rgb(0,0,.8,.5), "grey80"),
             xlab="", sub="x")
    text(x=0.25, y=0.52, labels = text[1], cex = 0.8)
    for (t in 2:length(text)) text(x=0.15*(t-1), y=0.48, labels = text[t], cex = 0.8) 
    legend(x = 0.05, y = .45, legend = c("RAIN", "SNOW"), col=c(rgb(0,0,.8,.5), grey(.2,.5)), lwd=3, bty = "n", horiz = T, )
    # add yearly precip sum
   
    # Evapotranspiration
    plot.zoo(et, ylab=c("mm"), main=paste(i, " | Evapotranspiration",sep=""), 
             type=c("h","h","h"), screens = c(1,1,1), 
             col=c(grey(.2,.5), rgb(0,.8,0,.5), rgb(0,0,.8,.5)))
    legend("topleft", legend = c("ET", "E", "T"), col=c(grey(.2,.5), rgb(0,.8,0,.5), rgb(0,0,.8,.5)), lwd=3, bty = "n")
    
    plot.zoo(ETcumsum, ylab=c("mm"), main=paste(i, " | Cumulated evapotranspiration",sep=""), 
             type=c("l","l","l"), screens = c(1,1,1), lwd=3, 
             col=c(grey(.2,.5), rgb(0,.8,0,.5), rgb(0,0,.8,.5)))
    legend("topleft", legend = c("ET", "E", "T"), col=c(grey(.2,.5), rgb(0,.8,0,.5), rgb(0,0,.8,.5)), lwd=3, bty = "n")
    
    # Add daily/monthly validation of ET (B2, P2)
    
    #ET to zoo 
    if (i == "B2" | i=="I1") 
    {
      #ET_B2 <- read.csv2("./validation_data/ET_B2.csv", header=TRUE)
      data(validation)
      ET_B2 <- ET_B2
      ET_B2 <- zoo(x=ET_B2$ET_mm_d, order.by=as.Date(x=ET_B2$date,format="%d.%m.%y"))
      
      evapotrans <- list_station[[i]]$ET
      
      start_comp <- as.POSIXlt(head(time(ET_B2),1),tz="UTC")
      end_comp1  <- as.POSIXlt(tail(time(evapotrans),1),tz="UTC")
      end_comp2  <- as.POSIXlt(tail(time(ET_B2),1),tz="UTC")
      if (end_comp1 > end_comp2) end_comp <- end_comp2
      if (end_comp1 < end_comp2) end_comp <- end_comp1
      
      ET_obs <- window(x=ET_B2, start=as.Date(start_comp), end=as.Date(end_comp))
      evapotrans_d <- aggregate(evapotrans,as.Date(time(evapotrans),tz="UTC"),sum)
      ET_mod <- window(x=evapotrans_d, start=as.Date(start_comp), end=as.Date(end_comp))
      
      ET <- cbind(ET_obs, ET_mod)
      ET_month <- aggregate(x=ET, by=as.yearmon(time(ET)), FUN=sum)
      
      # validation plots ET
      plot(cumsum(ET), screens=c(1,1), type="l", col=c("black","grey"), lwd=3, 
           main=paste(i, " | Cumulated evapotranspiration", sep=""), ylab="mm")
      legend("bottomright", legend=c("observed EddyCov", "simulated GEOtop"), lwd=3, bty="n", col=c("black", "grey"))
      plot(ET_month, screens=c(1,1), type="b", pch=c(1,16), cex=2, lwd=3,
           main=paste(i, " | Monthly evapotranspiration sums"), ylab="mm", 
           col=c("black", "grey"))
      legend("bottomleft", legend=c("observed EddyCov", "simulated GEOtop"), cex=1.25, pch=c(1,16), bty="n", col=c("black", "grey"))
      
      ecdf_ET_obs <- ecdf(coredata(ET_obs))
      ecdf_ET_mod <- ecdf(coredata(ET_mod))
      
      plot(ecdf_ET_mod, pch=16, cex=1, col="grey", 
           main=paste(i," | ECDF Evapotranspiration", sep=""), xlab="ET [mm/d]")
      lines(ecdf_ET_obs, cex=1)
      legend("bottomright", cex=1.25, legend=c("simulated GEOtop","observed EddyCov"), col=c("grey", "black"), pch=16, bty="n")
      
      qqplot( coredata(ET_obs), coredata(ET_mod),
              pch=16, cex=1, xlim=c(0,10), ylim=c(0,10),
              xlab="observed EddyCov [mm/d]", ylab="simulated GEOtop [mm/d]", main=paste(i," | QQ Daily evapotranspiration sums", sep=""))
      abline(0,1, lty=3, col=grey(.2,.5), lwd=2)
      
      ggof(sim=ET_mod, obs=ET_obs, ftype="o", FUN=sum, ylab="ET [mm]", 
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      ggof(sim=ET_mod, obs=ET_obs, ftype="ma", FUN=sum, ylab="ET [mm]", 
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # analysis & visualisation of residuals
#       r <- ET_mod - ET_obs
#       hydroplot(r, FUN=sum, var.unit = "mm")
      
      # avaraged daily cycle ET sim vs obs
      doy <- format(time(ET),"%j")
      
      ET_doy_mean <- aggregate(x = ET, by = list(doy), FUN = mean)
      ET_doy_mean$r <- coredata(ET_doy_mean)[,2]-coredata(ET_doy_mean)[,1]
      
      plot.zoo(ET_doy_mean, screens = c(1,1,2), col=c(grey(.5,.5),rgb(1,0,0,.5),"black"), lwd=3,
               main = paste(i, " | Mean annual cycle evapotranspiration", sep=""), ylab=c("ET [mm]", "residuals [mm]"))
      legend("topright", legend = c("simulated GEOtop", "observed EddyCov"), col = c(rgb(1,0,0,.5),grey(.5,.5)), lwd=3, bty = "n")
    }
    
    # Radiation
    #plot.zoo(radiation)
    
    # Energy
    plot.zoo(energy, main=paste(i, " | Energy Budget",sep=""), 
             ylab = c("LE soil [W/m²]", "H soil [W/m²]",  "Surface E Balance [W/m²]", "LE over canopy [W/m²]", "H over canopy [W/m²]", "Soil Heat [W/m²]"),
             screens = c(1,2,3,4,5,6), type=c("h","h","h","h","h","h"), 
             col=c(rgb(0,0,.5,.5),rgb(.5,0,0,.5),grey(.8,.5),rgb(0,0,.5,.5),rgb(.5,0,0,.5),grey(.8,.5)))
    #legend("bottomright", legend = c("IN", "NET"), col = c(grey(.6,.5),grey(.8,.5)), lwd=3, bty = "n")
    
    # add validation energy (P2)
    # if (i == "B2" | i == "I1" | i == "P2") 
    if (i == "B2" | i == "I1") 
    {
      # Energy fluxes to zoo
      #EnergyFluxes_df <- read.csv("./validation_data//EnergyFluxesVinschgau_I_2011_05-2014_04.csv", na.strings="#N/A")
      EnergyFluxes_df <- EnergyFluxes_df
      EnergyFluxes <- zoo(x=EnergyFluxes_df[,c(2,3)], 
                          order.by=as.POSIXct(x=strptime(as.character(EnergyFluxes_df$Date_Time), format = "%d/%m/%Y %H:%M")))
      #EnergyFluxes <- EnergyFluxes[-which(is.na(time(EnergyFluxes))),]
      EnergyFluxes_NAspline <- zoo(x = na.approx.default(EnergyFluxes, na.rm = FALSE), order.by = time(EnergyFluxes))
     
      EnergyFluxes_daily <- aggregate(x = EnergyFluxes, by = as.Date(time(EnergyFluxes)), FUN = mean, na.rm=TRUE)
      EnergyFluxes_NAspline_daily <- aggregate(x = EnergyFluxes_NAspline, by = as.Date(time(EnergyFluxes)), FUN = mean, na.rm=TRUE)
      
      LE_d <- aggregate(LE_overcanopy , by = as.Date(time(LE_overcanopy)), FUN = mean)
      H_d  <- aggregate(H_overcanopy  , by = as.Date(time(H_overcanopy)) , FUN = mean)  
      
      start_comp <- as.POSIXlt(head(time(EnergyFluxes_daily),1),tz="UTC")
      end_comp1 <- as.POSIXlt(tail(time(LE_d),1),tz="UTC")
      end_comp2 <- as.POSIXlt(tail(time(EnergyFluxes_daily),1),tz="UTC")
      if (end_comp1 > end_comp2) end_comp <- end_comp2
      if (end_comp1 < end_comp2) end_comp <- end_comp1
      
      Energy_obs <- window(x=EnergyFluxes_daily, start=as.Date(start_comp), end=as.Date(end_comp))
      Energy_mod <- cbind( window(x=LE_d, start=as.Date(start_comp), end=as.Date(end_comp)), 
                           window(x=H_d , start=as.Date(start_comp), end=as.Date(end_comp)))
      names(Energy_mod) <- names(Energy_obs) <- c("LE", "H")
      
      
      start_comp <- as.POSIXlt(head(time(EnergyFluxes_NAspline_daily),1),tz="UTC")
      end_comp1 <- as.POSIXlt(tail(time(LE_d),1),tz="UTC")
      end_comp2 <- as.POSIXlt(tail(time(EnergyFluxes_NAspline_daily),1),tz="UTC")
      if (end_comp1 > end_comp2) end_comp <- end_comp2
      if (end_comp1 < end_comp2) end_comp <- end_comp1
      
      Energy_obs_naspl <- window(x=EnergyFluxes_NAspline_daily, start=as.Date(start_comp), end=as.Date(end_comp))
      Energy_mod_naspl <- cbind( window(x=LE_d, start=as.Date(start_comp), end=as.Date(end_comp)), 
                                 window(x=H_d , start=as.Date(start_comp), end=as.Date(end_comp)))
      names(Energy_mod_naspl) <- names(Energy_obs_naspl) <- c("LE", "H")  
      
      # plots
      ecdf_LE_obs <- ecdf(coredata(Energy_obs$LE))
      ecdf_LE_mod <- ecdf(coredata(Energy_mod$LE))
      ecdf_H_obs <- ecdf(coredata(Energy_obs$H))
      ecdf_H_mod <- ecdf(coredata(Energy_mod$H))
      
      op <- par(mfrow=c(2,2), pty="s")
      
      plot(ecdf_LE_mod, pch=16, cex=.3, col="grey", xlab="LE [W/mm²]",
           main=paste(i, " | ECDF LE",  sep=""))
      lines(ecdf_LE_obs, cex=.3)
      legend("topleft", legend=c("simulated GEOtop","observed EddyCov"), 
             col=c("grey", "black"), pch=16, bty="n")
      
      qqplot( coredata(Energy_obs$LE), coredata(Energy_mod$LE),
              pch=16, cex=.5, xlim=c(-100,250), ylim=c(-100,250),
              xlab="obs EddyCov [W/m²]", ylab="sim GEOtop [W/mm²]", 
              main=paste(i, " | QQ LE", sep=""))
      abline(0,1, lty=3)
      
      plot(ecdf_H_mod, pch=16, cex=.3, col="grey", xlab="H [W/mm²]",
           main=paste(i, " | ECDF H",  sep="")) 
      lines(ecdf_H_obs, cex=.3)
      legend("topleft", legend=c("simulated GEOtop","observed EddyCov"), 
             col=c("grey", "black"), pch=16, bty="n")
      
      qqplot( coredata(Energy_obs$H), coredata(Energy_mod$H),
              pch=16, cex=.5, xlim=c(-100,250), ylim=c(-100,250),
              xlab="H obs [W/m²]", ylab="H sim [W/mm²]", 
              main=paste(i, " | QQ H", sep=""))
      abline(0,1, lty=3)
      
      par(op)
      
      ggof(sim=Energy_mod$LE, obs=Energy_obs$LE, ftype="o", FUN=mean, ylab="LE [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      ggof(sim=Energy_mod_naspl$LE, obs=Energy_obs_naspl$LE, ftype="o", FUN=mean, ylab="LE [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      ggof(sim=Energy_mod$LE, obs=Energy_obs$LE, ftype="ma", FUN=mean, ylab="LE [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      ggof(sim=Energy_mod_naspl$LE, obs=Energy_obs_naspl$LE, ftype="ma", FUN=mean, ylab="LE [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
#       residual_LE <- Energy_mod_naspl$LE- Energy_obs_naspl$LE
#       hydroplot(residual_LE, FUN=sum, var.unit = "W/m²")
      
      ggof(sim=Energy_mod$H, obs=Energy_obs$H, ftype="o", FUN=mean, ylab="H [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      ggof(sim=Energy_mod_naspl$H, obs=Energy_obs_naspl$H, ftype="o", FUN=mean, ylab="H [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      ggof(sim=Energy_mod$H, obs=Energy_obs$H, ftype="ma", FUN=mean, ylab="H [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      ggof(sim=Energy_mod_naspl$H, obs=Energy_obs_naspl$H, ftype="ma", FUN=mean, ylab="H [W/m²]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
#       residual_H  <- Energy_mod_naspl$H - Energy_obs_naspl$H
#       hydroplot(residual_H, FUN=sum, var.unit = "W/m²")
      
    }
    
    # Soil Water pressure head (pF values) 
    # units: milimeters convert to cm
    soil_psi <- soil_psi / 10

    max_psi <- max(log10(abs(soil_psi)),na.rm=T)
    max_time <- max(as.numeric(time(soil_psi)))

    plot.zoo(log10(abs(soil_psi)), main=paste(i, " | Soil Water Potential"), ylim = c(0,max_psi),
             ylab=paste("pF ", soil_head/1000, "m [-]",sep=""), 
             panel=function(x,y,...) {
               lines(x,y)
               abline(h=c(1.8,2.5,4.2), col=rgb(1,0,0,.3), lty="dashed")
               text(x = max_time, y = c((1.8+2.5)/2,4.2), labels = c("FC", "PWP"), col=rgb(1,0,0,.5))
             })

    if (val_aggr=="d") psi_liq <- aggregate(soil_psi, as.Date(time(soil_psi)), mean)
    # stay on hourly data
    if (val_aggr=="h") psi_liq <- zoo(coredata(soil_psi), as.POSIXct(time(soil_psi)))

    # 50mm 
    soil_psi5  <- psi_liq[,which.min(abs(soil_head-50))]
    # 200mm
    soil_psi20 <- psi_liq[,which.min(abs(soil_head-200))]
    
    if (i=="B2") 
    {
      # compare with mesure soil water pressure
      PSI <- dB_getSWP(path2files = as.character(SWCinfo[SWCinfo$STATION==i,2]), 
                       header.file = as.character(SWCinfo[SWCinfo$STATION==i,3]),
                       station = as.character(SWCinfo[SWCinfo$STATION==i,4]), 
                       station_nr = as.integer(substr(SWCinfo[SWCinfo$STATION==i,1],2,2)),
                       aggregation = val_aggr)
      
      # chron2posix
      posTime <- as.POSIXct(time(PSI))
      PSI <- zoo(coredata(PSI), posTime)
      
      psi_5 <- rowMeans(PSI[,c(2,3)])
      psi_5 <- zoo(psi_5, time(PSI))
      psi_5 <- log10(psi_5)
      
      soil_psi5 <- log10(-soil_psi5)
      
      swp5 <- merge(psi_5,soil_psi5)
      names(swp5) <- c("psi_obs", "psi_sim")
      
      psi_20 <- PSI[,4]
      psi_20 <- log10(psi_20)
      
      soil_psi20 <- log10(-soil_psi20)
      
      swp20 <- merge(psi_20,soil_psi20)
      names(swp20) <- c("psi_obs", "psi_sim")
      
      ggof(sim = swp5$psi_sim, obs = swp5$psi_obs,
           ftype="o", FUN=mean, ylab="pF in 5cm depth [-]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      ggof(sim = swp20$psi_sim, obs = swp20$psi_obs,
           ftype="o", FUN=mean, ylab="pF in 20cm depth [-]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # scatterplot
     
      
      plot(x = swp5$psi_sim, y =  swp5$psi_obs, ylim=c(0,7), xlim=c(0,7), ylab="observerd pF", xlab="simulated pF", 
           main="Water Potential sim vs. obs | 5cm")
      abline(a = 0,b = 1, col="grey", lty="dashed")
      abline(h=c(1.8,2.5,4.2), col=rgb(1,0,0,.3), lty="dashed")
      abline(v=c(1.8,2.5,4.2), col=rgb(1,0,0,.3), lty="dashed")
      text(x = 7, y = c((1.8+2.5)/2,4.2), labels = c("FC", "PWP"), col=rgb(1,0,0,.5))
      text(y = 7, x = c((1.8+2.5)/2,4.2), labels = c("FC", "PWP"), col=rgb(1,0,0,.5))
      text(x = 0, y = c(1.8,2.5,4.2), labels = as.character(c(1.8,2.5,4.2)), col=rgb(1,0,0,.5))
      text(y = 0, x = c(1.8,2.5,4.2), labels = as.character(c(1.8,2.5,4.2)), col=rgb(1,0,0,.5))
      
      plot(x = swp20$psi_sim, y =  swp20$psi_obs, ylim=c(0,7), xlim=c(0,7), ylab="observerd pF", xlab="simulated pF",
           main="Water Potential sim vs. obs | 5cm")
      abline(a = 0,b = 1, col="grey", lty="dashed")
      abline(h=c(1.8,2.5,4.2), col=rgb(1,0,0,.3), lty="dashed")
      abline(v=c(1.8,2.5,4.2), col=rgb(1,0,0,.3), lty="dashed")
      text(x = 7, y = c((1.8+2.5)/2,4.2), labels = c("FC", "PWP"), col=rgb(1,0,0,.5))
      text(y = 7, x = c((1.8+2.5)/2,4.2), labels = c("FC", "PWP"), col=rgb(1,0,0,.5))
      text(x = 0, y = c(1.8,2.5,4.2), labels = as.character(c(1.8,2.5,4.2)), col=rgb(1,0,0,.5))
      text(y = 0, x = c(1.8,2.5,4.2), labels = as.character(c(1.8,2.5,4.2)), col=rgb(1,0,0,.5))
      
    }
    
    # read measured data
    SWC <- dB_getSWC(path2files = as.character(SWCinfo[SWCinfo$STATION==i,2]), 
                  header.file = as.character(SWCinfo[SWCinfo$STATION==i,3]),
                  station = as.character(SWCinfo[SWCinfo$STATION==i,4]), 
                  station_nr = as.integer(substr(SWCinfo[SWCinfo$STATION==i,1],2,2)),
                  aggregation = val_aggr, calibrate = calibrate,
                  minVALUE=.05, maxVALUE=.5, remove_freezing=TRUE)
    
#     SWCcalib <- getSWC(path2files = as.character(SWCinfo[SWCinfo$STATION==i,2]), 
#                   header.file = as.character(SWCinfo[SWCinfo$STATION==i,3]),
#                   station = as.character(SWCinfo[SWCinfo$STATION==i,4]), 
#                   station_nr = as.integer(substr(SWCinfo[SWCinfo$STATION==i,1],2,2)),
#                   aggregation = val_aggr, calibration = TRUE,
#                   minVALUE=.05, maxVALUE=.5, remove_freezing=TRUE)
    
    # chron2posix
    posTime <- as.POSIXct(time(SWC))
    SWC <- zoo(coredata(SWC), posTime)
    
    # ?which sensors to use
    # ?how to aggregate (functional boxplots / contour plots / mean +- range/sd)
    
    # for B stations choose sensors to use
    # for P and I simply plot 3 sensors
    if (i=="B1") sens2use <- c(T,F,T,F,F,T,F,F,F,F,F,T)
    if (i=="B2") sens2use <- c(T,T,F,T,T,T,T,T,T,T,T,F,F,F,T,T,F,T,F,T,F,T,T)
    if (i=="B3") sens2use <- c(T,T,T,F,T,T,T,T)
    if (i=="I1" | i=="P1" | i=="P2" | i=="P3") sens2use <- rep(T,9)
    if (i=="I3") sens2use <- rep(T,6)
    
      plot_ind <- c()
      plot_ind[grep("z2",names(SWC))] <- 1
      plot_ind[grep("z5",names(SWC))] <- 2
      plot_ind[grep("z20",names(SWC))] <- 3
      
      #plot_ind <- c(plot_ind, c(1,2,3))
      
    if (val_aggr=="d") soil_liq_d <- aggregate(swc, as.Date(time(swc)), mean)
    # stay on hourly data
    if (val_aggr=="h") soil_liq_d <- zoo(coredata(swc), as.POSIXct(time(swc)))
      
      # 20mm daily
      soil_liq2 <- soil_liq_d[,which.min(abs(soil_head-20))]
      # 50mm daily
      soil_liq5 <- soil_liq_d[,which.min(abs(soil_head-50))]
      # 200mm daily 
      soil_liq20 <-  soil_liq_d[,which.min(abs(soil_head-200))]
      
      rMeans_5 <- rowMeans(SWC[,sens2use & (plot_ind==2)], na.rm=T)
      obs_mean_5cm <- zoo(rMeans_5, time(SWC))
      
      rMeans_20 <- rowMeans(SWC[,sens2use & (plot_ind==3)], na.rm=T)
      obs_mean_20cm <- zoo(rMeans_20, time(SWC))
    
    # Soil Water Retention Curve for 5 and 20cm
    
    cm5  <- which.min(abs(soil_head-50))
    cm20 <- which.min(abs(soil_head-200))
    
    # # add observed data: SWC vs. pF
    #     if (i==B2)
    #     {
    #       obs_psi_5cm <-   swp5$psi_obs 
    #       obs_psi_20cm <-  swp20$psi_obs 
    #     } else {
    #       obs_psi_5cm <- obs_psi_20cm <- NULL
    #     }  
    
    GEOtop_VisSoilWaterRet(alpha = soil_input$alpha[cm5], n = soil_input$n[cm5], theta_sat = soil_input$vwc_s[cm5], theta_res = soil_input$vwc_r[cm5], 
                           theta_pwp = soil_input$vwc_w[cm5], theta_fc = soil_input$vwc_fc[cm5], 
                           observed = NULL, add_ref_curves = T, accurate = 1, pdf = FALSE, main = "SoilWaterRetentionCurve 5cm")
    
    GEOtop_VisSoilWaterRet(alpha = soil_input$alpha[cm20], n = soil_input$n[cm20], theta_sat = soil_input$vwc_s[cm20], theta_res = soil_input$vwc_r[cm20], 
                           theta_pwp = soil_input$vwc_w[cm20], theta_fc = soil_input$vwc_fc[cm20], 
                           observed = NULL, add_ref_curves = T, accurate = 1, pdf = FALSE, main = "SoilWaterRetentionCurve 20cm")
    
    # Soil Water Content
    plot.zoo(swc*100, main=paste(i, " | Soil Water Content"), ylim = c(0,max(swc*100, na.rm = T)),
             ylab=paste(soil_head/1000,"m [vol%]",sep=""))
    
      if (i=="B1" | i=="B2" | i=="B3")
      {
        sd_5 <- apply(X=coredata(SWC[,sens2use & (plot_ind==2)]), MARGIN=1, FUN=sd, na.rm=T)
        obs_sd_5cm <- zoo(sd_5, time(SWC))
   
        sd_20 <- apply(X=coredata(SWC[,sens2use & (plot_ind==3)]), MARGIN=1, FUN=sd, na.rm=T)
        obs_sd_20cm <- zoo(sd_20, time(SWC))
        
        sd_data_5  <- merge(obs_mean_5cm, obs_sd_5cm, soil_liq5)
        sd_data_20 <- merge(obs_mean_20cm, obs_sd_20cm, soil_liq20)
     
        percentINsd_5 <- sum(sd_data_5[,3]>=(sd_data_5[,1]-sd_data_5[,2]) & 
                               sd_data_5[,3]<=(sd_data_5[,1]+sd_data_5[,2]), na.rm=T)/length(rMeans_5)*100
        percentINsd_20 <- sum(sd_data_20[,3]>=(sd_data_20[,1]-sd_data_20[,2]) & 
                                sd_data_20[,3]<=(sd_data_20[,1]+sd_data_20[,2]), na.rm=T)/length(rMeans_20)*100
      
        plot_data <- merge(SWC,soil_liq2,soil_liq5,soil_liq20)
        sens2use_plot <- c(sens2use,c(T,T,T))
        plot_ind_plot <- c(plot_ind,c(1,2,3))
        
      } else {
        plot_data <- SWC
        sens2use_plot <- sens2use
        plot_ind_plot <- plot_ind
      } 
  
      plot(plot_data[,sens2use_plot], screens=plot_ind_plot[sens2use_plot], plot.type="multiple", 
           ylim= c(0,0.6), lwd=.5,
           ylab=c("2cm [10²vol%]", "5cm [10²vol%]", "20cm [10²vol%]"), xlab="",
           main=paste(i, " | Volumetric Water Content",sep=""),
           panel=function(x,y,col=c("grey"),...)
           {
             panel.number <- parent.frame()$panel.number
             
             if (panel.number==1) 
             {
               if (i=="B1" | i=="B2" | i=="B3") {
                 lines(x, y, col=rgb(0,0,1,.75),...)
                 legend("bottomleft", col="black", bty="n",
                        legend=(c("percentage of SIM within sd of OBS",
                                  paste("5cm depth: ", round(percentINsd_5,1), "%", sep=""),
                                  paste("20cm depth: ", round(percentINsd_20,1), "%", sep=""))))
                } else {
                  if (i=="I1" | i=="P1" | i=="P2" | i=="P3")
                    lines(x, y, col=rgb(1,0,0,.75),...)
                 #lines(x[1:length(coredata(soil_liq20))], coredata(soil_liq20), col=rgb(0,0,1,.5), lwd=1)
                  
                    lines(soil_liq2, col=rgb(0,0,1,.75), lwd=1)
               }
             }
             
             if (panel.number==2) 
             {
               if (i=="B1" | i=="B2" | i=="B3")
               {
                 
                 
#                  rMeans <- rowMeans(SWC[,sens2use & (plot_ind==panel.number)], na.rm=T)
#                  sd <- apply(X=coredata(SWC[,sens2use & (plot_ind==panel.number)]), MARGIN=1, FUN=sd, na.rm=TRUE)
#                  noNA <- !is.na(sd)
#                  sd <- sd[noNA]
#                  rMeans <- rMeans[noNA]
                 
                 data_pan = plot_data[,sens2use_plot & plot_ind_plot==panel.number]
                 
                 rMeans <- apply(X=coredata(data_pan[,-grep("soil_liq",names(data_pan))]), MARGIN=1, FUN=mean, na.rm=TRUE)
                 sd <- apply(X=coredata(data_pan[,-grep("soil_liq",names(data_pan))]), MARGIN=1, FUN=sd, na.rm=TRUE)
                 noNA <- !is.na(sd)
                 sd <- sd[noNA]
                 rMeans <- rMeans[noNA]
                 polygon(c(x[noNA],rev(x[noNA])),c(rMeans+sd,rev(rMeans-sd)),col=rgb(1,0,0,0.02), border=NA)        
                 
                 #lines(x[noNA],y[noNA],col="grey",...)
                 
                 lines(x[noNA], rMeans, col=rgb(1,0,0,.75), lwd=1)   
                 #lines(x[1:length(coredata(soil_liq5))], coredata(soil_liq5), col=rgb(0,0,1,.75), lwd=1.2)
                 lines(soil_liq5, col=rgb(0,0,1,.75), lwd=1)
                 
               } else {
                 lines(x,y,col=rgb(1,0,0,.75),...)
                 #lines(x[1:length(coredata(soil_liq5))], coredata(soil_liq5), col=rgb(0,0,1,.5), lwd=1.2)
                 lines(soil_liq5, col=rgb(0,0,1,.75), lwd=1)
               }
               
             }
             
             if (panel.number==3)
             {
               if (i=="B1" | i=="B2" | i=="B3")
               {
#                  rMeans <- rowMeans(SWC[,sens2use & (plot_ind==panel.number)], na.rm=T)
#                  sd <- apply(X=coredata(SWC[,sens2use & (plot_ind==panel.number)]), MARGIN=1, FUN=sd, na.rm=TRUE)
#                  noNA <- !is.na(sd)
#                  sd <- sd[noNA]
#                  rMeans <- rMeans[noNA]
                 data_pan = plot_data[,sens2use_plot & plot_ind_plot==panel.number]
                 
                 rMeans <- apply(X=coredata(data_pan[,-grep("soil_liq",names(data_pan))]), MARGIN=1, FUN=mean, na.rm=TRUE)
                 sd <- apply(X=coredata(data_pan[,-grep("soil_liq",names(data_pan))]), MARGIN=1, FUN=sd, na.rm=TRUE)
                 noNA <- !is.na(sd)
                 sd <- sd[noNA]
                 rMeans <- rMeans[noNA]
                 polygon(c(x[noNA],rev(x[noNA])),c(rMeans+sd,rev(rMeans-sd)),col=rgb(1,0,0,0.02), border=NA)
                 
                 #lines(x[noNA],y[noNA], col="grey",...)
                 
                 lines(x[noNA], rMeans, col=rgb(1,0,0,.75), lwd=1)
                 
                 #lines(x[1:length(coredata(soil_liq20))], coredata(soil_liq20), col=rgb(0,0,1,.75), lwd=1)
                 lines(soil_liq20, col=rgb(0,0,1,.75), lwd=1)
                 
               } else {
                 lines(x,y, rgb(1,0,0,.75),...)
                # lines(x[1:length(coredata(soil_liq20))], coredata(soil_liq20), col=rgb(0,0,1,.5), lwd=1)
                 lines(soil_liq20, col=rgb(0,0,1,.75), lwd=1)
                 
               }
               
               legend("bottomleft", legend = c("OBS", "SIM"), col=c(rgb(1,0,0,1),rgb(0,0,1,.5)), 
                      lwd=2, bty="n")
             }
             
           }) 

    # add gof
    # 2cm
     if (i=="P1" | i=="P2" | i=="P3" | i=="I1")
     {
       rMeans_2 <- rowMeans(SWC[,sens2use & (plot_ind==1)], na.rm=F)
       obs_mean_2cm <- zoo(rMeans_2, time(SWC))
       
       swc2 <- merge(obs_mean_2cm,soil_liq2)
       ggof(sim = swc2$soil_liq2, swc2$obs_mean_2cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
     
      rMeans_5 <- rowMeans(SWC[,sens2use & (plot_ind==2)], na.rm=F)
       obs_mean_5cm <- zoo(rMeans_5, time(SWC))
       
       rMeans_20 <- rowMeans(SWC[,sens2use & (plot_ind==3)], na.rm=F)
       obs0_mean_20cm <- zoo(rMeans_20, time(SWC))
       
       swc5 <- merge(obs_mean_5cm,soil_liq5)
       ggof(sim = swc5$soil_liq5, obs = swc5$obs_mean_5cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
       
       swc20 <- merge(obs_mean_20cm,soil_liq20)
       ggof(sim = swc20$soil_liq20, obs = swc20$obs_mean_20cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
     }
     if (i=="I3")
     {
       rMeans_5 <- rowMeans(SWC[,sens2use & (plot_ind==2)], na.rm=F)
       obs_mean_5cm <- zoo(rMeans_5, time(SWC))
       
       rMeans_20 <- rowMeans(SWC[,sens2use & (plot_ind==3)], na.rm=F)
       obs0_mean_20cm <- zoo(rMeans_20, time(SWC))
       
       swc5 <- merge(obs_mean_5cm,soil_liq5)
       ggof(sim = swc5$soil_liq5, obs = swc5$obs_mean_5cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
       
       swc20 <- merge(obs_mean_20cm,soil_liq20)
       ggof(sim = swc20$soil_liq20, obs = swc20$obs_mean_20cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
     }
     if (i=="B1" | i=="B2" | i=="B3")
     {
       # 5cm
       swc5 <- merge(obs_mean_5cm,soil_liq5)
       ggof(sim = swc5$soil_liq5, obs = swc5$obs_mean_5cm,
            ftype="o", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
       if (val_aggr=="d")
       ggof(sim = swc5$soil_liq5, obs = swc5$obs_mean_5cm,
            ftype="dm", FUN=mean, ylab="VWC [10²vol%]",
            gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
       # 20cm
       swc20 <- merge(obs_mean_20cm,soil_liq20)
       ggof(sim = swc20$soil_liq20, obs = swc20$obs_mean_20cm,
                 ftype="o", FUN=mean, ylab="VWC [10²vol%]",
                 gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
       if (val_aggr=="d")
       ggof(sim = swc20$soil_liq20, obs = swc20$obs_mean_20cm,
                 ftype="dm", FUN=mean, ylab="VWC [10²vol%]",
                 gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))  
     }

# soil temperature
    SoilTempObs <- dB_getSoilTemp(path2files = as.character(SWCinfo[SWCinfo$STATION==i,2]), 
                       header.file = as.character(SWCinfo[SWCinfo$STATION==i,3]),
                       station = as.character(SWCinfo[SWCinfo$STATION==i,4]), 
                       station_nr = as.integer(substr(SWCinfo[SWCinfo$STATION==i,1],2,2)),
                       aggregation = val_aggr,
                       minVALUE=-30, maxVALUE=30)

    if (i=="B2") sens2use <- c(F,T,T,F,T,F,T,T,T,T)
    if (i=="P2") sens2use <- rep(T,9)
    SoilTempObs <- SoilTempObs[,sens2use]

    # chron2posix
    posTime <- as.POSIXct(time(SoilTempObs))
    SoilTempObs <- zoo(coredata(SoilTempObs), posTime)

    if (val_aggr=="d") soil_temp_d <- aggregate(soil_temp, as.Date(time(soil_temp)), mean)
    # stay on hourly data
    if (val_aggr=="h") soil_temp_d <- zoo(coredata(soil_temp), as.POSIXct(time(soil_temp)))
    
    if (i == "B2")
    {
      # 0mm
      soil_temp0_sim  <- soil_temp_d[,which.min(abs(soil_head-0))]
      grep <- grep("_z0", names(SoilTempObs))
      if (length(grep)==1) soil_temp0_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
        soil_temp0_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))
      # 100mm 
      soil_temp10_sim <-  soil_temp_d[,which.min(abs(soil_head-100))]
      grep <- grep("_z10", names(SoilTempObs))
      if (length(grep)==1) soil_temp10_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
        soil_temp10_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))
      # 500mm 
      soil_temp50_sim <-  soil_temp_d[,which.min(abs(soil_head-500))]
      grep <- grep("_z50", names(SoilTempObs))
      if (length(grep)==1) soil_temp50_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
        soil_temp50_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))
    }

    if (i == "P2")
    {
      # 20mm
      soil_temp2_sim  <- soil_temp_d[,which.min(abs(soil_head-20))]
      grep <- grep("_z2_", names(SoilTempObs))
      if (length(grep)==1) soil_temp2_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
        soil_temp2_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))
    }

      # 50mm
      soil_temp5_sim  <- soil_temp_d[,which.min(abs(soil_head-50))]
      # 200mm 
      soil_temp20_sim <-  soil_temp_d[,which.min(abs(soil_head-200))]
    
   if (i=="B2") {
     only5 <- !grep("_z5", names(SoilTempObs))%in%grep("_z50", names(SoilTempObs))
     grep <- grep("_z5", names(SoilTempObs))[only5]
   }

   if (i=="P2") {
     grep <- grep("_z5_", names(SoilTempObs))
   }
    if (length(grep)==1) soil_temp5_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
      soil_temp5_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))

    grep <- grep("_z20", names(SoilTempObs))
    if (length(grep)==1) soil_temp20_obs <- zoo(SoilTempObs[,grep], time(SoilTempObs)) else 
      soil_temp20_obs <- zoo(rowMeans(SoilTempObs[,grep], na.rm=T), time(SoilTempObs))

    if (i=="B2") {
      # 0cm
      soil_temp0 <- merge(soil_temp0_sim,soil_temp0_obs)
      ggof(sim = soil_temp0$soil_temp0_sim, obs = soil_temp0$soil_temp0_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp0$soil_temp0_sim, obs = soil_temp0$soil_temp0_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # 5cm
      soil_temp5 <- merge(soil_temp5_sim,soil_temp5_obs)
      ggof(sim = soil_temp5$soil_temp5_sim, obs = soil_temp5$soil_temp5_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp5$soil_temp5_sim, obs = soil_temp0$soil_temp5_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      #10cm
      soil_temp10 <- merge(soil_temp10_sim,soil_temp10_obs)
      ggof(sim = soil_temp10$soil_temp10_sim, obs = soil_temp10$soil_temp10_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp10$soil_temp10_sim, obs = soil_temp10$soil_temp10_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # 20cm
      soil_temp20 <- merge(soil_temp20_sim,soil_temp20_obs)
      ggof(sim = soil_temp20$soil_temp20_sim, obs = soil_temp20$soil_temp20_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp20$soil_temp20_sim, obs = soil_temp20$soil_temp20_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # 50cm
      soil_temp50 <- merge(soil_temp50_sim,soil_temp50_obs)
      ggof(sim = soil_temp50$soil_temp50_sim, obs = soil_temp50$soil_temp50_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp50$soil_temp50_sim, obs = soil_temp50$soil_temp50_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
    }

    if (i=="P2") {
      # 2cm
      soil_temp2 <- merge(soil_temp2_sim,soil_temp2_obs)
      ggof(sim = soil_temp2$soil_temp2_sim, obs = soil_temp2$soil_temp2_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp2$soil_temp2_sim, obs = soil_temp2$soil_temp2_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # 5cm
      soil_temp5 <- merge(soil_temp5_sim,soil_temp5_obs)
      ggof(sim = soil_temp5$soil_temp5_sim, obs = soil_temp5$soil_temp5_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp5$soil_temp5_sim, obs = soil_temp0$soil_temp5_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      
      # 20cm
      soil_temp20 <- merge(soil_temp20_sim,soil_temp20_obs)
      ggof(sim = soil_temp20$soil_temp20_sim, obs = soil_temp20$soil_temp20_obs,
           ftype="o", FUN=mean, ylab="soilT [degC]",
           gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
      if (val_aggr=="d")
        ggof(sim = soil_temp20$soil_temp20_sim, obs = soil_temp20$soil_temp20_obs,
             ftype="dm", FUN=mean, ylab="soilT [degC]",
             gofs = c("MAE", "RMSE", "NRMSE", "NSE", "PBIAS"))
    }

    dev.off()
    
    print(paste("end ploting station ", i, sep=""))
    print(paste("PDF ", paste(wpath,"/",i, "_", model_run, ".pdf", sep=""), " created", sep=""))
  }
  
}
