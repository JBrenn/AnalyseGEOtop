# Read GEOtop model point output (multiple points)
# --> slow / increase!
# it is only necessary to read point output data once!

# ARGUMENTS
# path        path to simulation
# model_run   simulation folder
# station     output station names (Mazia)
# val_aggr    time aggregation 
# lc_classes  names of landcover classes in simulation
# soil_files
# linux       TRUE: working on linux laptop, FALSE: working on windows laptop; for path retrieval

GEOtop_Read_multipoint <- function(path, model_run, stations, val_aggr, soil_files, lc_classes, linux)
{
  # load libraries
#   require(zoo)
#   require(geotopbricks)
#   require(hydroGOF)
#   require(hydroTSM)
#   require(ggplot2)
#   require(soilwater)
  
  #  source functions 1
  #  source("H:/Projekte/HydroAlp/06_Workspace/BrJ/03_R/GEOtopAnalyse/FUN_GEOtopReadPoint.R")
  
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
    
    soil_header <- c()
    for (i in 1:length(soil_head))
    {
      if (ceiling(soil_head[i])==soil_head[i]) {
        soil_header[i] <- paste("X", soil_head[i], ".000000", sep="")
      } else {
        soil_header[i] <- paste("X", soil_head[i], "00000", sep="")  }     
    } 
    
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
    #SWCinfo <- read.csv(file = "H:/Projekte/HydroAlp/06_Workspace/BrJ/03_R/GEOtopAnalyse/SWCinfo.txt")
    #SWCinfo <- read.csv2(file = "validation_data/SWCinfo.txt"
    
    # save workspace
    save(list = c("list_station","soil_head","soil_header","wpath","fc","soil_input"), 
         file = paste(wpath,"/point.RData",sep=""))
  
  return(list_station)
}