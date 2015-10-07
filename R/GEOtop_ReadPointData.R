# Function to load GEOtop point simulation output based on observations

#  wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_B2_007/"
 wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/MonaLisa/1D/Kaltern/sim006"
#  data("observations_B2")
#  
#  load(file.path(wpath, "obs", "observation.RData"))
#  names(observation) <- c("hour", "day")
#  obs <- observation
#  
#  obs   <- list(hour=B2_h, day=B2_d)

GEOtop_ReadPointData <- function(wpath, 
                                 soil_output_files=c("SoilLiqContentProfileFile","SoilIceContentProfileFile", "SoilLiqWaterPressProfileFile", "SoilAveragedTempProfileFile"), 
                                 soil_files=TRUE, save_rData=TRUE)
{
  
# get x- , y-coordinates of output points
  if (file.exists(file.path(wpath,"listpoints.txt")))
  {
    listpoints <- read.csv(file.path(wpath,"listpoints.txt"))
    xpoints <- listpoints$xcoord
    ypoints <- listpoints$ycoord
  } else {
    xpoints <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=T)
    ypoints <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=T)
  }
  
  level <- 1:length(xpoints)
# read point data with specified keyword  
  point_data <- get.geotop.inpts.keyword.value(keyword="PointOutputFile", wpath=wpath,
                                                 raster=FALSE,
                                                 data.frame=TRUE,
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
                                                 tz="Etc/GMT+1")
  
  dt <- as.data.table(point_data)
  
#LWnet.W.m2. and SWnet.W.m2. is below the canopy, also LE and H 
  
  
# get variables direct or postprocessed from point data 

  out <- 
  dt %>%
  # Evapotranspiration  
    mutate(Evapotranspiration.mm. = Evap_surface.mm. + Trasp_canopy.mm.) %>%
  # partitioning: 1 means full evaporation - 0 means full transpiration  
    mutate(Evapotranspiration_Partitioning = Evap_surface.mm. / Evapotranspiration.mm.) %>%
  # precipitation
    mutate(PrainPsnow_over_canopy.mm. = Psnow_over_canopy.mm. + Prain_over_canopy.mm.)  %>%
  # partitioning: 1 means full rain - 0 means full snow  
    mutate(Precipitation_part_over_canopy = Prain_over_canopy.mm. / PrainPsnow_over_canopy.mm.) %>%
  # net shortwave energy flux  
    mutate(Net_shortwave_flux_W.m2. = SWin.W.m2. - SWout.W.m2.) %>%
  # net shortwave energy flux  
    mutate(Net_longwave_flux_W.m2. = LWin.W.m2. - LWout.W.m2.) %>% 
  # net radiation 
    mutate(Net_radiation_W.m2. = Net_shortwave_flux_W.m2. + Net_longwave_flux_W.m2.) %>%
  # latent heat flux in air
    mutate(Latent_heat_flux_over_canopy_W.m2. = Canopy_fraction... * (LEg_veg.W.m2. + LEv.W.m2.) + (1-Canopy_fraction...) * LEg_unveg.W.m2.) %>%
  # sensible heat flux in air
    mutate(Sensible_heat_flux_over_canopy_W.m2. = Canopy_fraction... * (Hg_veg.W.m2. + Hv.W.m2.) + (1-Canopy_fraction...) * Hg_unveg.W.m2.) %>%
  # energy budget
    mutate(Energy_budget_storage_W.m2. = Net_radiation_W.m2. - Latent_heat_flux_over_canopy_W.m2. - Sensible_heat_flux_over_canopy_W.m2. - Soil_heat_flux.W.m2.) 
  
# get soil information
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
    
# soil moisture content liquid, soil moisture content ice, ...
    for (i in soil_output_files) {
      soil_file <- get.geotop.inpts.keyword.value(keyword=i, wpath=wpath, data.frame=TRUE)
      soil_header <- paste(substr(i,1,14), soil_head, sep="")
      names(soil_file)[7:length(soil_file)] <- soil_header
      
      out <- 
        out %>% 
        left_join(as.data.table(soil_file[,-1]))
    }

# add liquid and ice water content
  
# zoo object
  out <- zoo(out[, -c(1:5), with=F], time(point_data))  
    
# save or not    
  if(save_rData) save(list = "out", file = file.path(wpath,"PointOut.RData"))
  return(out)
}