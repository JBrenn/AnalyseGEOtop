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

GEOtop_ReadValidationData <- function(wpath, obs, soil_files=TRUE, save_rData=TRUE)
{
  # source lookup_tbl
  data(lookup_tbl_observation)
#   lookup_tbl_observation <- apply(lookup_tbl_observation, 2, as.character)
#   lookup_tbl_observation <- as.data.frame(lookup_tbl_observation)
  
# check observation data
  if (any(names(obs)=="hour") & any(names(obs)=="day")) Donly <- which(! dimnames(obs$day)[2][[1]] %in% dimnames(obs$hour)[2][[1]]) else Donly <- NULL
  
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
  
# read data from point file (preperation)
  if (!is.null(Donly)) base <- obs$day else base <- obs$hour
  
  df_names <- as.data.frame(dimnames(base)[2][[1]])
  names(df_names) <- "name"
  varPointIn <- merge(df_names, lookup_tbl_observation, by.x="name", by.y = "obs_var")
  varPointIn_what_direct <- varPointIn$geotop_what[varPointIn$geotop_where=="PointOutputFile"]
  varPointIn_name_direct <- varPointIn$name[varPointIn$geotop_where=="PointOutputFile"]
  

  level <- 1:length(xpoints)
# read point data with specified keyword  
  point_data <- get.geotop.inpts.keyword.value(keyword="PointOutputFile", wpath=wpath,
                                                 raster=FALSE,
                                                 data.frame=TRUE,
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
                                                 tz="Etc/GMT+1")
  
#LWnet.W.m2. and SWnet.W.m2. is below the canopy, see also LE and H 
  
  
# get variables direct or sums from point data
  var_out <- list()
  for (i in 1:length(varPointIn_what_direct)) 
  {
    name <- as.character(varPointIn_name_direct)[i]
    var <- as.character(varPointIn_what_direct)[i]
    i_split <- strsplit(as.character(var),"%")
    
    if (length(i_split[[1]])==1) {
      var_out[[name]] <- point_data[,var]
    } else {
      if (i_split[[1]][3]=="plus") {
        var_out[[ i_split[[1]][1] ]] <- point_data[ ,i_split[[1]][1] ]
        var_out[[ i_split[[1]][2] ]] <- point_data[ ,i_split[[1]][2] ]
        var_out[[name]] <- point_data[ ,i_split[[1]][1] ] + point_data[ ,i_split[[1]][2] ]
      } else {
        var_out[[ i_split[[1]][1] ]] <- point_data[ ,i_split[[1]][1] ]
        var_out[[ i_split[[1]][2] ]] <- point_data[ ,i_split[[1]][2] ]
        var_out[[name]] <- point_data[ ,i_split[[1]][1] ] - point_data[ ,i_split[[1]][2] ]
      }
   
    }
  }

# postprocess LE, H over canopy
  if ("postprocess_LE" %in% varPointIn$geotop_what) 
  {
    LE <- point_data[,c("LEg_veg.W.m2.", "LEg_unveg.W.m2.", "LEv.W.m2.", "Canopy_fraction..." )]
    names(LE) <- c("g_veg", "g_unveg", "veg", "cf")
    data <- list(LE=LE)
    LE <- GEOtop_EfluxOcanopy(data = data)
    
    name <- as.character(varPointIn$name[varPointIn$geotop_what%in%"postprocess_LE"])
    var_out[[name]] <- LE[[1]]
  }
  
  if ("postprocess_H" %in% varPointIn$geotop_what)
  {
    H <- point_data[,c("Hg_veg.W.m2.", "Hg_unveg.W.m2.", "Hv.W.m2.", "Canopy_fraction..." )]
    names(H) <- c("g_veg", "g_unveg", "veg", "cf")
    data <- list(H=H)
    H <- GEOtop_EfluxOcanopy(data = data)
    
    name <- as.character(varPointIn$name[varPointIn$geotop_what%in%"postprocess_H"])
    var_out[[name]] <- H[[1]]
  }
  
# postprocess Energy Budget
# error = Rn - G - LE - H
  if ("postprocess_EB" %in% varPointIn$geotop_what)
  {
    EB <- var_out[["net_radiation"]] - var_out[["latent_heat_flux_in_air"]] - var_out[["sensible_heat_flux_in_air"]] - 
          var_out[["soil_heat_flux"]]
    
    name <- as.character(varPointIn$name[varPointIn$geotop_what%in%"postprocess_EB"])
    var_out[[name]] <- EB
  }
  
# postprocess Radiation components
  if ("postprocess_Rn" %in% varPointIn$geotop_what)
  {
    Rn <- var_out[["net_downward_shortwave_flux"]] + var_out[["net_downward_longwave_flux"]]
    
    name <- as.character(varPointIn$name[varPointIn$geotop_what%in%"postprocess_Rn"])
    var_out[[name]] <- Rn
  }
   
# get soil information
  if (length(sapply(df_names, grep, pattern="soil_moisture_content", value=T)) > 1 |
      length(sapply(df_names, grep, pattern="soil_temperature", value=T)) > 1 |
      length(sapply(df_names, grep, pattern="liquid_soil_water_pressure", value=T)) > 1)
  {
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
    soil_time <- as.POSIXlt(strptime(x = as.character(soil_file[,1]), format = "%d/%m/%Y %H:%M", tz = "Etc/GMT+1"))
    soil_header <- names(soil_file)[-c(1:6)]
  }
  
# soil moisture content
  if (length(sapply(df_names, grep, pattern="soil_moisture_content", value=T)) > 1)
  {
    names <- sapply(df_names, grep, pattern="soil_moisture_content", value=T)
    strsplit_names <- str_split(names,"_")
    split_mat <- matrix(unlist(strsplit_names),nrow = length(names), ncol=length(strsplit_names[[1]]), byrow = T)
    depth_mm <- as.integer(unique(split_mat[,4]))
    choice <- sapply(depth_mm, function(x) which.min(abs(soil_head-x)))
    
    soil_data <- zoo(soil_file[,soil_header[choice]], soil_time)
    for (i in 1:length(depth_mm)) var_out[[paste("soil_moisture_content_", depth_mm[i], sep="")]] <- soil_data[,i]
  }  
  
# soil water pressure  
  if (length(sapply(df_names, grep, pattern="liquid_soil_water_pressure", value=T)) > 1)
  {
    soil_file <- get.geotop.inpts.keyword.value(keyword="SoilLiqWaterPressProfileFile", wpath=wpath, data.frame=TRUE)
    
    names <- sapply(df_names, grep, pattern="liquid_soil_water_pressure", value=T)
    strsplit_names <- str_split(names,"_")
    split_mat <- matrix(unlist(strsplit_names),nrow = length(names), ncol=length(strsplit_names[[1]]), byrow = T)
    depth_mm <- as.integer(unique(split_mat[,5]))
    choice <- sapply(depth_mm, function(x) which.min(abs(soil_head-x)))
    
    soil_data <- zoo(soil_file[,soil_header[choice]], soil_time)
    for (i in 1:length(depth_mm)) var_out[[paste("liquid_soil_water_pressure_", depth_mm[i], sep="")]] <- soil_data[,i] / (-10)
  }
  
# soil temperature  
  if (length(sapply(df_names, grep, pattern="soil_temperature", value=T)) > 1)
  {
    soil_file <- get.geotop.inpts.keyword.value(keyword="SoilAveragedTempProfileFile", wpath=wpath, data.frame=TRUE)
    
    names <- sapply(df_names, grep, pattern="soil_temperature", value=T)
    strsplit_names <- str_split(names,"_")
    split_mat <- matrix(unlist(strsplit_names),nrow = length(names), ncol=length(strsplit_names[[1]]), byrow = T)
    depth_mm <- as.integer(unique(split_mat[,3]))
    choice <- sapply(depth_mm, function(x) which.min(abs(soil_head-x)))
    
    soil_data <- zoo(soil_file[,soil_header[choice]], soil_time)
    for (i in 1:length(depth_mm)) var_out[[paste("soil_temperature_", depth_mm[i], sep="")]] <- soil_data[,i]
    
  }
  
  if(save_rData) save(list = "var_out", file = file.path(wpath,"PointOutValidation.RData"))
  return(var_out)
}