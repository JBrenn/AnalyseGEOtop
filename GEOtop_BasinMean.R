# mean Soil Moisture Content Basin

GEOtop_BasinMean <- function(wpath, mapkeys, soil_files, list_file="listpoints.txt")
{
  
  # soil water storage
  
  # soil saturation and layer thickness from soil input 
  if (soil_files) {
    nr_soiltypes <- get.geotop.inpts.keyword.value(keyword="SoilLayerTypes", wpath=wpath, numeric=TRUE)
    soil_map <- get.geotop.inpts.keyword.value(keyword = "SoilMapFile", raster = T, wpath=wpath, isNA = -9999)
    soil_map@data@values[soil_map@data@values==-9999] <- NA
    soil_type_summary <- summary(as.factor(soil_map@data@values))
    
    # ratio of soil types in basin
    soil_type_ratio   <- soil_type_summary[!names(soil_type_summary)=="NA's"] / sum(soil_type_summary[!names(soil_type_summary)=="NA's"])
    
    soil_input <- get.geotop.inpts.keyword.value(keyword="SoilParFile", wpath=wpath, data.frame=TRUE, 
                                                 level = 1:nr_soiltypes)
    
    soil_thickness_header <- get.geotop.inpts.keyword.value(keyword="HeaderSoilDz", wpath=wpath)
    soil_saturation_header <- get.geotop.inpts.keyword.value(keyword="HeaderThetaSat", wpath=wpath)
    
    if (is.list(soil_input)) {
      soil_thickness <- soil_input[[1]][,soil_thickness_header]
      
      saturation_ratio_mat <- c()
      for (i in names(soil_type_ratio))
      {
        saturation_ratio_mat <- cbind(saturation_ratio_mat, soil_input[[as.integer(i)]][,soil_saturation_header] * soil_type_ratio[i]) 
      }
      soil_saturation <- rowSums(saturation_ratio_mat)
    } else {
      soil_thickness <- soil_input[,soil_thickness_header]
      soil_saturation <- soil_input[,soil_saturation_header]
    }
    
  } else {
    soil_saturation <- get.geotop.inpts.keyword.value(keyword="ThetaSat", wpath=wpath, numeric=T)
    soil_thickness <- get.geotop.inpts.keyword.value("SoilLayerThicknesses", numeric = T, wpath=wpath)
  }
  
  nlayers <- length(soil_thickness)
  
  for (i in mapkeys)
  {
    if (mapkey=="")
  }
  
}