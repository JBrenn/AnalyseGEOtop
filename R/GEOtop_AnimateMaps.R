# Animate GEOtop maps

# need to install ImageMagick from http://www.imagemagick.org/
# LINUX CentOS UHREL Ubuntu see here: http://tecadmin.net/install-imagemagick-on-linux/

# use IrfanView to display .gif image
# press G to pause animation

# TEST

# library(geotopbricks)
# library(ggplot2)
# # # 
# wpath <- "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/Discharge/WG1_005/"
# mapkey <- "SoilLiqContentTensorFile"
# mapkey <- "SWEMapFile"
# wpath <- "/data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Mazia/Discharge/WG1_005/"
# 
# layers=NULL
# soil_files=F
# variable="SWE"
# limits=c(0,300)
# legend="mm"
# lowcol="#f7fbff"
# highcol="#08306b"

GEOtop_animateMAPS <- function(wpath, mapkey, layers, soil_files,
                               variable, limits, legend, 
                               lowcol="#f7fbff", highcol="#08306b",
                               delay_value=NULL)
{
  # set animation options, convert executable
  #oopt <- ani.options(convert = "convert")
  
  # extract start and end of simulation
  start <- get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,
                                          tz="UTC") + 24*60*60
  start <- as.Date(start)
  end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="UTC")
  end <- as.Date(end)
  
  dates <- seq(from = start, to = end, by = 1)
  
  # number of layers
  # soil saturation and layer thickness from soil input 
  if (soil_files) {
    nr_soiltypes <- get.geotop.inpts.keyword.value(keyword="SoilLayerTypes", wpath=wpath, numeric=TRUE)
    soil_map <- get.geotop.inpts.keyword.value(keyword = "SoilMapFile", raster = T, wpath=wpath, isNA = -9999)
    soil_map@data@values[soil_map@data@values==-9999] <- NA
    soil_type_summary <- summary(as.factor(soil_map@data@values))
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
  
  name_maps <- get.geotop.inpts.keyword.value(keyword = mapkey, wpath=wpath)
  
  dir.create(file.path(wpath,"gif"))
  dir.create(file.path(wpath,"pdf"))
  
  if (is.null(layers)) {
    pointerMAPS <- pointer.to.maps.xyz.time(wpath, map.prefix = name_maps, 
                                            suffix = "N%04d.asc", 
                                            zoo.index = NULL, ntime=1,
                                            nlayers=length(dates))
    
    print("create maps in pdf format for each time step")
    for (d in 1:length(dates))
    {
      p <- GEOtop_ReadPlotRst(map = pointerMAPS[[d]], date = dates[d], variable = variable, layer = layers, 
                              limits = limits, legend = legend, lowcol = lowcol, highcol = highcol)
      ggsave(filename = file.path(wpath,paste("pdf/",variable,"_D",dates[d],".pdf",sep="")), plot = p)
    }
    
    setwd(file.path(wpath,"pdf"))
    file.remove("Rplot.pdf")
    
    # for linux system
    # for linux system
    print("image processing with ImageMagick")
    if (is.null(delay_value)) {
      system(paste("convert -loop 1 *.pdf ", file.path(wpath, paste("gif/", variable,".gif",sep=""))) )
    } else {
      system(paste("convert -loop 1 -delay ", delay_value, " *.pdf ", file.path(wpath, paste("gif/", variable,".gif",sep=""))) )
    }

    file.remove(dir())

  } else {
    pointerMAPS <- pointer.to.maps.xyz.time(wpath, map.prefix = name_maps, 
                                            suffix = "L%04dN%04d.asc", 
                                            zoo.index = NULL, ntime=length(dates), 
                                            nlayers=nlayers)
    
    for (i in layers)
    {
      print(paste("create maps in pdf format for each time step, layer ", i, sep=""))
      for (d in 1:length(dates))
      {
        p <- GEOtop_ReadPlotRst(map = pointerMAPS[[i]][d], date = dates[d], variable = variable, layer = i, 
                                limits = limits, legend = legend, lowcol = lowcol, highcol = highcol)
        ggsave(filename = file.path(wpath,paste("pdf/",variable,"_L",i,"_D",dates[d],".pdf",sep="")), plot = p)
      }
      
      setwd(file.path(wpath,"pdf"))
      file.remove("Rplots.pdf")
      
      # for linux system
      print("image processing with ImageMagick")
      if (is.null(delay_value)) {
        system(paste("convert -loop 1 *.pdf ", file.path(wpath, paste("gif/", variable,"_L",i,".gif",sep=""))) )
      } else {
        system(paste("convert -loop 1 -delay ", delay_value, " *.pdf ", file.path(wpath, paste("gif/", variable,"_L",i,".gif",sep=""))) )
      }
     
      file.remove(dir())
    }
  }

}
