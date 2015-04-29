#--------------------------------------------------------------------------------------------------------------
# FUNCTION read GEOtop point output

# ARGUMENTS
# wpath     (working) path in GEOtop output folder
# keyword   keyword for files to read (point: "PointOutputFile", snow: "SnowProfileFile", snowcover: "SnowCoveredAreaFile",
#           thetaliq: "SoilLiqContentProfileFile", thetaliq: "SoilIceContentProfileFile")
# verOFint  keyword for variable of interest (see headers of GEOtop point output files)
# coordinates_value_df    logical (FALSE); should data frame of coordinates and corresponding values be output? 	
# landcover_classes       character vector containing landcover classes 

#           possible keywords:
#             point
#           Date12.DDMMYYYYhhmm., JulianDayFromYear0.days., TimeFromStart.days., Simulation_Period, Run,
#           IDpoint, Psnow_over_canopy.mm., Prain_over_canopy.mm., Psnow_under_canopy.mm.,
#           Prain_under_canopy.mm., Prain_rain_on_snow.mm., Wind_speed.m/s., Wind_direction.deg.,
#           Relative_Humidity.-., Pressure.mbar., Tair.C., Tdew.C., Tsurface.C., Tvegetation.C.,
#           Tcanopyair.C., Surface_Energy_balance.W/m2., Soil_heat_flux.W/m2., SWin.W/m2.,
#           SWbeam.W/m2., SWdiff.W/m2., LWin.W/m2., LWin_min.W/m2., LWin_max.W/m2., SWnet.W/m2.,
#           LWnet.W/m2., H.W/m2., LE.W/m2., Canopy_fraction.-., LSAI.m2/m2., z0veg.m., d0veg.m.,
#           Estored_canopy.W/m2., SWv.W/m2., LWv.W/m2., Hv.W/m2., LEv.W/m2., Hg_unveg.W/m2.,
#           LEg_unveg.W/m2., Hg_veg.W/m2., LEg_veg.W/m2., Evap_surface.mm., Trasp_canopy.mm.,
#           Water_on_canopy.mm., Snow_on_canopy.mm., Qvegetation.-., Qsurface.-., Qair.-., Qcanopyair.-.,
#           LObukhov.m., LObukhovcanopy.m., Wind_speed_top_canopy.m/s., Decay_of_K_in_canopy.-.,
#           SWup.W/m2., LWup.W/m2., Hup.W/m2., LEup.W/m2., snow_depth.mm., snow_water_equivalent.mm.,
#           snow_density.kg/m3., snow_temperature.C., snow_melted.mm., snow_subl.mm.,
#           snow_blown_away.mm., snow_subl_while_blown.mm., glac_depth.mm., glac_water_equivalent.mm.,
#           glac_density.kg/m3., glac_temperature.C., glac_melted.mm., glac_subl.mm.,
#           lowest_thawed_soil_depth.mm., highest_thawed_soil_depth.mm., lowest_water_table_depth.mm.,
#           highest_water_table_depth.mm.

# Evap_surface.mm. + Trasp_canopy.mm. = evapotranspiration
# 

#             theta
#           Date12.DDMMYYYYhhmm., JulianDayFromYear0.days., TimeFromStart.days., 
#           Simulation_Period, Run, IDpoint, X15.000000, X80.000000, X230.000000, X580.000000,
#           X1430.000000, X3530.000000, X7530.000000, X16030.000000, X34530.000000



# VALUE 
# zoo dataframe from begining to end of simulation

# Requires packages geotopbricks, zoo

GEOtop_ReadPointVar <- function(wpath, keyword, varOFint, coordinates_value_df=FALSE, landcover_classes)
{
#load libraries
#  require(geotopbricks)
  
#  if (coordinates_value_df==FALSE) require(zoo)
  
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

if (coordinates_value_df)
{
# get general maps for whole area
  # land cover
  lco <- get.geotop.inpts.keyword.value(keyword="LandCoverMapFile", wpath=wpath, raster=TRUE)
  # ascpect
  asp <- get.geotop.inpts.keyword.value(keyword="AspectMapFile", wpath=wpath, raster=TRUE)
  # slope
  slp <- get.geotop.inpts.keyword.value(keyword="SlopeMapFile", wpath=wpath, raster=TRUE)
  # digital elevation model
  dem <- get.geotop.inpts.keyword.value(keyword="DemFile", wpath=wpath, raster=TRUE)
  # soil type
  soil <- get.geotop.inpts.keyword.value(keyword="SoilMapFile", wpath=wpath, raster=TRUE)
  
# prepare vector of x- and y-coordinates (length = rows X cols)  
  xcord <- rep (seq( dem@extent@xmin, dem@extent@xmax, length.out=dem@ncols), dem@nrows)
  ycord_raw <- rep (x=seq( dem@extent@ymax, dem@extent@ymin, length.out=dem@nrows), 
                    times=dem@ncols)
  ycord <- ycord_raw[order(ycord_raw,decreasing=TRUE)]
  
# get vector defining position of point output coordinates in map value vector  
  pos=c()
  for (i in 1:length(xpoints))
  {
    x_a <- ((xcord<=xpoints[i]) == ((xcord+diff(xcord)[1])>xpoints[i]))
    y_a <- ((ycord>=ypoints[i]) == ((ycord-diff(ycord_raw)[1])<ypoints[i]))
    
    pos[i] <-  which(x_a==TRUE)[which(x_a==TRUE) %in% which(y_a==TRUE)]
  }

# define corresponding land cover classes to land cover values  	 
  lu_class <- c()
  for (i in 1:length(lco@data@values[pos])) lu_class[i] <- landcover_classes[lco@data@values[pos][i]]
  
  
# create dataframe of coordinates and corresponding values (land cover, aspect, ...)   
  points_df <- data.frame(x=xpoints, y=ypoints, 
                          landcover_val=lco@data@values[pos], landcover_class=lu_class,
                          dem=dem@data@values[pos],
                          aspect=asp@data@values[pos], slp=slp@data@values[pos],
                          soil_type=soil@data@values[pos])
  
  return(points_df)
} else {
# get start and end of simulation period  
  start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="Etc/GMT+1")
  end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,tz="Etc/GMT+1")
  
# get levels, number of output points
  level <- 1:length(xpoints)
  
# read point data with specified keyword  
  point_data <- get.geotop.inpts.keyword.value(keyword=keyword, wpath=wpath,
                                               raster=FALSE,
                                               data.frame=TRUE,
                                             #  start_date=as.POSIXct(start), 
                                             #  end_date=as.POSIXct(end),
                                               level=level, 
                                               date_field="Date12.DDMMYYYYhhmm.",
                                               tz="Etc/GMT+1")

# read psi 
  if (keyword=="SoilLiqWaterPressProfileFile" | keyword=="SoilTotWaterPressProfileFile")
    point_data <- get.geotop.inpts.keyword.value(keyword=keyword, wpath=wpath,
                                                 raster=FALSE,
                                                 data.frame=TRUE,
                                                 #  start_date=as.POSIXct(start), 
                                                 #  end_date=as.POSIXct(end),
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
                                                 tz="Etc/GMT+1", isNA=NA)

# extract Variable of interest
#------------------------------
  # evemtually as loop - for more variales of interest...
#------------------------------  
  # for more than one output point
  if (is.list(point_data)) {
    int <- lapply( X=point_data, FUN=function(x,a)  {
      out <- x[,a]
      return(out)
    }, 
                   a=varOFint )
    # get date  
    zoo_time <- time(point_data[[1]])
  
  # for a single output point  
  } else {
    int <- point_data[,varOFint]
    zoo_time <- time(point_data)
  }

# create zoo object

  # unlist int and write data in matrix
  dummy <- matrix(unlist(int), nrow=length(zoo_time))
  
  # create coloumn names 
  if (is.list(point_data)) 
  {
    coln <- c()
    for (i in level) coln <- c(coln,paste(varOFint,i,sep="_")) 
    colnames(dummy) <- coln
  }
  
  # do zoo
  df <- zoo(dummy, zoo_time)
  
  return(df)
}
  

}
