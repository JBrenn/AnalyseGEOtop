# Function to load GEOtop point simulation output based on observations

wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_B2_011/"
obs   <- list(hour=B2_h, day=B2_d)

GEOtop_readValidationData <- function(wpath, observations)
{
  # source lookup_tbl
  data(lookup_tbl_observation)
#   lookup_tbl_observation <- apply(lookup_tbl_observation, 2, as.character)
#   lookup_tbl_observation <- as.data.frame(lookup_tbl_observation)
  
  # check observation data
  if (any(names(obs)=="hour") & any(names(obs)=="day")) Donly <- which(! names(obs$day) %in% names(obs$hour)) else Donly <- NULL
  
  # read data from point file
  if (!is.null(Donly)) base <- obs$day else base <- obs$hour
  
  df_names <- as.data.frame(names(base))
  varPointIn <- merge(df_names, lookup_tbl_observation, by.x="names(base)", by.y = "obs_var")
  varPointIn_what_direct <- varPointIn$geotop_what[varPointIn$geotop_where=="PointOutputFile"]
  varPointIn_name_direct <- varPointIn$`names(base)`[varPointIn$geotop_where=="PointOutputFile"]
  
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
                                                 #  start_date=as.POSIXct(start), 
                                                 #  end_date=as.POSIXct(end),
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
                                                 tz="Etc/GMT+1")
  
  var_out <- list()
  for (i in 1:length(varPointIn_what_direct)) 
  {
    name <- as.character(varPointIn_name_direct)[i]
    var <- as.character(varPointIn_what_direct)[i]
    i_split <- strsplit(as.character(var),"%")
    
    if (length(i_split[[1]])==1) {
      var_out[[name]] <- point_data[,var]
    } else {
      var_out[[ i_split[[1]][1] ]] <- point_data[ ,i_split[[1]][1] ]
      var_out[[ i_split[[1]][2] ]] <- point_data[ ,i_split[[1]][2] ]
      var_out[[name]] <- point_data[ ,i_split[[1]][1] ] + point_data[ ,i_split[[1]][2] ]
    }
  }
}