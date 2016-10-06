# createGEOtopMAPS
  # function to create GEOtop inpts maps from DEM (fine resolution)
  # watershed delineation based on stream gauge location
  # masking of grids
  # resampling to fine resolution (res)

# AUTHOR
  # Johannes Brenner, EURAC research, Institute for Alpine Environment
  # Johannes.Brenner@eurac.edu

# VERSION 3 | 09.02.2015
  # set SAGA environment as argument
  #e.g.   SAGAENV <- rsaga.env(path = "C:/Program Files (x86)/SAGA-GIS2.1.0", modules = "C:/Program Files (x86)/SAGA-GIS2.1.0/modules")
  # pit filling resampled dem
  # 1) resampling 2) masking (with option only_mask_lc --> other maps are masked internally in GEOtop)

# ARGUMENTS
  # dem           dem ascii, located in working directory
  # res           resolution of the output maps
  # stream_gauge  ascii, defining location of stream gauge, located in working directory
  #               (NA ascii, -1 = stream gauge location)
  #               manual preprcessing with help of river/channel network
  #               ?AUTOMATISATION - search highest value in channel network around specified outlet
  # name_gauge    name of the stream gauge, character
  # preprocess    should raw DEM (fine resolution) be preprocessed?
  #               including - .asc to .sgrd for all maps
  #                         - basis morphometry (slope, aspect, curvature calculation)
  #                         - sky view factor calculation
  # soil          is soil.asc is provided in working directory
  # landcover     is landcover.asc is provided in working directory
  # 
  # SAGAENV       SAGA environment: local path, modules

# DEPENDENCIES loaded internally
  # RSAGA, rgdal,geotopbricks

# TO DO
  # possibility to stay on full DEM extent - no masking of DEM - no cropping of watershed
  # get DEM & Soil/Landcover matched
  # change ascii header output
  # example:# ncols 166 
            # nrows 192 
            # xllcorner 616083.75 
            # yllcorner 5165296.25 
            # cellsize 100 
            # NODATA_value -9999

# set working directory for test
  #setwd("H:/GIS/Arbeitsbereich/BrJ/SAGA/GEOtopMAPS/Mazia/")

# get rsaga modules & usage
  #rsaga.get.modules(env=SAGAENV)
  #rsaga.get.usage(libs = , module = , env=SAGAENV)

GEOtop_CreateInptsMAPS <- function(dem, res, stream_gauge, minArea, name_gauge,
                                  preprocess=TRUE, 
                                  SkyView=FALSE, FlowAccum=FALSE, rivernet=FALSE, idgauges=FALSE,
                                  soil=FALSE, landcover=FALSE, lc_scenes=c("landcover.asc"),
                                  bedrock=FALSE, LAI=FALSE, geology=FALSE,
                                  SAGAENV = rsaga.env(path = "/usr/bin", 
                                                      modules = "/usr/lib/x86_64-linux-gnu/saga/", 
                                                      version = "2.2.3"), 
                                  mask_only_lc=FALSE,
                                  GEOtop=FALSE, MHM=FALSE, ASCII=TRUE)
{
  # load necessary libraries
  #require(RSAGA)
  #require(rgdal)
  #require(geotopbricks)
  
  # SAGA environment setings 
  # WINDOWS
  # SAGAENV <- rsaga.env(path = "C:/Program Files (x86)/SAGA-GIS2.1.0", 
  #                      modules = "C:/Program Files (x86)/SAGA-GIS2.1.0/modules")
  # LINUX zen
   SAGAENV <- rsaga.env(path = "/usr/bin", 
                        modules = "/usr/lib/x86_64-linux-gnu/saga/", version = "2.2.3")
  
  if (preprocess)
  {
    print("START PREPROCESSING: .asc ->> .sgrd")
    
  # create DEM folder
    dir.create("./DEM")
    
  # DEM ascii to SAGA sgrd
    rsaga.esri.to.sgrd(in.grids = dem, out.sgrds = "./DEM/dem.sgrd" ,
                       env = SAGAENV)
  
  # fill sinks in DEM (method: Planchon/Darboux, 2001)
  #  rsaga.fill.sinks(in.dem = "./DEM/dem.sgrd", out.dem =  "./DEM/dem_filled.sgrd", 
  #                   env = SAGAENV)
  # find sinks in DEM and deepen terrain
    rsaga.sink.removal(in.dem = "./DEM/dem.sgrd", out.dem =  "./DEM/dem_filled.sgrd", 
                       env = SAGAENV, method = "deepen")
  # fill sinks in DEM (method: Wang & Liu, 2006)
  # plus flow direction and watershed delineation  
    print("fill sinks in DEM & FLOW DIRECTION, method: Wang & Liu, 2006")
    dir.create("FlowDir"); dir.create("Watershed")
    rsaga.geoprocessor(lib = "ta_preprocessor", module = 4, 
                       param = list(ELEV="./DEM/dem.sgrd", FILLED="./DEM/dem_filled_WangLiu2006.sgrd",
                                    FDIR="./FlowDir/fdir.sgrd", WSHED="./Watershed/watersheds.sgrd")) 
     
    print("BASIS MORPHOMETRY: SLOPE, ASPECT, CURVATURE")
    
  # create folders for slope, aspect & curvature
    dir.create("./Slope"); dir.create( "./Curvature"); dir.create("./Aspect")
    
  # basis terrain analysis: calculate slope, aspect & curvature from DEM
  
#     rsaga.geoprocessor(lib = "ta_morphometry", module = 0, env=SAGAENV,
#                        param = list(ELEVATION="./DEM/dem.sgrd", SLOPE="./Slope/slope.srgd",
#                                     ASPECT="./Aspect/aspect.srgd",
#                                     METHOD=1  # Maximum Triangle Slope Tarboton1997 #
#                                     ))
    int.saga.version <- as.integer(substr(SAGAENV$version,1,1)) *100 + 
      as.integer(substr(SAGAENV$version,3,3)) *10 + as.integer(substr(SAGAENV$version,5,5))
    
    if (int.saga.version < 211) {
      rsaga.local.morphometry(in.dem = "./DEM/dem.sgrd", 
                              out.slope = "./Slope/slope.sgrd", 
                              out.aspect = "./Aspect/aspect.sgrd",
                              out.curv = "./Curvature/curv.sgrd", 
                              env = SAGAENV, 
                              unit.slope = "degrees", unit.aspect = "degrees")
    } else {
      rsaga.slope.asp.curv(in.dem = "./DEM/dem.sgrd", 
                           out.slope = "./Slope/slope.sgrd", 
                           out.aspect = "./Aspect/aspect.sgrd",
                           out.cgene = "./Curvature/curv.sgrd", 
                           env = SAGAENV, 
                           unit.slope = "degrees", unit.aspect = "degrees")
    }

   #rsaga.aspect(in.dem = "./DEM/dem.sgrd", out.aspect = "./Aspect/asp.sgrd", method = 5, env=SAGAENV)
   #rsaga.curvature(in.dem = "./DEM/dem.sgrd", out.aspect = "./Curvature/curv.sgrd", method = 5, env=SAGAENV)
   
  # calculate sky view factor
    if (SkyView)
    {
      print("SKY VIEW")
      dir.create("SkyViewFactor")
      rsaga.geoprocessor(lib = "ta_lighting", module = 3, env=SAGAENV, 
                         param = list(DEM="./DEM/dem.sgrd", SVF="SkyViewFactor/svf.srgd")
      )
    }
    
    if (FlowAccum)
    {
      print("FLOW ACCUMULATION")
      dir.create("FlowAccum")
      # flow accumulation by RHO8 top-down (number of cells)
      rsaga.geoprocessor(lib = "ta_hydrology", module = 0, env=SAGAENV, 
                         param = list(ELEVATION="./DEM/dem_filled.sgrd",CAREA ="FlowAccum/facc.srgd", 
                                      CAREA_UNIT=0, METHOD=1)
                         
      )
    }
  }

#END POSTPROCESS

  # additionally data from ascii to .srgd
   
  # soil
  if (soil)
  { 
    dir.create("Soil")
    
    rsaga.esri.to.sgrd(in.grids = "./soil.asc", out.sgrds = "./Soil/soil.sgrd" ,
                       env = SAGAENV)
  } 
  
  # landcover
  if (landcover)
  {
    dir.create("Landcover")
    
    for (lc in lc_scenes)
      rsaga.esri.to.sgrd(in.grids = lc, out.sgrds = paste("./Landcover/",gsub("asc", "sgrd", lc),sep=""),
                         env = SAGAENV)
  }
   
  # bedrock
  if (bedrock)
  {
    dir.create("Bedrock")
    
    rsaga.esri.to.sgrd(in.grids = "./bedrock.asc", out.sgrds = "./Bedrock/bedrock.sgrd" ,
                       env = SAGAENV)
  }
  
  # LAI
  if (LAI)
  {
    dir.create("LAI")
    
    rsaga.esri.to.sgrd(in.grids = "./lai.asc", out.sgrds = "./LAI/lai.sgrd" ,
                       env = SAGAENV)
  }
   
   # idgauges
   if (idgauges) 
   {
     dir.create("Idgauges")
     
     rsaga.esri.to.sgrd(in.grids = "./idgauges.asc", out.sgrds = "./Idgauges/idgauges.sgrd" ,
                        env = SAGAENV)
   }  
   
   # geology
   if (geology) 
   {
     dir.create("Geology")
     
     rsaga.esri.to.sgrd(in.grids = "./geology.asc", out.sgrds = "./Geology/geology.sgrd" ,
                        env = SAGAENV)
   }  

  print("WATERSHED DELINEATION")
  
# create folders for ChannelNetwork & Watershed
  dir.create("ChannelNet")

# ChannelNet ascii to SAGA .sgrd
  rsaga.esri.to.sgrd(in.grids = stream_gauge, out.sgrds = paste("./ChannelNet/",name_gauge,".sgrd",sep="") ,
                     env = SAGAENV)

# watershed delineation based on stream gauge input (stream_gauge.asc)  
  rsaga.geoprocessor(lib = "ta_channels", module = 1, env=SAGAENV,
                     param = list(ELEVATION = "./DEM/dem_filled.sgrd",
                                  CHANNELS = paste("./ChannelNet/",name_gauge,".sgrd",sep=""),
                                  BASINS = paste("./Watershed/watershed",name_gauge,".sgrd",sep=""),
                                  MINSIZE = minArea))
  
# crop watershed to valid data
#   rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
#                      param = list(INPUT=paste("./Watershed/watershed",name_gauge,".sgrd",sep=""),
#                                   OUTPUT=paste("./Watershed/watershed",name_gauge,".sgrd",sep="")))

# write out shape file of watershed
#  rsaga.geoprocessor(lib = "shapes_grid", module = 6, 
#                     param = list(GRID="./Watershed/watershed", 
#                                  POLYGONS=paste("./Watershed/watershed_",name_gauge,sep="")))
  
# DEM dummy | get resolution of input dem
dem_header <- read.ascii.grid.header(dem)

if (res!=dem_header$cellsize)
{
  print(paste("SCALING | RESOLUTION: ", res, "m", sep=""))    
  
  # Scaling to defined resolution
  #0 WATERSHED
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT=paste("./Watershed/watershed",name_gauge,".sgrd",sep=""),
                                USER_GRID=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  
  #1 DEM
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="./DEM/dem.sgrd", 
                                USER_GRID=paste("./DEM/dem",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  # fill sinks DEM in new res
  rsaga.fill.sinks(in.dem = paste("./DEM/dem",res,".sgrd",sep=""), 
                   out.dem = paste("./DEM/dem_filled",res,".sgrd",sep=""), 
                   env = SAGAENV)  
  
  #2 Aspect
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="./Aspect/aspect.sgrd", 
                                USER_GRID=paste("./Aspect/aspect",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  #3 Curvature
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="./Curvature/curv.sgrd", 
                                USER_GRID=paste("./Curvature/curv",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  #4 SkyViewFactor
  if (SkyView)
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="./SkyViewFactor/svf.sgrd", 
                                USER_GRID=paste("./SkyViewFactor/svf",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  #5 Slope
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="Slope/slope.sgrd", 
                                USER_GRID=paste("./Slope/slope",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  
  #7 FlowDir
  rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                     param=list(INPUT="FlowDir/fdir.sgrd", 
                                USER_GRID=paste("./FlowDir/fdir",res,".sgrd",sep=""),
                                USER_SIZE=res, SCALE_UP_METHOD=2))
  
  #8 FlowAccum
  if (FlowAccum)
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="FlowAccum/facc.sgrd", 
                                  USER_GRID=paste("./FlowAccum/facc",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=2))
  
  #9 Soil
  if (soil)
  {
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="Soil/soil.sgrd", 
                                  USER_GRID=paste("./Soil/soil",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=9))
    #   rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
    #                      param = list(GRID=paste("./Soil/soil",res,".sgrd",sep=""), 
    #                                   MASK=paste("./DEM/dem",res,".sgrd",sep=""),
    #                                   MASKED=paste("./Soil/soil",res,".sgrd",sep="")))
  }
  
  #10 Landcover
  if (landcover)
  {
    for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
      rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                         param=list(INPUT=paste("Landcover/",lc,".sgrd",sep=""), 
                                    USER_GRID=paste("./Landcover/",lc,res,".sgrd",sep=""),
                                    USER_SIZE=res, SCALE_UP_METHOD=9))
    #   rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
    #                      param = list(GRID=paste("./Landcover/landcover",res,".sgrd",sep=""), 
    #                                   MASK=paste("./DEM/dem",res,".sgrd",sep=""),
    #                                   MASKED=paste("./Landcover/landcover",res,".sgrd",sep="")))
  }
  
  #11 Bedrock
  if (bedrock)
  {
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="Bedrock/bedrock.sgrd", 
                                  USER_GRID=paste("./Bedrock/bedrock",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=2))
    #   rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
    #                      param = list(GRID=paste("./Bedrock/bedrock",res,".sgrd",sep=""), 
    #                                   MASK=paste("./DEM/dem",res,".sgrd",sep=""),
    #                                   MASKED=paste("./Bedrock/bedrock",res,".sgrd",sep="")))
  }
  
  # 12 LAI
  if (LAI)
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="LAI/lai.sgrd", 
                                  USER_GRID=paste("./LAI/lai",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=2))
  
  # 13 Idgauges
  if (idgauges)
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="Idgauges/idgauges.sgrd", 
                                  USER_GRID=paste("./Idgauges/idgauges",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=2))
  
  # 13 Geology
  if (geology)
    rsaga.geoprocessor(lib="grid_tools", module=0, env = SAGAENV,
                       param=list(INPUT="Geology/geology.sgrd", 
                                  USER_GRID=paste("./Geology/geology",res,".sgrd",sep=""),
                                  USER_SIZE=res, SCALE_UP_METHOD=2))
  
} else {
  # rename output
  file.copy(from = c(paste("./Watershed/watershed",name_gauge,c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./DEM/dem"),c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./DEM/dem_filled"),c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./Aspect/aspect"),c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./Curvature/curv"),c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./Slope/slope"),c(".sgrd",".mgrd",".sdat"),sep=""),
                  paste(c("./FlowDir/fdir"),c(".sgrd",".mgrd",".sdat"),sep="")),
         to = c(paste("./Watershed/watershed",name_gauge,res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./DEM/dem"),res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./DEM/dem_filled"),res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./Aspect/aspect"),res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./Curvature/curv"),res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./Slope/slope"),res,c(".sgrd",".mgrd",".sdat"),sep=""),
                paste(c("./FlowDir/fdir"),res,c(".sgrd",".mgrd",".sdat"),sep="")),
         overwrite=TRUE
               )
  
  if (soil)
    file.copy(from = c(paste("./Soil/soil",c(".sgrd",".mgrd",".sdat"),sep="")),
           to = c(paste("./Soil/soil",res,c(".sgrd",".mgrd",".sdat"),sep="")),
    overwrite=TRUE)
  
  if (landcover)
  {
    for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
      file.copy(from = c(paste("./Landcover/",lc,c(".sgrd",".mgrd",".sdat"),sep="")),
                to = c(paste("./Landcover/",lc,res,c(".sgrd",".mgrd",".sdat"),sep="")),
                overwrite=TRUE)
  }
  
  if (bedrock)
    file.copy(from = c(paste("./Bedrock/bedrock",c(".sgrd",".mgrd",".sdat"),sep="")),
           to = c(paste("./Bedrock/bedrock",res,c(".sgrd",".mgrd",".sdat"),sep="")),
           overwrite=TRUE)
  
  if (SkyView)
    file.copy(from = c(paste("./SkyViewFactor/svf",c(".sgrd",".mgrd",".sdat"),sep="")),
              to = c(paste("./SkyViewFactor/svf",res,c(".sgrd",".mgrd",".sdat"),sep="")),
              overwrite=TRUE)
  
  if (FlowAccum)
    file.copy(from = c(paste("./FlowAccum/facc",c(".sgrd",".mgrd",".sdat"),sep="")),
              to = c(paste("./FlowAccum/facc",res,c(".sgrd",".mgrd",".sdat"),sep="")),
              overwrite=TRUE)
  
  if (LAI)
    file.copy(from = c(paste("./LAI/lai",c(".sgrd",".mgrd",".sdat"),sep="")),
              to = c(paste("./LAI/lai",res,c(".sgrd",".mgrd",".sdat"),sep="")),
              overwrite=TRUE)
  
  if (idgauges)
    file.copy(from = c(paste("./Idgauges/idgauges",c(".sgrd",".mgrd",".sdat"),sep="")),
              to = c(paste("./Idgauges/idgauges",res,c(".sgrd",".mgrd",".sdat"),sep="")),
              overwrite=TRUE)
  
  if (geology)
    file.copy(from = c(paste("./Geology/geology",c(".sgrd",".mgrd",".sdat"),sep="")),
              to = c(paste("./Geology/geology",res,c(".sgrd",".mgrd",".sdat"),sep="")),
              overwrite=TRUE)
}

if (mask_only_lc) {
  #7 Landcover
  if (landcover)
  {
    for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
      rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                        param = list(GRID=paste("./Landcover/",lc,res,".sgrd",sep=""), 
                                     MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                     MASKED=paste("./Landcover/",lc,res,".sgrd",sep="")))
#     rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
#                        param = list(INPUT=paste("./Landcover/landcover",res,".sgrd",sep=""),
#                                     OUTPUT=paste("./Landcover/landcover",res,".sgrd",sep="")))
  }
} else {
  # mask maps with watershed & crop to data
  #1 DEM
  rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                     param = list(GRID=paste("./DEM/dem",res,".sgrd",sep=""), 
                                  MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                  MASKED=paste("./DEM/dem",res,".sgrd",sep="")))
  rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                     param = list(INPUT=paste("./DEM/dem",res,".sgrd",sep=""),
                                  OUTPUT=paste("./DEM/dem",res,".sgrd",sep="")))
  
  # fill sinks DEM in new res
  #rsaga.fill.sinks(in.dem = paste("./DEM/dem",res,".sgrd",sep=""), 
  #                 out.dem = paste("./DEM/dem_filled",res,".sgrd",sep=""), 
  #                 env = SAGAENV)  
  # find sinks in DEM and deepen terrain new res
  rsaga.sink.removal(in.dem = paste("./DEM/dem",res,".sgrd",sep=""), 
                     out.dem =  paste("./DEM/dem_filled",res,".sgrd",sep=""), 
                     env = SAGAENV, method = "deepen")
  
  #2 Aspect
  rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                     param = list(GRID=paste("./Aspect/aspect",res,".sgrd",sep=""), 
                                  MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                  MASKED=paste("./Aspect/aspect",res,".sgrd",sep="")))
  rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                     param = list(INPUT=paste("./Aspect/aspect",res,".sgrd",sep=""),
                                  OUTPUT=paste("./Aspect/aspect",res,".sgrd",sep="")))
  #3 Curvature
  rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                     param = list(GRID=paste("./Curvature/curv",res,".sgrd",sep=""), 
                                  MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                  MASKED=paste("./Curvature/curv",res,".sgrd",sep="")))
  rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                     param = list(INPUT=paste("./Curvature/curv",res,".sgrd",sep=""),
                                  OUTPUT=paste("./Curvature/curv",res,".sgrd",sep="")))
  
  #4 Slope
  rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                     param = list(GRID=paste("./Slope/slope",res,".sgrd",sep=""), 
                                  MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                  MASKED=paste("./Slope/slope",res,".sgrd",sep="")))
  rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                     param = list(INPUT=paste("./Slope/slope",res,".sgrd",sep=""),
                                  OUTPUT=paste("./Slope/slope",res,".sgrd",sep="")))
  
  #5 Sky View Factor
  if (SkyView) 
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./SkyViewFactor/svf",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./SkyViewFactor/svf",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./SkyViewFactor/svf",res,".sgrd",sep=""),
                                    OUTPUT=paste("./SkyViewFactor/svf",res,".sgrd",sep="")))
  }

  #6 FlowDir
  rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                     param = list(GRID=paste("./FlowDir/fdir",res,".sgrd",sep=""), 
                                  MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                  MASKED=paste("./FlowDir/fdir",res,".sgrd",sep="")))
  rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                     param = list(INPUT=paste("./FlowDir/fdir",res,".sgrd",sep=""),
                                  OUTPUT=paste("./FlowDir/fdir",res,".sgrd",sep="")))
  #7 FlowAccum
  if (FlowAccum)
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./FlowAccum/facc",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./FlowAccum/facc",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./FlowAccum/facc",res,".sgrd",sep=""),
                                    OUTPUT=paste("./FlowAccum/facc",res,".sgrd",sep="")))
  }

  #8 Soil
  if (soil)
  { 
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./Soil/soil",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./Soil/soil",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./Soil/soil",res,".sgrd",sep=""),
                                    OUTPUT=paste("./Soil/soil",res,".sgrd",sep="")))
  }
  
  #9 Landcover
  if (landcover)
  {
    for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
    {
      rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                         param = list(GRID=paste("./Landcover/",lc,res,".sgrd",sep=""), 
                                      MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                      MASKED=paste("./Landcover/",lc,res,".sgrd",sep="")))
      rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                         param = list(INPUT=paste("./Landcover/",lc,res,".sgrd",sep=""),
                                      OUTPUT=paste("./Landcover/",lc,res,".sgrd",sep="")))
    }
  }
  
  #10 Bedrock
  if (bedrock)
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./Bedrock/bedrock",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./Bedrock/bedrock",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./Bedrock/bedrock",res,".sgrd",sep=""),
                                    OUTPUT=paste("./Bedrock/bedrock",res,".sgrd",sep="")))
  }
  
  #11 LAI
  if (LAI)
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./LAI/lai",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./LAI/lai",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./LAI/lai",res,".sgrd",sep=""),
                                    OUTPUT=paste("./LAI/lai",res,".sgrd",sep="")))
  }
  
  #11 Idgauges
  if (idgauges)
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./Idgauges/idgauges",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./Idgauges/idgauges",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./Idgauges/idgauges",res,".sgrd",sep=""),
                                    OUTPUT=paste("./Idgauges/idgauges",res,".sgrd",sep="")))
  }
  
  #11 Geology
  if (geology)
  {
    rsaga.geoprocessor(lib = "grid_tools", module = 24, env = SAGAENV, 
                       param = list(GRID=paste("./Geology/geology",res,".sgrd",sep=""), 
                                    MASK=paste("./Watershed/watershed",name_gauge,res,".sgrd",sep=""),
                                    MASKED=paste("./Geology/geology",res,".sgrd",sep="")))
    rsaga.geoprocessor(lib = "grid_tools", module = 17, env=SAGAENV,
                       param = list(INPUT=paste("./Geology/geology",res,".sgrd",sep=""),
                                    OUTPUT=paste("./Geology/geology",res,".sgrd",sep="")))
  }
}

if (rivernet)
{
  print("CALCULATION RIVER NETWORK")
  
  # calculate river/channel network
  rsaga.geoprocessor(lib = "ta_channels", module = 0, env = SAGAENV,
                     param = list(ELEVATION = paste("./DEM/dem_filled",res,".sgrd",sep=""),
                                  CHNLNTWRK = paste("./ChannelNet/channelnet",res,".srgd",sep=""),
                                  INIT_GRID = paste("./DEM/dem_filled",res,".sgrd",sep="")))
}
  
  print("START POSTPROCESSING: .sgrd ->> .asc")

# create .asc from .sgrd
  if (ASCII)
  {
    rsaga.sgrd.to.esri(in.sgrds = c(paste("./DEM/dem",res,".sgrd",sep=""),
                                    paste("./DEM/dem_filled",res,".sgrd",sep=""),
                                    paste("./Aspect/aspect",res,".sgrd",sep=""),
                                    paste("./Curvature/curv",res,".sgrd",sep=""),
                                    paste("./FlowDir/fdir",res,".sgrd",sep=""),
                                    paste("./Slope/slope",res,".sgrd",sep="")),
                       out.grids = c(paste("./DEM/dem",res,".asc",sep=""),
                                     paste("./DEM/dem_filled",res,".asc",sep=""),
                                     paste("./Aspect/aspect",res,".asc",sep=""),
                                     paste("./Curvature/curv",res,".asc",sep=""),
                                     paste("./FlowDir/fdir",res,".asc",sep=""),
                                     paste("./Slope/slope",res,".asc",sep="")),
                       env=SAGAENV, prec=c(2,2,2,2,0,2))

    if (rivernet)
      rsaga.sgrd.to.esri(in.sgrds = paste("./ChannelNet/channelnet",res,".sgrd",sep=""), 
                         out.grids = paste("./ChannelNet/channelnet",res,".asc",sep=""), 
                         env=SAGAENV, prec=0)

    if (FlowAccum)
      rsaga.sgrd.to.esri(in.sgrds = paste("./FlowAccum/facc",res,".sgrd",sep=""), 
                         out.grids = paste("./FlowAccum/facc",res,".asc",sep=""), 
                         env=SAGAENV, prec=0)
    
    if (SkyView)
      rsaga.sgrd.to.esri(in.sgrds = paste("./SkyViewFactor/svf",res,".sgrd",sep=""), 
                         out.grids = paste("./SkyViewFactor/svf",res,".asc",sep=""), 
                         env=SAGAENV, prec=2)
    
    if (soil)
      rsaga.sgrd.to.esri(in.sgrds = paste("./Soil/soil",res,".sgrd",sep=""), 
                         out.grids = paste("./Soil/soil",res,".asc",sep=""), 
                         env=SAGAENV, prec=0)
    if (landcover)
    {
      for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
        rsaga.sgrd.to.esri(in.sgrds = paste("./Landcover/",lc,res,".sgrd",sep=""), 
                           out.grids = paste("./Landcover/",lc,res,".asc",sep=""), 
                           env=SAGAENV, prec=0)
    }
     
    if (bedrock)
      rsaga.sgrd.to.esri(in.sgrds = paste("./Bedrock/bedrock",res,".sgrd",sep=""), 
                         out.grids = paste("./Bedrock/bedrock",res,".asc",sep=""), 
                         env=SAGAENV, prec=2)
    
    if (LAI)
      rsaga.sgrd.to.esri(in.sgrds = paste("./LAI/lai",res,".sgrd",sep=""), 
                         out.grids = paste("./LAI/lai",res,".asc",sep=""), 
                         env=SAGAENV, prec=2)
    
    if (idgauges)
      rsaga.sgrd.to.esri(in.sgrds = paste("./Idgauges/idgauges",res,".sgrd",sep=""), 
                         out.grids = paste("./Idgauges/idgauges",res,".asc",sep=""), 
                         env=SAGAENV, prec=2)
    
    if (geology)
      rsaga.sgrd.to.esri(in.sgrds = paste("./Geology/geology",res,".sgrd",sep=""), 
                         out.grids = paste("./Geology/geology",res,".asc",sep=""), 
                         env=SAGAENV, prec=2)
    
  }
  
# POSTPROCESS: create .asc for GEOtop input
  
  if (GEOtop)
  {
    dir.create(paste("GEOtopASC",res,sep=""))  
    
    # header
    geotop_header <- c("ncols", "nrows", "xllcorner", "yllcorner", "cellsize", "nodata_value")
    
    #1.1 dem
    
    ascii <- readLines(con =paste("./DEM/dem",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/dem.asc",sep=""))
    
    #1.2 dem filled
    
    ascii <- readLines(con =  paste("./DEM/dem_filled",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/dem_filled.asc",sep=""))
    
    #2 curv
    
    ascii <- readLines(con = paste("./Curvature/curv",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/curv.asc",sep=""))
    
    #3 svf
    
    if (SkyView)
    {
      ascii <- readLines(con = paste("./SkyViewFactor/svf",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./GEOtopASC",res,"/svf.asc",sep=""))
    }
   
    #4 aspect: rad -> deg (no more necessary)
    #asp <- read.ascii.grid(file = paste("./Aspect/aspect",res,".asc",sep=""))
    #asp$data <- asp$data* 180 / pi
    #write.ascii.grid(data = asp, file = paste("./GEOtopASC",res,"/aspect.asc",sep=""))
    
    ascii <- readLines(con = paste("./GEOtopASC",res,"/aspect.asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/aspect.asc",sep=""))
    
    #5 slope: rad -> deg (no more necessary)
    #slp <- read.ascii.grid(file =  paste("./Slope/slope",res,".asc",sep=""))
    #slp$data <- slp$data* 180 / pi
    #write.ascii.grid(data = slp, file =  paste("./GEOtopASC",res,"/slope.asc",sep=""))
    
    ascii <- readLines(con = paste("./GEOtopASC",res,"/slope.asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/slope.asc",sep=""))
    
    #6 river network: all -10
    dem <- read.ascii.grid(file =  paste("./DEM/dem",res,".asc",sep=""))
    data_dem <- !is.na(dem$data)
    rivernet <- read.ascii.grid(file =  paste("./ChannelNet/channelnet",res,".asc",sep=""))
    data_rivernet <- !is.na(rivernet$data)  
    rivernet$data[data_dem] <- 0
    rivernet$data[data_rivernet] <- 10
    write.ascii.grid(data = rivernet, file = paste("./GEOtopASC",res,"/channelnet.asc",sep=""))
    
    ascii <- readLines(con = paste("./GEOtopASC",res,"/channelnet.asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./GEOtopASC",res,"/channelnet.asc",sep=""))
    
    #7 soil
    if (soil)
    { 
      ascii <- readLines(con = paste("./Soil/soil",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./GEOtopASC",res,"/soil.asc",sep=""))
    }
    
    #8 landcover
    if (landcover)
    {
      for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
      {
        ascii <- readLines(con = paste("./Landcover/",lc,res,".asc",sep=""))
        header <- ascii[1:6]
        
        for (i in 1:length(header)){
          pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
          header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
        }
        
        ascii[1:6] <- header
        writeLines(text = ascii, con = paste("./GEOtopASC",res,"/",lc,".asc",sep=""))
      }
    }
    
    #8 bedrock
    if (bedrock)
    {
      ascii <- readLines(con = paste("./Bedrock/bedrock",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./GEOtopASC",res,"/bedrock.asc",sep=""))
    }
  }
 
  #  POSTPROCESS: prepare .asc for mHM input
  if (MHM)
  {
    dir.create(paste("mHMASC",res,sep=""))  
    
    # header
    geotop_header <- c("ncols", "nrows", "xllcorner", "yllcorner", "cellsize", "NODATA_value")
    
    #1.1 dem
    ascii <- readLines(con =paste("./DEM/dem",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./mHMASC",res,"/dem.asc",sep=""))
    
    #1.2 dem filled
    ascii <- readLines(con =  paste("./DEM/dem_filled",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./mHMASC",res,"/dem_filled.asc",sep=""))
    
    #2 FlowDir 
    #----------------TODO! (1-8 ->> 1-256)
    fdir <- read.ascii.grid(file = paste("./FlowDir/fdir",res,".asc",sep=""))
    fdir$data[fdir$data == 1] <- 128
    fdir$data[fdir$data == 2] <- 1
    fdir$data[fdir$data == 3] <- 2
    fdir$data[fdir$data == 5] <- 8
    fdir$data[fdir$data == 6] <- 16
    fdir$data[fdir$data == 7] <- 32
    fdir$data[fdir$data == 0] <- 64
    write.ascii.grid(data = fdir, file = paste("./mHMASC",res,"/fdir.asc",sep=""))
    
    ascii <- readLines(con = paste("./mHMASC",res,"/fdir.asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./mHMASC",res,"/fdir.asc",sep=""))
    
    #3 FlowAccum
    if (FlowAccum)
    {
      ascii <- readLines(con = paste("./FlowAccum/facc",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./mHMASC",res,"/facc.asc",sep=""))
    }
    
    #4 aspect: rad -> deg (no more necessary)
    #asp <- read.ascii.grid(file = paste("./Aspect/aspect",res,".asc",sep=""))
    #asp$data <- asp$data* 180 / pi
    #write.ascii.grid(data = asp, file = paste("./mHMASC",res,"/aspect.asc",sep=""))
    
    ascii <- readLines(con = paste("./Aspect/aspect",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./mHMASC",res,"/aspect.asc",sep=""))
    
    #5 slope: rad -> deg (no more necessary)
    #slp <- read.ascii.grid(file =  paste("./Slope/slope",res,".asc",sep=""))
    #slp$data <- slp$data* 180 / pi
    #write.ascii.grid(data = slp, file =  paste("./mHMASC",res,"/slope.asc",sep=""))
    
    ascii <- readLines(con = paste("./Slope/slope",res,".asc",sep=""))
    header <- ascii[1:6]
    
    for (i in 1:length(header)){
      pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
      header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
    }
    
    ascii[1:6] <- header
    writeLines(text = ascii, con = paste("./mHMASC",res,"/slope.asc",sep=""))
    
    #6 LAI
    if (LAI)
    {
      ascii <- readLines(con = paste("./LAI/lai",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./mHMASC",res,"/LAI_class.asc",sep=""))
    }
    
    #7 soil
    if (soil)
    { 
      ascii <- readLines(con = paste("./Soil/soil",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./mHMASC",res,"/soil_class.asc",sep=""))
    }
    
    #8 landcover
    if (landcover)
    {
      for (lc in substr(lc_scenes, 1, nchar(lc_scenes)-4))
      {
        ascii <- readLines(con = paste("./Landcover/",lc,res,".asc",sep=""))
        header <- ascii[1:6]
        
        for (i in 1:length(header)){
          pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
          header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
        }
        
        ascii[1:6] <- header
        writeLines(text = ascii, con = paste("./mHMASC",res,"/",lc,".asc",sep=""))
      }
    }
    
    #8 idgauges
    if (idgauges)
    {
      ascii <- readLines(con = paste("./Idgauges/idgauges",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./mHMASC",res,"/idgauges.asc",sep=""))
    }
    
    #9 geology
    if (geology)
    {
      ascii <- readLines(con = paste("./Geology/geology",res,".asc",sep=""))
      header <- ascii[1:6]
      
      for (i in 1:length(header)){
        pattern_ <- strsplit(x = header[i], split = " ")[[1]][1]
        header[i] <- gsub(pattern = pattern_, replacement = geotop_header[i], x = header[i])
      }
      
      ascii[1:6] <- header
      writeLines(text = ascii, con = paste("./mHMASC",res,"/geology_class.asc",sep=""))
    }
    
    # change NA values
    files <- dir(paste("./mHMASC",res,sep=""), full.names = T)
    for (i in files)
    {
      print(paste("create",i))
      system(paste("sed -i -e 's/-99999.00/-9999/g' ",i,sep=""))
      system(paste("sed -i -e 's/-99999/-9999/g' ",i,sep=""))
    }
    
  }
    
}