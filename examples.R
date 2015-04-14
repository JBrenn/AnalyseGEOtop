# test package

#-------
# package DataBaseAlpEnvEURAC provides functions to read station data from Mazia/Matsch
# hereafter used to retrieve soil moisture and soil temperature data
library(DataBaseAlpEnvEURAC)

#-------
# using functions
# 1 GEOtop_ReadPointVar

WG1_005 <- GEOtop_Read_multipoint(path = "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Discharge/",
                       model_run = "WG1_005", 
                       stations = c("M3","M4"), val_aggr = "h", 
                       soil_files = F, 
                       lc_classes = c("Urban","Forest","Grassland(dry)/Pastures","Meadows",
                                      "Rocks","Bare soil","Larch Meadows","Agriculture (Baumobst- und Weinbau)",
                                      "Glacier/Snow", "Lake/River/Bog"))

#-------
# using functions
# 1 dB_readStationData
# 2 dB_getSWC
# 3 dB_getSoilTemp

GEOtop_multiplePointPlot_Montecini(path = "Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Discharge/",
                                   model_run = "WG1_004", 
                                   stations = c("M3","M4"), val_aggr = "h", 
                                   read_data = F,calibration = F,
                                   use_swc_liq = F,soil_files = F, 
                                   lc_classes = c("Urban","Forest","Grassland(dry)/Pastures","Meadows",
                                                  "Rocks","Bare soil","Larch Meadows","Agriculture (Baumobst- und Weinbau)",
                                                  "Glacier/Snow", "Lake/River/Bog"))

#-------
#Function GEOtop_CheckHydroBudget on simulation WG1_004 (Johannes) and umvoti_011 (Shalini)

library(zoo)
library(chron)
Q_obs <- read.csv2("H:/Projekte/HiResAlp/06_Workspace/BrJ/02_data/discharge/WG1_Saldur.csv",header=T)
datetime <- chron(dates. = substr(Q_obs$Time,1,10),times. = paste(substr(Q_obs$Time,12,17),":00",sep=""),
                  format = c(dates="d.m.y", times="h:m:s"), out.format = c(dates="d/m/y", times="h:m:s"))
datetime <- as.POSIXct(datetime)
Q_obs_data <- zoo(x = as.numeric(as.character((Q_obs$Q.m3.s))), order.by = datetime)  

GEOtop_CheckHydroBudget(wpath="Y:/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/Discharge/WG1_004/", 
                        Q_obs = "hour", Q_obs_data = Q_obs_data, 
                        soil_files = F)


library(zoo)
library(chron)
library(devtools)

remove.packages("AnalyseGeotop")
install_github("JBrenn/AnalyseGeotop")
library(AnalyseGeotop)

Q_obs <- read.csv("H:/Projekte/Shalini/Data/ObsRunoff.csv",header=T)
date  <- as.Date(x = Q_obs$Date, format="%d/%m/%Y")
Q_obs_data <- zoo(x = Q_obs$obs_flow.m3.s., order.by = date)  

GEOtop_CheckHydroBudget(wpath="Y:/Simulation_GEOtop_2_0_GIT/Shalini/umvoti_011/", 
                        Q_obs = "day",  Q_obs_data = Q_obs_data,
                        soil_files = T)

