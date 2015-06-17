#!/usr/bin/env Rscript

library(DataBaseAlpEnvEURAC)
library(AnalyseGeotop)
library(geotopbricks)
library(geotopOptim)

args <- commandArgs(TRUE)

PATH <- argsParser(option="-wpath",args=args)
SIM  <- argsParser(option="-sim",args=args)
ST   <- argsParser(option="-st",args=args)
AGGR <- argsParser(option="-aggr",args=args)
READ <- argsParser(option="-read",args=args)
RUN  <- argsParser(option="-run",args=args)


# launch GEOtop simulation
if (RUN=="Y")
  system(command = paste("/home/jbr/hydroModels/GEOtop/uzh_exact/src/geotop/geotop ", file.path(PATH,SIM), sep=""))

if (READ=="Y") {
  read_data <- TRUE
} else {
  read_data <- FALSE
}

# run 1D analysis
  GEOtop_multiplePointPlot_Montecini(path = PATH, model_run = SIM, stations = ST, val_aggr = AGGR, read_data = read_data, calibrate = T, use_swc_liq = T, soil_files = T, linux = T)
