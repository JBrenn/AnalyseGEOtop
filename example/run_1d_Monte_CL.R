#!/usr/bin/env Rscript

# launch GEOtop simulation

# run 1D analysis

library(AnalyseGeotop)
library(geotopbricks)
library(geotopOptim)

args <- commandArgs(TRUE)
print(args)

PATH <- argsParser(option="-wpath",args=args)
SIM  <- argsParser(option="-sim",args=args)
ST   <- argsParser(option="-st",args=args)
AGGR <- argsParser(option="-aggr",args=args)

system(command = paste("/home/jbr/hydroModels/GEOtop/uzh_exact/src/geotop/geotop ", file.path(PATH,SIM), sep=""))

GEOtop_multiplePointPlot_Montecini(path = PATH, model_run = SIM, stations = ST, val_aggr = AGGR, read_data = T, calibrate = T, use_swc_liq = T, soil_files = T, linux = T)
