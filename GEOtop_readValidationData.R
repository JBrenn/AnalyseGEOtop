# Function to load GEOtop point simulation output based on observations

wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_B2_011/"
obs   <- list(hour=B2_h, day=B2_d)

GEOtop_readValidationData <- function(wpath, observations, )
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

}