GEOtop_updateLookUpTbl <- function() {
  lookup_tbl_observation <- read.csv("/home/jbr/GitHub/AnalyseGEOtop/data/tbl_observations")
  save(list = "lookup_tbl_observation", file = "/home/jbr/GitHub/AnalyseGEOtop/data/lookup_tbl_observation.RData")
}