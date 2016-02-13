# Fit Analyser
#
library(jsonlite)
source(file = "FitUtil.R")


tsFileFolder <- '../inst/extdata/daily-time-series/'
master <- createTsMasterFrame(tsFileFolder)
master <- markValidRows(master)
goal <- 'calories'
cat("rows for Predictive Model " , sum(master$valid))



