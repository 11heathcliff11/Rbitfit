#library(testthat)
#library(caret)
#library(dplyr)
#library(fitcoach)
#library(R6)

context("FitAnalyzer tests")

test_that("FitAnalyzer test cases", {
  masterPath <- system.file("extdata", "daily-time-series", package = "fitcoach")
  masterPath <- 'fitcoach/inst/extdata/daily-time-series/'

  # Tests for daily-file analysis
  # Test 1
  ana <- FitAnalyzer$new("calories")
  ts <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")
  expect_equal(nrow(ts), 191)

  # Test 2
  vars <- ana$findImportantVariables(tsDataFrame = ts)
  expect_equal(vars$name[1] , "minutesLightlyActive")
  
  # Test 3
  ana$showMostImportantCharts(ts)
  
  # Test 4
  rows.test <- ts[c(3,7,10),]
  rows.test <- createDependentVariableFrame(master = rows.test , goal = "calories")
  res <- ana$predictToday(rows.test)
  expect_less_than(res[1] , 2820)
  expect_gte(res[1] , 2819)
  


  # Tests for intra-day analysis
  masterPath <- system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
  masterPath <- 'fitcoach/inst/extdata/intra-daily-timeseries/'
  ana <- FitAnalyzer$new()
  intra <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "intra.day")
  expect_equal(nrow(intra), 2016)
  
  

})


