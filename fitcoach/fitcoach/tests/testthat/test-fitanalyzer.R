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
  ana <- FitAnalyzer$new()
  ts <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")
  expect_equal(nrow(ts), 191)

  # Test 2
  vars <- ana$findImportantVariables(goal = "calories", tsDataFrame = ts)
  expect_equal(vars$name[1] , "xminutesLightlyActive")
  
  # Test 3
  ana$showMostImportantCharts(ts)

  
  
  # Tests for intra-day analysis
  masterPath <- system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
  masterPath <- 'fitcoach/inst/extdata/intra-daily-timeseries/'
  ana <- FitAnalyzer$new()
  intra <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "intra.day")
  expect_equal(nrow(intra), 2016)
  
  
  
})


