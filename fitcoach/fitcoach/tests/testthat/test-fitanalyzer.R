library(testthat)
library(caret)
library(dplyr)
library(fitcoach)
library(R6)

test_that("FitAnalyzer test cases", {
  masterPath <- system.file("extdata", "daily-time-series", package = "fitcoach")

  # Test 1
  ana <- FitAnalyzer$new()
  ts <- ana$getTsDailyFrame(ts.daily.json.folder = masterPath)
  expect_equal(nrow(ts), 191)

  # Test 2
  vars <- ana$findImportantVariables(goal = "calories", tsDataFrame = ts)

    # Test 3
   ana$showMostImportantCharts(ts)

})


