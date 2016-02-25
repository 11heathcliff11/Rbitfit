#library(testthat)


test_that("FitAnalyzer test cases", {
  masterPath <- system.file("extdata", "daily-time-series", package="fitcoach")

  # Test 1
  ana <- FitAnalyzer$new()
  ts <- ana$getTsDailyFrame(ts.daily.json.folder = masterPath)
  vars <- ana$findImportantVariables(goal = "calories" , tsDataFrame = ts)

  # Test 2
  # write test case 2 here

})
