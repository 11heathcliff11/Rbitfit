#library(testthat)
context("Fit util tests")
test_that("Fitutil  test cases", {
  # Test 1
  masterPath <- system.file("extdata", "daily-time-series", package="fitcoach")
  masterPath <- 'fitcoach/inst/extdata/daily-time-series/'
  print(masterPath)
  master <- createTsMasterFrame(masterPath)
  master <- markValidRows(master)
  master <- master[master$valid == TRUE ,]
  master <- augmentData(master)
  expect_equal(nrow(master) , 191)
  
  # Test 2
  y <- createGoalVariableVector(master  , goal = "calories")
  expect_gte(mean(y) , 2632)

  # Test 3
  x <- createDependentVariableFrame(master , goal = "calories")
  expect_equal(ncol(x) , 9)

  
#  distance

  
# intra-day tests below : Tests 
  folder <- system.file("extdata", "intra-daily-timeseries", package="fitcoach")
  folder <- 'fitcoach/inst/extdata/intra-daily-timeseries/'
  intraMaster <- createIntraFrame(folder)
  intraMaster <- augmentIntraData(intraMaster)

})


