#library(testthat)

test_that("Fitbit test cases", {
  # Test 1
  masterPath <- system.file("extdata", "daily-time-series", package="fitcoach")
  #masterPath <- 'inst/extdata/daily-time-series/'
  print(masterPath)
  tsFileFolder <- masterPath
  master <- createTsMasterFrame(tsFileFolder)
  master <- markValidRows(master)
  master <- master[master$valid == TRUE ,]
  expect_equal(nrow(master) , 191)

  # Test 2
  # write test case 2 here

})


