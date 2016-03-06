#library(testthat)
context("Fit util tests")

test_that("Fitutil test cases", {

    # Test 1
    masterPath <-
        system.file("extdata", "daily-time-series", package = "fitcoach")
    master <- createTsMasterFrame(masterPath)
    master <- markValidRows(master)
    master <- master[master$valid == TRUE, ]
    master <- augmentData(master)
    expect_equal(nrow(master), 191)
    
    # Test 2
    y <- createGoalVariableVector(master, goal = "calories")
    expect_gte(mean(y) , 2632)
    
    # Test 3
    x <- createDependentVariableFrame(master, goal = "calories")
    expect_equal(ncol(x), length(getDailyResourcePathList()))
    
    # Test 4 - Distance Goal
    y <- createGoalVariableVector(master, goal = "distance")
    expect_lte(mean(y) , 4.9)
    
    # Test 5 - Distance X
    x <- createDependentVariableFrame(master, goal = "distance")
    expect_equal(ncol(x), 8)
    
        
    # Intra-day tests below : Tests
    # Test 6 
     folder <-
         system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
     intraMaster <- createIntraFrame(folder)
     expect_equal(nrow(intraMaster) , 2016)

     # Test 7
     intraMaster <- augmentIntraData(intraMaster)
     expect_equal(18 , ncol(intraMaster))
     
     
     
     
     
    
})
