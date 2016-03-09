context("FitAnalyzer tests")

test_that("FitAnalyzer test cases", {
    
    masterPath <-
        system.file("extdata", "daily-time-series", package = "fitcoach")

    ### Tests for daily-file analysis
    
    # Test 1
    ana <- FitAnalyzer$new("calories")
    ts <-
        ana$getAnalysisFrame(folder = masterPath, analysis.type = "daily")
    expect_equal(nrow(ts), 191)
    
    # Test 2
    vars <- ana$findImportantVariables(tsDataFrame = ts)
    expect_equal(vars$name[1], "minutesLightlyActive")
    
    # Test 3
    ana$findImportantVariables()
    ana$showMostImportantCharts(ts)
    
    # Test 4
    rows.test <- ts[c(3,7,10), ]
    rows.test <- createDependentVariableFrame(master = rows.test, goal = "calories")
    res <- ana$predictGoal(rows.test)
    expect_less_than(res[1], 2796)
    expect_gte(res[1], 2795)
    
    ### Tests for intra-day analysis
    
    # Test 5
    masterPath <-
        system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
    ana <- FitAnalyzer$new("calories")
    intra <-
        ana$getAnalysisFrame(folder = masterPath, analysis.type = "intra.day")
    expect_equal(nrow(intra), 2016)
    
    # Test 6
    vars <- ana$findImportantVariables(intra)
    vars <- sort(vars, decreasing = TRUE)
    expect_equal(names(vars[1]), "steps")
    
    # Test 7
    ana$showMostImportantCharts(intra)
    
    
})
