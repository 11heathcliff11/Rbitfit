context("FitAnalyzer tests")

test_that("FitAnalyzer test cases", {
    
    masterPath <-
        system.file("extdata", "daily-time-series", package = "fitcoach")

    ### Tests for daily-file analysis
    
    # Test 1
    ana <- FitAnalyzer$new("calories")
    ts <-
        ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")
    expect_equal(nrow(ts), 191)
    
    # Test 2
    vars <- ana$findImportantVariables(tsDataFrame = ts)
    expect_equal(vars$name[1] , "minutesLightlyActive")
    
    # Test 3
    ana$showMostImportantCharts(ts)
    
    # Test 4
    ### >>>>>> TO FIX: predictToday() does not exist yet
    #
    # rows.test <- ts[c(3,7,10),]
    # rows.test <- createDependentVariableFrame(master = rows.test , goal = "calories")
    # res <- ana$predictToday(rows.test)
    # expect_less_than(res[1] , 2820)
    # expect_gte(res[1] , 2819)
    
    ### Tests for intra-day analysis
    ### 
    ### >>>> TO FIX: looks like some intraday functions (in FitUtil and FitAnalyzer) 
    ### are not yet working
    
    # masterPath <-
    #     system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
    # ana <- FitAnalyzer$new()
    # intra <-
    #     ana$getAnalysisFrame(folder = masterPath, analysis.type = "intra.day")
    # expect_equal(nrow(intra), 2016)
    
    
    
    ### Tests for charts plotting
    
    # Test 5
    ana$showCharts(data = ts, activity = "steps", average = 7)
    
    # Test 6
    ana$showCharts(data = ts, activity = "calories", average = 4)
    
})
