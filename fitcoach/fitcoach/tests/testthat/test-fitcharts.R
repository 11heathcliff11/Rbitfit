test_that("FitCharts test cases", {

    masterPath <- './inst/extdata/daily-time-series/'
    ana <- FitAnalyzer$new()
    masterData <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")

    testCharts <- MakeChart$new()
    testCharts$showCharts(data = masterData, activity = "steps")

})