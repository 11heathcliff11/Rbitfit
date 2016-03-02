test_that("FitCharts test cases", {

    masterPath <- './inst/extdata/daily-time-series/'
    ana <- FitAnalyzer$new()
    masterData <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")

    showCharts(data = masterData, x_axis = "date", y_axis = "steps")

    }
)