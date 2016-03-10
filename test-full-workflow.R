###
### DataLoader R6
###

# Initialize new object
testObject <- DataLoader$new()

# Connect to API
testObject$connect(appname = "cdlr",
                   key = "227FWR",
                   secret = "3089e3d1ac5dde1aa00b54a0c8661f42"
)

# Get Daily data for all possible activities
all.daily.activities <- getDailyResourcePathList()
    
testObject$request(
    type = 'day', 
    activities = all.daily.activities, 
    start.date = "2016-01-20", 
    end.date = "2016-02-05", 
    path = "~/check-daily/")

# Get Intraday data for all possible activities
all.intraday.activities <- getIntradayResourcePathList()

testObject$request(
    type = 'intraday', 
    activities = all.intraday.activities, 
    start.date = "2016-02-01", 
    path = "~/check-intraday/")


###
### FitAnalyzer R6
###

# Get JSON files and build dataframe
masterPath <- "~/check-daily/"
ana <- FitAnalyzer$new()
masterData <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")

# Plot a chart
ana$showCharts(data = masterData, activity = "steps", average = 7)


### Final tests

# Daily analysis

masterPath <- system.file("extdata", "daily-time-series", package = "fitcoach")
ana <- FitAnalyzer$new("calories")
ts <- ana$getAnalysisFrame(folder = masterPath, analysis.type = "daily")
vars <- ana$findImportantVariables(tsDataFrame = ts, seed = 12345)
vars <- ana$findImportantVariables()
ana$showMostImportantCharts(ts)

# Intra-day analysis

masterPath <- system.file("extdata", "intra-daily-timeseries", package = "fitcoach")
ana <- FitAnalyzer$new("calories")
intra <- ana$getAnalysisFrame(folder = masterPath, analysis.type = "intra.day")
vars <- ana$findImportantVariables(intra)
vars <- sort(vars, decreasing = TRUE)
ana$showMostImportantCharts(intra)

