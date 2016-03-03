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
all_daily_activities <- getDailyResourcePathList()
    
testObject$request(
    type = 'day', 
    activities = all_daily_activities, 
    start_date = "2016-01-20", 
    end_date = "2016-02-05", 
    path = "~/check-daily/")

# Get Intraday data for all possible activities
all_intraday_activities <- getIntradayResourcePathList()

testObject$request(
    type = 'intraday', 
    activities = all_intraday_activities, 
    start_date = "2016-02-01", 
    path = "~/check-intraday/")


###
### FitAnalyzer R6
###

# Get JSON files and build dataframe
masterPath <- "~/check-daily"
ana <- FitAnalyzer$new()
masterData <- ana$getAnalysisFrame(folder = masterPath , analysis.type = "daily")

# Plot a chart
ana$showCharts(data = masterData, activity = "steps")
