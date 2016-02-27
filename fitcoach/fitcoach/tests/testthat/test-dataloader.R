library('testthat')
library('R6')

### Bulk requests using our DataLoader object

expect_output(
        BulkRequest <- DataLoader$new(), 
        "Object DataLoader initialized"
    )

BulkRequest$connect(appname = "cdlr",
                    key = "227FWR",
                    secret = "3089e3d1ac5dde1aa00b54a0c8661f42"
                    )

expect_equal(BulkRequest$api_token$app$key, "227FWR")

BulkRequest$requestAndWrite(
    type = 'day', 
    activities = c('steps', 'calories', 'minutesLightlyActive'), 
    start_date = "2016-01-20", 
    end_date = "2016-02-05", 
    path = "./inst/extdata/tests/")


BulkRequest$requestAndWrite(
    type = 'intraday', 
    activities = c('steps', 'calories', 'distance'), 
    start_date = "2016-02-01", 
    path = "./inst/extdata/tests/")

# expect_equal(content(BulkRequest$response)[[1]][17][[1]]$value, "9282")

# 
# bulk_activities <- list(
#     "calories",
#     "caloriesBMR",
#     "steps",
#     "distance",
#     "floors",
#     "elevation",
#     "minutesSedentary",
#     "minutesLightlyActive",
#     "minutesFairlyActive",
#     "minutesVeryActive",
#     "activityCalories"
# )
# 
# lapply(bulk_activities, function(x) {
#     BulkRequest$get(type = 'day', 
#                     activity = x, 
#                     start_date = "2016-01-20", 
#                     end_date = "2016-02-05", 
#                     path = "./inst/extdata/tests/")
#     }
# )


