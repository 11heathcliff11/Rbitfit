test_that("DataLoader test cases", {
    
    # Object creation 
    expect_message(
            testObject <- DataLoader$new(), 
            "Object DataLoader initialized"
        )
    
    # API connection - charles
    testObject$connect(appname = "cdlr",
                        key = "227FWR",
                        secret = "3089e3d1ac5dde1aa00b54a0c8661f42"
                        )
    expect_equal(testObject$api.token$app$key, "227FWR")
    
    # Test requests 1
    testObject$request(
        type = 'day',
        activities = c('steps', 'calories', 'minutesLightlyActive'),
        start.date = "2016-01-20",
        end.date = "2016-02-05",
        path = "./inst/extdata/tests/")
    expect_equal(fromJSON('./inst/extdata/tests/max-steps.json')[[1]][17,2], '9282')

    # Test requests 2
    testObject$request(
        type = 'intraday',
        activities = c('steps', 'calories', 'distance'),
        start.date = "2016-02-01",
        path = "./inst/extdata/tests/")
    expect_equal(fromJSON('./inst/extdata/tests/intra-steps-2016-02-01.json')[[1]][[2]], '5198')
    
    }
)