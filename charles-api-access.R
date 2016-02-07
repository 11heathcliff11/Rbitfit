
###
### Begin Charles code
###

library(httr)

# 1. Set up credentials
fitbit_endpoint <- oauth_endpoint(
    request = "https://api.fitbit.com/oauth2/token",
    authorize = "https://www.fitbit.com/oauth2/authorize",
    access = "https://api.fitbit.com/oauth2/token")
myapp <- oauth_app(
    appname = "cdlr",
    key = "227FWR", 
    secret = "3089e3d1ac5dde1aa00b54a0c8661f42")

# 2. Get OAuth token
# Scope: see dev.fitbit.com/docs/oauth2/#scope
scope <- c("activity", "heartrate", "location", "nutrition", "profile", "settings", "sleep", "social", "weight")
fitbit_token <- oauth2.0_token(fitbit_endpoint, myapp,
                               scope = scope, use_basic_auth = TRUE)

# 3. Make API requests
date <- "2016-02-03"
geturl <- paste("https://api.fitbit.com/1/user/-/activities/date/", date, ".json", sep = "")
resp <- GET(url = geturl, config(token = fitbit_token))

# 4. Write content to file
respContent <- content(resp, as = "text")
writeBin(respContent, "charles-apicontent.json")

###
### End Charles code
###