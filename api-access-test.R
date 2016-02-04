# install_github("hadley/httr")
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
scope <- c("sleep","activity")  # See dev.fitbit.com/docs/oauth2/#scope
fitbit_token <- oauth2.0_token(fitbit_endpoint, myapp,
                               scope = scope, use_basic_auth = TRUE)

# 3. Make API requests

resp <- GET(url = "https://api.fitbit.com/1/user/2RY8WC/activities/date/2016-02-01.json", 
            config(token = fitbit_token))

str(resp)
resp$content
