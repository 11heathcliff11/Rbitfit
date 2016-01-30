
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
    secret = "97285ac3fc9fdf1ee3e7a30a15693e8b")

# 2. Get OAuth token
scope <- c("sleep","activity")  # See dev.fitbit.com/docs/oauth2/#scope
fitbit_token <- oauth2.0_token(fitbit_endpoint, myapp,
                               scope = scope, use_basic_auth = TRUE)

# 3. Make API requests
resp <- GET(url = "https://api.fitbit.com/1/user/-/activities/date/2016-01-30.json", 
            config(token = fitbit_token))
resp