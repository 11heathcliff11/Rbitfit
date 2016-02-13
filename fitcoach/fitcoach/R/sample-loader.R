library(httr)
key <- '78a2a677ef62755e08d3cf4197e8042b'
secret <- 'f4bcfad2bd8afed9d9d2eae89f83f291'
clientID = "229WRY"


tokenURL <- 'http://api.fitbit.com/oauth/request_token'
accessTokenURL <- 'https://api.fitbit.com/oauth2/token'
authorizeURL <- 'https://www.fitbit.com/oauth2/authorize'
scope <- 'activity nutrition heartrate location nutrition profile settings sleep social weight'
request_url <- 'https://api.fitbit.com/1/user/-/activities/date/2015-12-12.json'

oauthString <- paste0(authorizeURL ,
                      "?response_type=token" ,
                      "&client_id=",
                      clientID,
                      "&redirect_uri=http%3A%2F%2Flocalhost%3A1410",
                      "&scope=",
                      scope,
                      "&expires_in=604800"
)
print(oauthString)


t <- GET(oauthString)

## Take the oauthString. Type in Browser and get the token from the redirected URL. Put the token String into the token field below


token <- 'Bearer eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE0NTU5NjUzMzUsInNjb3BlcyI6Indwcm8gd2xvYyB3bnV0IHdzbGUgd3NldCB3aHIgd3dlaSB3YWN0IHdzb2MiLCJzdWIiOiIzTUc0Q1kiLCJhdWQiOiIyMjlXUlkiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJpYXQiOjE0NTUzNjA1MzV9.affCK0ZECspFL_EgLxtVvQ3CeoyyKCmA3zTHCdbkWuQ'

resourcePath <- getResourcePathList()

##---------------------------------
## Load time Series Data
#timeseries
#timeseriesBaseURL <- https://api.fitbit.com/1/user/[user-id]/activities/heart/date/[date]/[period].json
## ----------------------------------------

heartBaseURL  <- 'https://api.fitbit.com/1/user'
user <- '/-'
source(file = "FitUtil.R")
date <- 'today'
period <- '1m'

request_url <- paste(timeseriesBaseURL , user , "/activities/heart/date/" , date , "/" , period , ".json" , sep="")
cat(request_url)

fetchAndStoreFile(request_url , token , "../inst/extdata/daily-time-series/heart-1m.json")

## ---------------------------------
## Load heart rate data
#  timeseriesBaseURL <- '/1/user/[user-id]/[resource-path]/date/[date]/max.json'
## ------------------------------



