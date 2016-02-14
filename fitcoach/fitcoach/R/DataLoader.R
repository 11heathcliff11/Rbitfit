
### Load Libraries
library('R6')
library('httr')


DataLoader <- R6Class("DataLoader",
                      
      private = list(
          # API URLs for authentication
          api_url_request = "https://api.fitbit.com/oauth2/token",
          api_url_authorize = "https://www.fitbit.com/oauth2/authorize",
          api_url_access = "https://api.fitbit.com/oauth2/token",
          
          # Oauth token
          api_token = NA
          
      ),
      
      public = list (
          # Scope of activities to be retrieved
          # See http://dev.fitbit.com/docs/oauth2/#scope
          scope = NA,
          
          # Authentication settings
          api_appname = NA,
          api_key = NA,
          api_secret = NA,
          
          # FUNCTION
          # Initialization
          initialize = function(
              appname = "cdlr",
              key = "227FWR",
              secret = "3089e3d1ac5dde1aa00b54a0c8661f42",
              scope = c("activity", "heartrate", "location","nutrition", 
                        "profile", "settings","sleep", "social", "weight")
          ) {
              self$api_appname <- appname
              self$api_key <- key
              self$api_secret <- secret
              self$scope <- scope
          },
          
          # FUNCTION
          # API Connection
          connect = function() {
              fitbit_endpoint <- httr::oauth_endpoint(
                  request = private$api_url_request,
                  authorize = private$api_url_authorize,
                  access = private$api_url_request
              )
              
              # Get OAuth token
              private$api_token <-
                  httr::oauth2.0_token(
                      fitbit_endpoint,
                      httr::oauth_app(self$api_appname, self$api_key, self$api_secret),
                      scope = self$scope,
                      use_basic_auth = TRUE
                  )
              
          },
          
          # FUNCTION
          # Build URL, send request and write response to JSON file
          get = function(type = "summary", activity = "steps", start_date = Sys.Date(), end_date = "1d", detail_level) {
              
              # Check 'type' argument
              if(!(type %in% c("summary", "time", "intraday"))) {
                  stop("Invalid type of data. Must be 'summary', 'time' or 'intraday'")
              }
              
              ## /!\ TO-DO: check all arguments formats
              
              
              
              # Build URL for request 
              if (type == "summary") {
                  get_url <- paste("activities", 
                                   start_date,
                                   sep = "/")
              } else if (type %in% c("time", "intraday")) {
                  get_url <- paste("activities", 
                                   activity, 
                                   "date",
                                   start_date,
                                   end_date,
                                   sep = "/")
              }
              if (type == "intraday") {
                  get_url <- paste(get_url,
                                   detail_level,
                                   sep = "/")
              }
              
              get_url <- paste("https://api.fitbit.com/1/user/-/",
                               get_url,
                               ".json",
                               sep = "")    
              
              # Send the request
              self$connect()
              response <- GET(url = get_url, config(token = private$api_token))
              warn_for_status(response)
              
              # Writes the response content into a JSON file
              json_file <- paste(type, activity, start_date, end_date, detail_level, sep = "_")
              json_file <- paste("./inst/extdata/tests/", json_file, ".json", sep = "")
              resp_content <- content(response, as = "text")
              write(resp_content, json_file)
              
        }
              
      )
                      
)


TestRequest <- DataLoader$new()
TestRequest$get(type = 'intraday', 
                activity = 'steps', 
                start_date = '2016-02-03', 
                detail_level = '1min')


######## TYPES OF DATA
######## https://dev.fitbit.com/docs/activity/

### Daily Activity Summary
# https://api.fitbitcom/1/user/[user-id]/activities/date/[date].json
# 
### Activity Time Series
# /1/user/[user-id]/[resource-path]/date/[date]/[period].json
# /1/user/[user-id]/[resource-path]/date/[base-date]/[end-date].json
# 
## ACTIVITY
# 
# activities/calories  
# activities/caloriesBMR  
# activities/steps  
# activities/distance  
# activities/floors  
# activities/elevation  
# activities/minutesSedentary  
# activities/minutesLightlyActive  
# activities/minutesFairlyActive  
# activities/minutesVeryActive  
# activities/activityCalories
# 
# 
## TRACKER ACTIVITY
# 
# activities/tracker/calories  
# activities/tracker/steps  
# activities/tracker/distance  
# activities/tracker/floors  
# activities/tracker/elevation  
# activities/tracker/minutesSedentary  
# activities/tracker/minutesLightlyActive  
# activities/tracker/minutesFairlyActive  
# activities/tracker/minutesVeryActive  
# activities/tracker/activityCalories
# 
### Intraday Times Series
# 
# GET https://api.fitbit.com/1/user/-/[resource-path]/date/[date]/[date]/[detail-level].json
# GET https://api.fitbit.com/1/user/-/[resource-path]/date/[date]/1d/[detail-level].json
# GET https://api.fitbit.com/1/user/-/[resource-path]/date/[date]/[date]/[detail-level]/time/[start-time]/[end-time].json
# GET https://api.fitbit.com/1/user/-/[resource-path]/date/[date]/1d/[detail-level]/time/[start-time]/[end-time].json
# 
# 



