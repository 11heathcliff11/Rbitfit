
### Load Libraries
library('R6')
library('httr')


DataLoader <- R6Class("DataLoader",
                      
  private = list(
      
      # API URLs for authentication
      api_url_request = "https://api.fitbit.com/oauth2/token",
      api_url_authorize = "https://www.fitbit.com/oauth2/authorize",
      api_url_access = "https://api.fitbit.com/oauth2/token",
      
      # Authentication settings
      api_token = NA,
      api_appname = NA,
      api_key = NA,
      api_secret = NA,
      
      # Scope of activities to be retrieved
      # See http://dev.fitbit.com/docs/oauth2/#scope
      scope = NA,
      
      # Request paramaters
      req_type = NA,
      req_activity = NA,
      req_start_date = NA,
      req_end_date = NA,
      req_detail_level = NA,
      req_url = NA
      
  ),
  
  public = list (
      
      # Request response
      response = NA,
      
      # JSON extracts
      json_list = NA,
      json_df = NA,
      
      ### FUNCTION Initialize
      
      initialize = function(
          appname = "cdlr",
          key = "227FWR",
          secret = "3089e3d1ac5dde1aa00b54a0c8661f42",
          scope = c("activity", "heartrate", "location","nutrition", 
                    "profile", "settings","sleep", "social", "weight")
      ) {
          private$api_appname <- appname
          private$api_key <- key
          private$api_secret <- secret
          private$scope <- scope
      },
      
      
      ### FUNCTION Connect
      ### 
      ### API Connection
      connect = function() {
          
          fitbit_api <- httr::oauth_endpoint(
              request = private$api_url_request,
              authorize = private$api_url_authorize,
              access = private$api_url_request
          )
          
          private$api_token <-
              httr::oauth2.0_token(
                  fitbit_api,
                  httr::oauth_app(private$api_appname, private$api_key, private$api_secret),
                  scope = private$scope,
                  use_basic_auth = TRUE
              )
          
      },
      
      ### FUNCTION Request
      ### 
      ### Build URL, send request and write response to JSON file
      request = function(debug = FALSE) {
          
          # Check 'type' argument
          if(!(private$req_type %in% c("day", "intraday")))
              stop("Invalid 'req_type'. Must be 'day' or 'intraday'")
          
          # Check 'start_date' argument
          if(!(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", private$req_start_date)))
              stop("Invalid 'start_date'. Must be in the following format: 'YYYY-MM-dd'")
          

          # Build URL for request 
          private$req_url <- paste("activities", 
                                   private$req_activity, 
                                   "date",
                                   private$req_start_date,
                                   private$req_end_date,
                                   sep = "/")
          
          if (private$req_type == "intraday") {
              private$req_url <- paste(private$req_url,
                                       private$req_detail_level,
                                       sep = "/")
          }
          
          private$req_url <- paste("https://api.fitbit.com/1/user/-/",
                                   private$req_url,
                                   ".json",
                                   sep = "")    
          
          # Send the request
          self$response <- GET(url = private$req_url, config(token = private$api_token))
          warn_for_status(self$response)

      },
      
      ### FUNCTION Write
      ### 
      ### Writes the result to a JSON file
      
      write = function() {
          
          # Writes the response content into a JSON file
          if(private$req_type == 'day') {
              json_file <- paste("max",
                                 private$req_activity, 
                                 sep = "-")
          } else if (private$req_type == 'intraday') {
              json_file <- paste("intraday",
                                 private$req_activity, 
                                 private$detail_level,
                                 sep = "-")
          }
          
          json_file <- paste("./inst/extdata/tests/", json_file, ".json", sep = "")
          resp_content <- content(self$response, as = "text")
          write(resp_content, json_file)

      },
      
      ### FUNCTION Get
      # Stocks variables and calls other functions for the request
      
      get = function(type = "day", 
                     activity = "", 
                     start_date = Sys.Date(), 
                     end_date = "", 
                     detail_level = "") {
          
          # Stock variables
          private$req_type <- type
          private$req_activity <- activity
          private$req_start_date <- start_date
          private$req_end_date <- end_date
          private$req_detail_level <- detail_level
          
          # Call functions for authentication, request, and JSON writing
          self$connect()
          self$request()
          self$write()
          
      }
      
  )
                  
)

### Bulk requests using our DataLoader object

BulkRequest <- DataLoader$new()

bulk_activities <- list(
    "calories",
    "caloriesBMR",
    "steps",
    "distance",
    "floors",
    "elevation",
    "minutesSedentary",
    "minutesLightlyActive",
    "minutesFairlyActive",
    "minutesVeryActive",
    "activityCalories"
)

lapply(bulk_activities, function(x) {
    BulkRequest$get(type = 'day', activity = x, start_date = "2016-01-20", end_date = "2016-02-05")
}
    )


