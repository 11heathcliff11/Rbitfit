
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
      
      # JSON extract
      json_list = NA,
      
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
          
          fitbit_endpoint <- httr::oauth_endpoint(
              request = private$api_url_request,
              authorize = private$api_url_authorize,
              access = private$api_url_request
          )
          
          # Get OAuth token
          private$api_token <-
              httr::oauth2.0_token(
                  fitbit_endpoint,
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
          if(!(private$req_type %in% c("summary", "time", "intraday"))) {
              stop("Invalid type of data. Must be 'summary', 'time' or 'intraday'")
          }
          
          ## /!\ TO-DO: check all arguments formats
          ## 
          ## 
          ## 
          
          # Build URL for request 
          if (private$req_type == "summary") {
              private$req_url <- paste("activities/date", 
                                       private$req_start_date,
                                       sep = "/")
              
          } else if (private$req_type %in% c("time", "intraday")) {
              private$req_url <- paste("activities", 
                                       private$req_activity, 
                                       "date",
                                       private$req_start_date,
                                       private$req_end_date,
                                       sep = "/")
          }
          
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
          if(debug == TRUE) print(private$req_url)
          
      },
      
      ### FUNCTION Write
      ### 
      ### Writes the result to a JSON file
      
      write = function(debug = FALSE) {
          
          # Writes the response content into a JSON file
          json_file <- paste(private$req_type, 
                             private$req_activity, 
                             private$req_start_date, 
                             private$req_end_date, 
                             private$req_detail_level, 
                             sep = "_")
          json_file <- paste("./inst/extdata/tests/", json_file, ".json", sep = "")
          resp_content <- content(self$response, as = "text")
          write(resp_content, json_file)
          if(debug == TRUE) print(json_file)
          
      },
      
      ### FUNCTION Get
      # Stocks variables and calls other functions for the request
      
      get = function(type = "summary", 
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
          self$request(debug = TRUE)
          self$write(debug = TRUE)
          
      },
      
      ### FUNCTION Read to DF
      # Reads JSON files and convert them to Dataframes
      
      readToDF = function(file, path) {
          
          file_path <- paste(path, file, sep = "")
          self$json_list <- jsonlite::fromJSON(file_path, 
                                         flatten = TRUE, 
                                         simplifyDataFrame = TRUE)

          ## /!\ 2DO: convert to Data frames, depending on the type of data
          
          self$json_list
          
      }
       
  )
                  
)

### Bulk requests using our DataLoader object

BulkRequest <- DataLoader$new()

for(i in paste("2016-02-", c("01", "02", "03", "04", "05"), sep = "")) {
    BulkRequest$get(type = 'summary', start_date = i)
    BulkRequest$get(type = 'time', activity = 'steps', end_date = "7d", start_date = i)
    BulkRequest$get(type = 'intraday', activity = 'steps', start_date = i, end_date = "1d", detail_level = "15min")
    BulkRequest$get(type = 'intraday', activity = 'steps', start_date = i, end_date = "1d", detail_level = "1min")
}

### Read JSON files and convert them to Data Frames

file_name <- "intraday_steps_2016-02-01_1d_15min.json"
file_path <- "./inst/extdata/tests/"
json_raw <- BulkRequest$readToDF(file = file_name, path = file_path)
# WIP



