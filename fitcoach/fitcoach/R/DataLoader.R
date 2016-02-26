#' R6 class for Loading fitbot data
#'
#'
#'
#' @docType class
#' @import R6
#' @import httr
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export DataLoader
#'
#'


### Load Libraries
#library('R6')
#library('httr')
DataLoader <- R6Class("DataLoader",

  public = list (

      ### 
      ### Public variables
      ### 
     
      api_token = NA, # API Token
      response = NA, # Request response

      ### 
      ### FUNCTION Initialize
      ### Standard R6 Initialize function
      ###

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

      ### 
      ### FUNCTION Connect
      ### Connects to the API with credentials
      ### 
      
      connect = function(appname, key, secret) {

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

      ###
      ### FUNCTION Request
      ### Build URL, send request and write response to JSON file
      ### 
      
      request = function(type = "day",
                         activity = "",
                         start_date = Sys.Date(),
                         end_date = "",
                         path = "./json/"
                         ) {
          
          self$response <- makeAPIRequest(type = type, 
                                          activity = activity, 
                                          start_date = start_date, 
                                          end_date = end_date, 
                                          path = path, 
                                          api_token = self$api_token
                                          )

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

