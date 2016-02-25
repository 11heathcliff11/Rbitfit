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


DataLoader <- R6Class("DataLoader",
                      
  public = list (

      # Request response
      response = NA,
      
      # API Token
      api_token = NA,

      ### FUNCTION Initialize

      initialize = function() {
          print("Object DataLoader initialized")
      },

      ### FUNCTION Connect
      ###
      ### API Connection
      connect = function(appname, key, secret) {

          self$api_token <- connectToAPI(appname, key, secret)

      },

      ### FUNCTION Request
      ###
      ### Build URL, send request and write response to JSON file
      request = function(type = "day",
                         activity = "",
                         start_date = Sys.Date(),
                         end_date = "",
                         detail_level = "15min",
                         path = "./json/"
                         ) {
          
          self$response <- makeAPIRequest(type = type, 
                                          activity = activity, 
                                          start_date = start_date, 
                                          end_date = end_date, 
                                          detail_level = detail_level, 
                                          path = path, 
                                          api_token = self$api_token
                                          )

      },

      
      write = function() {
          
          # # Writes the response content into a JSON file
          # if(private$req_type == 'day') {
          #     json_file <- paste("max",
          #                        private$req_activity,
          #                        sep = "-")
          # } else if (private$req_type == 'intraday') {
          #     json_file <- paste("intraday",
          #                        private$req_activity,
          #                        private$detail_level,
          #                        sep = "-")
          # }
          # 
          # json_file <- paste(self$path, json_file, ".json", sep = "")
          # resp_content <- content(self$response, as = "text")
          # write(resp_content, json_file)
          
      }

  )

)


