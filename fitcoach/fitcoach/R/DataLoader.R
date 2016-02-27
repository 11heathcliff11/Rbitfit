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
DataLoader <- R6Class(
    "DataLoader",
    
    public = list (
        
        ###
        ### Public variables
        ###
        
        # API Token
        api_token = NA,
        # Request response
        response = NA,
        
        ###
        ### FUNCTION initialize
        ### Standard R6 Initialize function
        ###
        
        initialize = function() {
            print("Object DataLoader initialized")
        },
        
        ###
        ### FUNCTION connect
        ### Connects to the API with credentials
        ###
        
        connect = function(appname, key, secret) {
            if (file.exists('.httr-oauth')) {
                print('Use existing Oauth file') # Debug only
                self$api_token <- readRDS('.httr-oauth')[[1]]
            } else {
                print('Create new Oauth file') # Debug only
                self$api_token <- connectToAPI(appname, key, secret)
            }
        },
        
        ###
        ### FUNCTION requestAndWrite
        ### Build URL, send request and write response to JSON file
        ###
        
        requestAndWrite = function(type = "day",
                           activities = "",
                           start_date = Sys.Date(),
                           end_date = "",
                           path = "./json/") {
         
            # Check 'type' argument
            if (!(type %in% c("day", "intraday")))
                stop("Invalid 'req_type'. Must be 'day' or 'intraday'")
            
            # Check 'start_date' argument
            if (!(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", start_date)))
                stop("Invalid 'start_date'. Must be in the following format: 'YYYY-MM-dd'")
            
            # Call request function for each activity
            for (acty in activities) {

                # Debug only
                print(paste("Making request for ", acty, sep = ""))
                
                self$response <- makeAPIRequest(
                    type = type,
                    activity = acty,
                    start_date = start_date,
                    end_date = end_date,
                    api_token = self$api_token
                )
                
                writeToJSON(content = content(self$response, as = "text"),
                            path = path,
                            type = type, 
                            activity = acty)
                
            }
        }
    )
    
)

