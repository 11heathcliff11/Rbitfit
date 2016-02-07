###
### API Access
###

library('httr')

# Setup API connection

apiConnection <- function(appname = "cdlr", key = "227FWR", secret = "3089e3d1ac5dde1aa00b54a0c8661f42") {
    
    # Set up credentials
    fitbit_endpoint <- oauth_endpoint(
        request = "https://api.fitbit.com/oauth2/token",
        authorize = "https://www.fitbit.com/oauth2/authorize",
        access = "https://api.fitbit.com/oauth2/token")
    
    myapp <- oauth_app(appname, key, secret)
    
    # Get OAuth token
    # Scope: see dev.fitbit.com/docs/oauth2/#scope
    scope <- c("activity", "heartrate", "location", "nutrition", "profile", "settings", "sleep", "social", "weight")
    fitbit_token <<- oauth2.0_token(fitbit_endpoint, 
                                    myapp,
                                    scope = scope, 
                                    use_basic_auth = TRUE)

}



# Makes API request and writes JSON files (1 per day of data)
getApiContent <- function(date = "2016-02-03", json_file = "daily-summary.json") {
    
    # Build URL with date for request (daily activity summary)
    # Cf. https://dev.fitbit.com/docs/activity/#get-daily-activity-summary
    get_url <- paste("https://api.fitbit.com/1/user/-/activities/date/", 
                     date, 
                     ".json", 
                     sep = "")

    # Send the request
    response <- GET(url = get_url, config(token = fitbit_token))

    # Writes the response content into a JSON file
    resp_content <- content(response, as = "text")
    writeBin(resp_content, json_file)
    
}

# Function calls
apiConnection()
getApiContent()

###
### Convert JSON to Data Frame
### 

library('rjson')
library('jsonlite')
# library('data.table')

json_file <- "daily-summary.json"
json_raw <- rjson::fromJSON(file = json_file)

# Conversion to simple vector
json_vector <- unlist(json_raw)

# Conversion to data frame with data.table package: does not work 
# json_df <- data.table::rbindlist(json_raw, fill = TRUE)


###
###
