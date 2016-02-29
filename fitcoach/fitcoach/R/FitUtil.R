# -----------------------------------------------------------------------------------
# Utility for Fitbit coach package Contains the various functions that are used
# by R6 Classes in the package
# -----------------------------------------------------------------------------------

# 
# #' A function to fetch data from fitbit.com and store the data as json files
# #' to a user defined file destination
# #'
# #' @param request_url the URL for GET call to fitbit.com
# #' @param token the authorization token. Assumption that oauth2.0 has been done earlier in the flow
# #' @param targetFile the place to store the json file
# #' @return the targetfile
# #' @import httr
# #' @export
# fetchAndStoreFile <- function(request_url, token, targetFile) {
#   request <- GET(request_url, add_headers(Authorization = token))
#   bin <- content(request, as = "text")
#   write(bin, targetFile)
#   return(targetFile)
# }

#' @export
getDailyResourcePathList <- function() {
  resourcePath <- list ("calories",
                        "caloriesBMR",
                        "steps",
                        "distance",
                        "floors",
                        "elevation",
                        "minutesSedentary",
                        "minutesLightlyActive",
                        "minutesFairlyActive",
                        "minutesVeryActive",
                        "activityCalories")
    return (resourcePath)
}

#' @export
getIntradayResourcePathList <- function() {
  resourcePath <- list ("calories",
                        "steps",
                        "distance",
                        "floors",
                        "elevation")
  return (resourcePath)
}


#' A function to create the Master Data Frame from Timeseries JSON files.
#'
#' @param tsFileFolder Folder containing all time-series files. Naming convention for files is max-[resource].json
#' @param resourcePath the resource paths to look. Default will get getResourcePathList()
#' @return The Master Data Frame
#' @import jsonlite
#' @export

createTsMasterFrame <- function (tsFileFolder, resourcePath = getResourcePathList ()) {
  dflist <- lapply (resourcePath, function (x) {
    df <- as.data.frame (fromJSON (paste(tsFileFolder, .Platform$file.sep, "max-",
                          x, ".json", sep = ""), simplifyDataFrame = TRUE))
    colnames (df)[1] <- "datetime"
    colnames (df)[2] <- x
    return (df)
  })
  masterdf <- as.data.frame (dflist[1])

  for (i in 2:length (dflist)) {
    masterdf <- merge(masterdf, as.data.frame(dflist[i]), by = "datetime")
  }

  masterdf$datetime <- as.Date(masterdf$datetime)
  lapply(2:ncol(masterdf), function(x) {
    masterdf[, x] <<- as.numeric(masterdf[, x])
  })
  return(masterdf)
}

#' @export
createGoalVariableVector <- function(master, goal) {
  y <- eval(parse(text = paste("master$", goal, sep = "")))
}

#' @export
createDependentVariableFrame <- function(master, goal) {
  master$datetime <- NULL
  # remove variables out of individuals direct control : eg calories
  master$calories <- NULL
  master$caloriesBMR <- NULL
  master$activityCalories <- NULL
  master$valid <- NULL
  master$holiday <- ifelse(master$weekend, 1, 0)
  master$weekday <- NULL
  master$weekend <- NULL
  eval(parse(text = paste("master$", goal, " <- NULL", sep = "")))
  return(master)
}

#' @export
augmentData <- function(masterTsDataFrame) {
  ## augment weekday information
  masterTsDataFrame$weekday <- weekdays(as.Date(masterTsDataFrame$datetime))
  masterTsDataFrame$weekday <- as.factor(masterTsDataFrame$weekday)
  masterTsDataFrame$weekend <- ifelse(masterTsDataFrame$weekday == "Saturday" |
    masterTsDataFrame$weekday == "Sunday", TRUE, FALSE)
  return(masterTsDataFrame)

}

#' A function that incorporates rules for marking if the data entry in MasterTSFrame are valid or not
#'
#' @param masterTsDataFrame The Master Time Series data Frame
#' @return The marked Master Data Frame. i.e column valid is added at the end of the data.frame
#' @export

markValidRows <- function(masterTsDataFrame) {
  masterTsDataFrame$valid <- (as.numeric(masterTsDataFrame$distance) != 0)
  return(masterTsDataFrame)
}


createIntraFrame <- function(resourceType , folder){
  files <- list.files(folder)
  indexes <- grep("intra-+" , files)
  files <- files[indexes]
  indexes <- grep(paste('-' , resourceType , '-' , sep ="" ) , files)
  res.files <- files[indexes]
  res.files <- paste(intraTsFolder , res.files , sep = "")
  
  dfList <- lapply(res.files , function(x) {as.data.frame(fromJSON(x , simplifyDataFrame = TRUE ))   })
  
  res.df <- as.data.frame(dfList[1])
  
  for (i in 2:length(dfList)) {
    res.df <- rbind(res.df , as.data.frame(dfList[i]))
  }
  
  res.df$resource <- resourceType
  if(resourceType != "calories"){
    level <- rep(0 , nrow(res.df))
    mets <- rep(NA , nrow(res.df))
    res.df <- cbind(res.df[,1:2] , mets , level , res.df[,3:7])
  }
  intraColNames <- c("date",
                     "total.value",
                     "level",
                     "mets",
                     "time",
                     "intra.value",
                     "time.interval",
                     "dataset.type" ,
                     "resource")
  
  colnames(res.df) <- intraColNames
  return(res.df)
}


augmentIntraData <- function(inFrame){
  inFrame$date <- as.Date(inFrame$date)
  inFrame$dataset.type <- NULL
  inFrame$time.interval <- NULL
  inFrame$weekday <- weekdays(inFrame$date)
  inFrame$weekday <- as.factor(inFrame$weekday)
  inFrame$weekend <- ifelse(inFrame$weekday == "Saturday" |
                              inFrame$weekday == "Sunday", TRUE, FALSE)
  return(inFrame)
}


######
###### Utility functions for DataLoader R6 Class
######

#' Gets the scopes that will be retrieved by the API request

getAPIScope <- function() {
    APIScope <- c(
        "activity", 
        "heartrate", 
        "location",
        "nutrition",
        "profile", 
        "settings",
        "sleep", 
        "social", 
        "weight"
    )
    return (APIScope)
}

#' Connects to Fibit API with OAuth 2
#' 
#' @param appname Name of the Fitbit App
#' @param key Fitbit API Client key
#' @param secret Fibit API Client secret
#' 
#' @importFrom httr oauth_endpoint oauth_app

connectToAPI <- function(appname, key, secret) {
    fitbit_api <- httr::oauth_endpoint(
        request = "https://api.fitbit.com/oauth2/token",
        authorize = "https://www.fitbit.com/oauth2/authorize",
        access = "https://api.fitbit.com/oauth2/token")
    
    api_token <-
        httr::oauth2.0_token(
            endpoint = fitbit_api,
            app = httr::oauth_app(appname, key, secret),
            scope = getAPIScope(),
            use_basic_auth = TRUE,
            cache = TRUE
        )
        
    return(api_token)
}

#' Make request to Fitbit API
#' 
#' @param type Type of time series. Must be 'day' or 'intraday'
#' @param activity Type of activity. See below for details.
#' @param start_date Start date in format YYYY-mm-dd
#' @param end_date End date in format YYYY-mm-dd
#' @param api_token API token for connection to Fitbit API

makeAPIRequest <-
    function(type,
             activity,
             start_date,
             end_date,
             api_token) {
        
        # Build URL for request
        req_url <- paste("activities",
                         activity,
                         "date",
                         start_date,
                         sep = "/")
        
        if (end_date != "") {
            req_url <- paste(req_url, end_date, sep = "/")
        }
        
        if (type == "intraday") {
            if (end_date == "") req_url <- paste(req_url, "1d", sep = "/")
            req_url <- paste(req_url, "15min", sep = "/")
        }
        
        req_url <- paste("https://api.fitbit.com/1/user/-/",
                         req_url,
                         ".json",
                         sep = "")
        # Debug only
        print(req_url)

        # Send the request
        response <- GET(url = req_url, config(token = api_token))
        warn_for_status(response)
        return(response)
        
    }

#' Writes API response content to JSON files
#' 
#' @param content JSON content to be written to file
#' @param path Path to folder where files will be created
#' @param type Type of time series. Must be 'day' or 'intraday'
#' @param activity Type of activity. See below for details.
#' @param start_date Start date

writeToJSON <- function(content, path, type, activity, start_date) {
    
    if (type == 'day') {
        json_file <- paste("max", activity, sep = "-")
    } else if (type == 'intraday') {
        json_file <- paste("intra", activity, start_date, sep = "-")
    }
    
    json_file <- paste(path, json_file, ".json", sep = "")
    write(content, json_file)
    
}


