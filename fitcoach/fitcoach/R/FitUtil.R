# ------------------------------------------------------------------------------
# Utility for 'fitcoach' package. Contains the various functions 
# that are used by R6 Classes in the package.
# ------------------------------------------------------------------------------


#' @export
getDailyResourcePathList <- function() {
  resourcePath <- list ("calories",
                        "caloriesBMR",
                        "steps",
                        "distance",
                        # "floors",
                        # "elevation",
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
                        # "floors",
                        # "elevation",
                        "distance")
  return (resourcePath)
}


#' Creates the Master Data Frame from Timeseries JSON files.
#'
#' @param tsFileFolder Folder containing all time-series files. Naming convention for files is max-[resource].json
#' @param resourcePath the resource paths to look. Default will get getDailyResourcePathList()
#' @return The Master Data Frame
#' @importFrom jsonlite fromJSON
#' 
#' @export

createTsMasterFrame <-
    function(tsFileFolder, resourcePath = getDailyResourcePathList()) {
        dflist <- lapply(resourcePath, function (x) {
            json_file <- paste(
                tsFileFolder,
                .Platform$file.sep,
                "max-", x, ".json",
                sep = ""
            )
            df <- as.data.frame(jsonlite::fromJSON(json_file, simplifyDataFrame = TRUE))
            colnames (df)[1] <- "date"
            colnames (df)[2] <- x
            return (df)
        })
        masterdf <- as.data.frame(dflist[1])
        
        for (i in 2:length(dflist)) {
            #FIX : used dplyr here
            masterdf <-
                merge(masterdf, as.data.frame(dflist[i]), by = "date")
        }
        
        masterdf$date <- as.Date(masterdf$date)
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
    master$date <- NULL
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
    masterTsDataFrame$weekday <-
        weekdays(as.Date(masterTsDataFrame$date))
    masterTsDataFrame$weekday <- as.factor(masterTsDataFrame$weekday)
    masterTsDataFrame$weekend <-
        ifelse(
            masterTsDataFrame$weekday == "Saturday" |
                masterTsDataFrame$weekday == "Sunday",
            TRUE,
            FALSE
        )
    return(masterTsDataFrame)
}

#' A function that incorporates rules for marking if the data entry in MasterTSFrame are valid or not
#'
#' @param masterTsDataFrame The Master Time Series data Frame
#' @return The marked Master Data Frame. i.e column valid is added at the end of the data.frame
#' 
#' @importFrom dplyr inner_join
#' @importFrom plyr ldply
#' @export

markValidRows <- function(masterTsDataFrame) {
    masterTsDataFrame$valid <-
        (as.numeric(masterTsDataFrame$distance) != 0)
    return(masterTsDataFrame)
}


# Create Intraday Frame
createIntraFrame <- function(folder) {
    files <- list.files(folder)
    indexes <- grep("intra-+", files)
    files <- files[indexes]
    
    #Calorie
    indexes <- grep(paste('-calories-', sep = ""), files)
    res.files <- files[indexes]
    res.files <- paste(folder, res.files, sep = "")
    dfList <- lapply (res.files,
                      function(x) {
                          d <- as.data.frame (fromJSON (x, simplifyDataFrame = TRUE))
                          d$sequence <- seq(1:nrow(d))
                          return(d)
                      })
    calorie.df <- plyr::ldply(dfList, data.frame)
    calorie.df <- calorie.df[-c(7, 8)]
    intraColNames <- c(
        "date",
        "calories",
        "intra.level",
        "intra.mets",
        "time",
        "intra.calorie",
        "timeseq"
    )
    colnames(calorie.df) <- intraColNames
    
    # Other resource types
    resources <- getIntradayResourcePathList()
    resources <- resources[-c(1)]
    for (i in 1:length(resources)) {
        resource.df <- fetchIntraResourceData(folder, resources[i], files)
        calorie.df <- dplyr::inner_join(calorie.df, resource.df)
    }
    return(calorie.df)
}

#' @export
fetchIntraResourceData <- function (folder, resource, files) {
    indexes <- grep(paste('-', resource, '-', sep = ""), files)
    res.files <- files[indexes]
    res.files <- paste(folder, res.files, sep = "")
    dfList <- lapply(res.files,
                     function(x) {
                         as.data.frame (fromJSON (x, simplifyDataFrame = TRUE))
                     })
    resource.df <- plyr::ldply(dfList, data.frame)
    resource.df <- resource.df[(-c(5, 6))]
    intraColNames <- c("date", resource, "time",
                       paste('intra.', resource, sep = ""))
    colnames(resource.df) <- intraColNames
    return (resource.df)
}


augmentIntraData <- function(inFrame) {
    inFrame$date <- as.Date(inFrame$date)
    inFrame$dataset.type <- NULL
    inFrame$time.interval <- NULL
    inFrame$weekday <- weekdays(inFrame$date)
    inFrame$weekday <- as.factor(inFrame$weekday)
    inFrame$weekend <- ifelse(inFrame$weekday == "Saturday" |
                                  inFrame$weekday == "Sunday",
                              TRUE,
                              FALSE)
    return(inFrame)
}


######
###### Utility functions for DataLoader R6 Class
######

#' Get API scope
#' 
#' Gets the scopes that will be retrieved by the API request to fitbit.
#' See https://dev.fitbit.com/docs/oauth2/#scope 

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

#' Connects to Fibit API 
#' 
#' Connects to the Fitbit API with OAuth 2. 
#' See https://dev.fitbit.com/docs/oauth2/
#' 
#' @param appname Name of the Fitbit App
#' @param key Fitbit API Client key
#' @param secret Fibit API Client secret
#' 
#' @importFrom httr oauth_endpoint oauth_app oauth2.0_token

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

#' Make API Request
#' 
#' Makes request to Fitbit API, and stores the response into a variable.
#' 
#' @param type Type of time series. Must be 'day' or 'intraday'
#' @param activity Type of activity. See below for details.
#' @param start_date Start date in format YYYY-mm-dd
#' @param end_date End date in format YYYY-mm-dd
#' @param api_token API token for connection to Fitbit API
#' 
#' @importFrom httr GET warn_for_status config

makeAPIRequest <-
    function(type, activity,
             start_date, end_date,
             api_token) {
        
        # Build URL for request
        req_url <- paste("activities",
                         activity,
                         "date",
                         start_date,
                         sep = "/")
        
        if (end_date != "") {
            req_url <- paste(req_url, end_date, sep = .Platform$file.sep)
        }
        
        if (type == "intraday") {
            if (end_date == "") req_url <- paste(req_url, "1d", sep = "/")
            req_url <- paste(req_url, "15min", sep = .Platform$file.sep)
        }
        
        req_url <- paste("https://api.fitbit.com/1/user/-/",
                         req_url,
                         ".json",
                         sep = "")
        # Debug only
        print(req_url)

        # Send the request
        response <- httr::GET(url = req_url, httr::config(token = api_token))
        httr::warn_for_status(response)
        return(response)
        
    }

#' Write to JSON
#' 
#' Writes API response content to JSON files, in a specific folder
#' 
#' @param content JSON content to be written to file
#' @param path Path to folder where files will be created
#' @param type Type of time series. Must be 'day' or 'intraday'
#' @param activity Type of activity. See below for details.
#' @param start_date Start date

writeToJSON <- function(content, path, type, activity, start_date) {
    
    if (!dir.exists(path)) { 
        dir.create(path)    
    }
    
    if (type == 'day') {
        json_file <- paste("max", activity, sep = "-")
    } else if (type == 'intraday') {
        json_file <- paste("intra", activity, start_date, sep = "-")
    }
    
    json_file <- paste(path, json_file, ".json", sep = "")
    write(content, json_file)
    
}

#' Build Chart
#' 
#' Plots charts that have been selected as most relevant.
#' 
#' @param data Dataframe
#' @param x_axis Name of the X-axis data. Default is 'date'.
#' @param y_axis Name of the Y-axis data. 
#' @param moving Number of observations to use for moving average. Default is 7.
#' 
#' @import ggplot2

buildChart <- function(data, x_axis, y_axis, moving = 7) {
    
    # Build graph
    graph <- ggplot(data = data, aes(x = data[[x_axis]])) +
        geom_area(aes(y = data[[y_axis]], fill = y_axis, color = y_axis), alpha = 0.5) +
        labs(title = "", x = x_axis, y = y_axis) + 
        scale_colour_manual(values = c("black", "red"), name = "") +
        guides(fill = "none")
    
    # Moving average if n > 7
    if ((moving > 0) & (length(data[[x_axis]]) > moving)) {
        data$average <- movingAvg(data[[y_axis]], n = moving)
        graph <- 
            graph +
            geom_line(aes(y = data$average, color = "moving average"), na.rm = TRUE)
    }
    
    plot(graph)
}

#' Moving average
#' 
#' Computes moving average for charts. 
#' 
#' @param data Vector of data
#' @param n Number of observations used for moving average. Default is 7.

movingAvg <- function(data, n) {
    stats::filter(data, rep(1/n, n), sides = 2)
}


