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
                        # Charles: needed to remove these two lines, as my
                        # app doesn't support floors and elevation...
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
                        # Same as above
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
#' @export
createTsMasterFrame <-
    function(tsFileFolder, resourcePath = getDailyResourcePathList()) {
        dflist <- lapply(resourcePath, function (x) {
            json.file <- paste(
                tsFileFolder,
                .Platform$file.sep,
                "max-", x, ".json",
                sep = ""
            )
            df <- as.data.frame(jsonlite::fromJSON(json.file, simplifyDataFrame = TRUE))
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


#' Create Intraday Frame
#' @importFrom jsonlite fromJSON
#' @export
createIntraFrame <- function(folder) {
    files <- list.files(folder)
    indexes <- grep("intra-+", files)
    files <- files[indexes]
    
    #Calorie
    indexes <- grep(paste('-calories-', sep = ""), files)
    res.files <- files[indexes]
    res.files <- paste(folder, "/", res.files, sep = "")
    
    dfList <- lapply (res.files,
                      function(x) {
                          d <- as.data.frame (jsonlite::fromJSON (x, simplifyDataFrame = TRUE, flatten = TRUE))
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

#' @importFrom jsonlite fromJSON
#' @export
fetchIntraResourceData <- function (folder, resource, files) {
    indexes <- grep(paste('-', resource, '-', sep = ""), files)
    res.files <- files[indexes]
    res.files <- paste(folder, "/", res.files, sep = "")
    dfList <- lapply(res.files,
                     function(x) {
                         as.data.frame (jsonlite::fromJSON (x, simplifyDataFrame = TRUE))
                     })
    resource.df <- plyr::ldply(dfList, data.frame)
    resource.df <- resource.df[(-c(5, 6))]
    intraColNames <- c("date", resource, "time",
                       paste('intra.', resource, sep = ""))
    colnames(resource.df) <- intraColNames
    return (resource.df)
}

#' @export
augmentIntraData <- function(inFrame) {
    inFrame$date <- as.Date(inFrame$date)
    inFrame$dataset.type <- NULL
    inFrame$time.interval <- NULL
    inFrame$weekday <- weekdays(inFrame$date)
    inFrame$weekday <- as.factor(inFrame$weekday)
    inFrame$weekend <- ifelse(inFrame$weekday == "Saturday" |
                                  inFrame$weekday == "Sunday",
                              1,
                              0)
    inFrame$calories <- as.numeric(inFrame$calories)
    inFrame$time <- NULL
    inFrame[, 2:9] <-
        lapply(2:9, function(x)
            as.numeric(inFrame[, x]))
    
    
    a <-
        cut(
            inFrame$timeseq,
            breaks = c(0, 23, 41, 77, 90, 96),
            labels = c("night", "morning", "day", "eve", "latenight")
        )
    inFrame$slot <- a
    #mod<- transform(df,  cumsum.calorie = ave(intra.calorie, date, slot, FUN=cumsum))
    mod <-
        transform(inFrame,  cumsum.calorie = ave(intra.calorie, date,  FUN = cumsum))
    mod <-
        transform(mod, cumsum.steps = ave(intra.steps, date, FUN = cumsum))
    mod <-
        transform(mod, cumsum.level = ave(intra.level, date, FUN = cumsum))
    mod <-
        transform(mod, cumsum.mets = ave(intra.mets, date, FUN = cumsum))
    mod <-
        transform(mod, cumsum.distance = ave(intra.distance, date, FUN = cumsum))
    #mod<- transform(mod, cumsum.floors = ave(intra.floors, date, FUN=cumsum))
    #mod<- transform(mod, cumsum.elevation = ave(intra.elevation, date, FUN=cumsum))
    inFrame <- mod
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
    fitbit.api <- httr::oauth_endpoint(
        request = "https://api.fitbit.com/oauth2/token",
        authorize = "https://www.fitbit.com/oauth2/authorize",
        access = "https://api.fitbit.com/oauth2/token")
    
    api.token <-
        httr::oauth2.0_token(
            endpoint = fitbit.api,
            app = httr::oauth_app(appname, key, secret),
            scope = getAPIScope(),
            use_basic_auth = TRUE,
            cache = TRUE
        )
        
    return(api.token)
}

#' Make API Request
#' 
#' Makes request to Fitbit API, and stores the response into a variable.
#' 
#' @param type Type of time series. Must be 'day' or 'intraday'
#' @param activity Type of activity. See below for details.
#' @param start.date Start date in format YYYY-mm-dd
#' @param end.date End date in format YYYY-mm-dd
#' @param api.token API token for connection to Fitbit API
#' 
#' @importFrom httr GET warn_for_status config

makeAPIRequest <-
    function(type, activity,
             start.date, end.date,
             api.token) {
        
        # Build URL for request
        req.url <- paste("activities",
                         activity,
                         "date",
                         start.date,
                         sep = "/")
        
        if (end.date != "") {
            req.url <- paste(req.url, end.date, sep = .Platform$file.sep)
        }
        
        if (type == "intraday") {
            if (end.date == "") req.url <- paste(req.url, "1d", sep = "/")
            req.url <- paste(req.url, "15min", sep = .Platform$file.sep)
        }
        
        req.url <- paste("https://api.fitbit.com/1/user/-/",
                         req.url,
                         ".json",
                         sep = "")

        # Send the request
        response <- httr::GET(url = req.url, httr::config(token = api.token))
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
#' @param start.date Start date

writeToJSON <- function(content, path, type, activity, start.date) {
    
    if (!dir.exists(path)) { 
        dir.create(path)    
    }
    
    if (type == 'day') {
        json.file <- paste("max", activity, sep = "-")
    } else if (type == 'intraday') {
        json.file <- paste("intra", activity, start.date, sep = "-")
    }
    
    json.file <- paste(path, json.file, ".json", sep = "")
    write(content, json.file)
    
}

#' Build Chart
#' 
#' Plots charts that have been selected as most relevant.
#' 
#' @param data Dataframe
#' @param x.axis Name of the X-axis data. Default is 'date'.
#' @param y.axes Name of the Y-axes data, as a vector of characters
#' 
#' @import ggplot2
#' @importFrom reshape2 melt

buildChart <- function(data, x.axis, y.axes) {
    
    # Keep only relevant columns and melt data
    data <- subset(data, select = c(x.axis, y.axes))
    data <- reshape2::melt(data, id.vars = x.axis)
    
    # Build graph
    graph <- 
        ggplot(data, aes(x = data[[x.axis]], y = data$value, color = data$variable)) +
        geom_line(na.rm = TRUE, alpha = 0.3) +
        geom_smooth(span = 0.1, se = FALSE) + 
        facet_grid(variable ~ ., scales = "free_y") +
        labs(title = "", x = x.axis, y = "", color = "Activity")
    plot(graph)
    
}
