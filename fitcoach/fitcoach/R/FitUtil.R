library(httr)
library(jsonlite)

#-----------------------------------------------------------------------------------
# Utility for Fitbit coach package
#
# Contains the various functions that are used by R6 Classes in the package
#
#-----------------------------------------------------------------------------------


#' A function to fetch data from fitbit.com and store the data as json files
#' to a user defined file destination
#'
#' @param request_url the URL for GET call to fitbit.com
#' @param token the authorization token. Assumption that oauth2.0 has been done earlier in the flow
#' @param targetFile the place to store the json file
#' @return the targetfile
#' @import httr
#' @export
fetchAndStoreFile <- function(request_url , token , targetFile ){
  request <- GET(request_url, add_headers("Authorization"= token))
  bin <- content(request , as = "text")
  write(bin , targetFile)
  return(targetFile)
}

getResourcePathList <- function(){
  resourcePath <- list(
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
  return (resourcePath)
}

#' A function to create the Master Data Frame from Timeseries JSON files.
#'
#' @param tsFileFolder Folder containing all time-series files. Naming convention for files is max-[resource].json
#' @param resourcePath the resource paths to look. Default will get getResourcePathList()
#' @return The Master Data Frame
#' @import jsonlite
#' @export

createTsMasterFrame <- function(tsFileFolder, resourcePath = getResourcePathList()){
  dflist <- lapply(resourcePath, function(x){
    df <- as.data.frame(fromJSON(paste(tsFileFolder , "max-" , x ,".json" , sep = "") , simplifyDataFrame = TRUE));
    colnames(df)[1] <- "datetime";
    colnames(df)[2] <- x;
    return(df)
  })
  masterdf <- as.data.frame(dflist[1])
  for(i in 2:length(dflist)){
    masterdf <- merge(masterdf , as.data.frame(dflist[i]) , by = "datetime")
  }
  return(masterdf)
}

#' A function that incorporates rules for marking if the data entry in MasterTSFrame are valid or not
#'
#' @param masterTsDataFrame The Master Time Series data Frame
#' @return The marked Master Data Frame. i.e column valid is added at the end of the data.frame
#' @export

markValidRows <- function(masterTsDataFrame){
  masterTsDataFrame$valid <- (as.numeric(masterTsDataFrame$distance) != 0)
  return(masterTsDataFrame)
}


