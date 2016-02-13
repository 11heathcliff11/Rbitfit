library(httr)

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

