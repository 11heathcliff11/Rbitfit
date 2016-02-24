#' R6 class for Analyzing Fitbit  Data
#'
#' This is a R6 class for Analyzing Fitbit data.
#' Class is useful one you have the fitbit json files loaded in a folder as a starting point
#'
#'
#' @docType class
#' @import R6
#' @import jsonlite
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export FitAnalyzer
#' @section Methods:
#' \describe{
#'  \item{\code{analyze.ts.daily(ts.daily.json.folder)}}{This method uses \code{trip} as an argument to return a plot of 4 features (X-Y coordinates, velocity, acceleration, breaks).}
#' }

 library(R6)
 library(jsonlite)
 library(glmnet)
 source(file = "FitUtil.R")

FitAnalyzer <- R6Class("FitAnalyzer" ,
                        public = list(
                          dummy <- NA,
                          initialize = function(){
                            cat("init called")
                          },
                         getTsDailyFrame = function(ts.daily.json.folder = NA){
                           private$ts.daily.json.folder <- ts.daily.json.folder
                           master <- createTsMasterFrame(ts.daily.json.folder)
                           master <- markValidRows(master)
                           master <- master[master$valid == TRUE ,]
                           master <- augmentData(master)
                           return(master)
                         },
                         setGoal = function(goal){
                           private$goal <- goal
                         },
                         getGoal = function(){
                           return(private$goal)
                         }
                         ),
                        private = list(
                           ts.daily.json.folder = NA,
                          goal = NA
                        )
                    )
