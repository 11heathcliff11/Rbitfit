#' R6 class for Analyzing Fitbit  Data
#'
#' This is a R6 class for Analyzing Fitbit data.
#' Class is useful one you have the fitbit json files loaded in a folder as a starting point
#'
#'
#' @docType class
#' @import R6
#' @import jsonlite
#' @import stats
#' @import caret
#' @import dplyr
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export FitAnalyzer
#' @section Methods:
#' \describe{
#'  \item{\code{getTsDailyFrame(ts.daily.json.folder) }{This method uses \code{ts.daily.json.folder} as an
#'   argument to return a data.frame that is clean and augmented with additional features like weekend.}
#' }
#'
#'
#library(R6)
#library(jsonlite)
#library(glmnet)
#source(file = "FitUtil.R")
#source(file = "FitChartsUtil.R")
#library(stats)
#library(caret)
#library(dplyr)
FitAnalyzer <- R6Class("FitAnalyzer",
                       public = list(
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
                         getTsIntradayFrame = function(intra.day.folder = NA){
                           cat ("to be implemented")
                         },
                         setGoal = function(goal){
                           private$goal <- goal
                         },
                         getGoal = function(){
                           return(private$goal)
                         },
                         findImportantVariables = function(goal = getGoal() , tsDataFrame){
                          y <- createGoalVariableVector(master = tsDataFrame , goal = goal)
                          x <- createDependentVariableFrame(master = tsDataFrame , goal = goal)
                          x <- as.matrix(x)
                          fit <- glm(y~x , family = "gaussian")
                          imp<- varImp(fit , scale = FALSE)
                          imp$name <- rownames(imp)
                          imp <- arrange(imp , -Overall)
                          private$imp.vars = imp
                          return(imp)
                         },
                         showMostImportantCharts = function(tsDataFrame){
                           showCharts(tsDataFrame , c("minutesLightlyActive"))
                         },
                         recommendationsToday = function(){
                           cat ("to be implemented")
                         }
                       ),
                       private = list(
                         ts.daily.json.folder = NA,
                         goal = "calorie",
                         imp.vars = NA
                       )
)


