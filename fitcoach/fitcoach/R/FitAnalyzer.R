#' R6 class for Analyzing Fitbit  Data
#'
#' FitAnalyzer is an R6 class for analyzing Fitbit data. It is an opinionated implementation of a particular workflow for analysis. 
#' For people attempting to conduct their own analysis in a different fashion you should use the more generic functions implemented in #' FitUtil.
#' The workflow implemented for FitAnalyzer is the following
#' 1.	Create the FitAnalyzer with the goal variable for analysis. Eg: Calories or steps or distance. The goal variable is your personal #' goal that you are trying to analyze better.
#' 2.	Call \code{findImportantVariables} to understand the most important variables unique to you that enable meeting your goal. 
#' 3.	Call \code{showMostImportantCharts} to get relevant charts that are unique to your data
#' 4.	Call \code{predictGoal} to get a prediction on performance of the goal
#' You can conduct two types of analysis based on the type of dataset in consideration. \code{analysis.type} can be 'intra.day'
#' analysis or it can be 'daily' . 
#' 
#' @docType class
#' @import R6
#' @import jsonlite
#' @import caret
#' @import dplyr
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export FitAnalyzer
#' @section Methods:
#' \describe{
#'  \item{\code{getAnalysisFrame(folder , analysis.type) }{This method uses \code{folder} \code{analysis.type} as an
#'   argument to return a data.frame that is clean and augmented with additional features like weekend.}
#' }
#'
#'
#library(R6)
#library(jsonlite)
#source(file = "FitUtil.R")
#source(file = "FitChartsUtil.R")
#library(caret)
#library(dplyr)
FitAnalyzer <- R6Class("FitAnalyzer",
                       public = list(
                         initialize = function(goal = "calories"){
                           private$goal <- goal
                         },
                         getAnalysisFrame = function(folder = NA , analysis.type){
                           private$folder <- folder
                           private$analysis.type <- analysis.type
                           master <- NULL
                           if(analysis.type == "intra.day"){ #FIX : try to use switch here
                             master <- createIntraFrame(folder)                             
                           }else{
                             master <- createTsMasterFrame(folder)
                             master <- markValidRows(master)
                             master <- master[master$valid == TRUE ,]
                             master <- augmentData(master)
                           }
                           return (master)
                         },
                         findImportantVariables = function(tsDataFrame){
                           imp <- NULL
                           if(private$analysis.type == "intra.day"){ #FIX : try to use switch here
                             cat(" To be implemented")
                          }else{
                            y <- createGoalVariableVector(master = tsDataFrame , goal = private$goal)
                            x <- createDependentVariableFrame(master = tsDataFrame , goal = private$goal)
                            glm.fit <- glm(y~. , data = x, family = "gaussian")
                            imp<- varImp(glm.fit , scale = FALSE)
                            imp$name <- rownames(imp)
                            imp <- arrange(imp , -Overall)
                            private$imp.vars = imp
                            private$fit <- glm.fit
                          }
                           return(imp)
                         },
                         showMostImportantCharts = function(tsDataFrame){
                           if(private$analysis.type == "intra.day"){
                             cat(" To be implemented")
                           }else{
                             showCharts(tsDataFrame , c("minutesLightlyActive"))
                           }
                         },
                         predictGoal = function(x){
                           response <- NULL
                           if(private$analysis.type == "intra.day"){
                             cat(" To be implemented")
                           }else{
                             response <- predict.glm(private$fit , newdata = as.data.frame(x) , type = "response")
                           }
                           return(response)
                         }
                       ),
                       private = list(
                         folder = NA,
                         goal = NA,
                         imp.vars = NA ,
                         analysis.type  = NA,
                         fit = NA
                       )
)


