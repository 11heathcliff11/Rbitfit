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
#' @importFrom R6 R6Class
#' @importFrom dplyr arrange
#' @importFrom caret varImp
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' @export FitAnalyzer
#' 
#' @section Methods:
#' \code{getAnalysisFrame(folder, analysis.type)}
#' This method uses \code{folder} \code{analysis.type} as an argument to return a data.frame that is clean and augmented with additional features like weekend.

FitAnalyzer <- R6::R6Class(
    "FitAnalyzer",
    
    public = list(
        
        initialize = function(goal = "calories") {
            private$goal <- goal
        },
        
        # Get Analysis frame
        getAnalysisFrame = function(folder = NA, analysis.type) {
            
            private$folder <- folder
            private$analysis.type <- analysis.type
            master <- NULL
            
            if (analysis.type == "intra.day") {
              master <-
                    createIntraFrame(folder)
              master <-
                    augmentIntraData(master)
            } else {
                master <- 
                    createTsMasterFrame(folder)
                master <- 
                    markValidRows(master)
                master <-
                    master[master$valid == TRUE, ]
                master <- augmentData(master)
            }
            return (master)
        },
        
        # Find important variables
        findImportantVariables = function(tsDataFrame) {
            if(!is.na (private$fit)){
              return (private$imp.vars)
            }

           ifelse (private$analysis.type == "intra.day",
                   private$createIntraFit (tsDataFrame),
                   private$createDailyFrameFit (tsDataFrame))

           return (private$imp.vars)
        },
        getFit = function(){
          return (private$fit)
        },
        
        # Choose most important charts 
        # Charles - is this method required . Can we get rid of this and use showCharts only
        showMostImportantCharts = function(tsDataFrame) {
            if (private$analysis.type == "intra.day") {
                cat("To be implemented")
            } else {
                self$showCharts(tsDataFrame, c("minutesLightlyActive"))
            }
        },
        
        # Predict goals
        predictGoal = function(x) {
            response <- NULL
            response <- 
                  ifelse (private$analysis.type == "intra.day",
                          gbm::predict.gbm(private$fit, newdata = x,
                                           n.trees = private$gbm.best.iter),
                          predict.glm(private$fit, 
                                      newdata = as.data.frame (x), 
                                      type = "response"))

            return(response)
        },
        
        # Plot a chart
        showCharts = function(data, activity.1, activity.2, average = 7) {
            buildChart(data = data, x.axis = "date", 
                       y.axis.1 = activity.1, y.axis.2 = activity.2, 
                       moving = average)
        }
        
    ),
    
    # Private variables
    private = list(
        folder = NA,
        goal = NA,
        imp.vars = NA,
        analysis.type  = NA,
        fit = NA,
        gbm.best.iter = NA,
        createDailyFrameFit = function(master){
          y <-
            createGoalVariableVector(master, goal = private$goal)
          x <-
            createDependentVariableFrame(master, goal = private$goal)
          glm.fit <-
            glm(y ~ ., data = x, family = "gaussian")
          imp <- caret::varImp(glm.fit, scale = FALSE)
          imp$name <- rownames(imp)
          imp <- dplyr::arrange(imp,-Overall)
          private$fit <- glm.fit
          private$imp.vars <- imp
        },
        createIntraFit = function(master){
          master$date <- NULL
          gbm.fit <- gbm::gbm(formula = calories~., data = master, distribution = "gaussian", n.trees = 500,
                          shrinkage = .05, interaction.depth = 5, bag.fraction = .5, train.fraction = .8,
                          cv.folds = 3, verbose = FALSE)
          private$fit <- gbm.fit
          private$gbm.best.iter <- gbm::gbm.perf(gbm.fit,method="test")
          private$imp.vars <-  relative.influence(gbm.fit, n.trees = 500, scale = TRUE)
        }
    )
)


