#' R6 class for Analyzing Fitbit  Data
#'
#' FitAnalyzer is an R6 class for analyzing Fitbit data. It is an opinionated implementation of a particular workflow for analysis. 
#' For people attempting to conduct their own analysis in a different fashion you should use the more generic functions implemented in #' FitUtil. \cr \cr
#' The workflow implemented for FitAnalyzer is the following: \cr
#' 1.	Create the FitAnalyzer with the goal variable for analysis. Eg: Calories or steps or distance. The goal variable is your personal #' goal that you are trying to analyze better. \cr
#' 2.	Call \code{findImportantVariables} to understand the most important variables unique to you that enable meeting your goal. \cr
#' 3.	Call \code{showMostImportantCharts} to get relevant charts that are unique to your data \cr
#' 4.	Call \code{predictGoal} to get a prediction on performance of the goal \cr \cr
#' You can conduct two types of analysis based on the type of dataset in consideration. \code{analysis.type} can be 'intra.day' analysis or it can be 'daily' . 
#' 
#' @docType class
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' 
#' @importFrom R6 R6Class
#' @importFrom dplyr arrange select group_by summarise_each
#' @importFrom caret varImp
#' @importFrom gbm gbm predict.gbm gbm.perf relative.influence
#' @export FitAnalyzer
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{getAnalysisFrame(folder, analysis.type)}}{This method uses \code{folder} \code{analysis.type} as an argument to return a data.frame that is clean and augmented with additional features like weekend.}
#'   \item{\code{showMostImportantCharts(tsDataFrame)}}{This method plots charts for the most relevant goals, with actual data and moving average using geom_smooth().
#'   \cr \code{tsDataFrame}: a dataframe containing the fitibit activities.}
#' }
#' 


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
        findImportantVariables = function(tsDataFrame , seed = 12345) {
          set.seed(seed)
            if (!is.null(private$fit)){
                return (private$imp.vars)
            }

           ifelse (private$analysis.type == "intra.day",
                   private$createIntraFit (tsDataFrame),
                   private$createDailyFrameFit (tsDataFrame))

           return (private$imp.vars)
        },
        
        # Get fit
        getFit = function() {
          return (private$fit)
        },
        
        # Plot most important charts 
        showMostImportantCharts = function(tsDataFrame) {
            if (private$analysis.type == "intra.day") {
                tsDataFrame <- tsDataFrame %>% 
                    select(matches("timeseq|intra.")) %>% 
                    group_by(timeseq) %>% 
                    summarise_each(funs(mean))
                intra.vars <- names(sort(private$imp.vars, decreasing = TRUE))
                intra.vars <- intra.vars[grep('intra.', intra.vars)]
                buildChart(data = tsDataFrame, 
                           x.axis = "timeseq", 
                           y.axes = intra.vars[1:4])
            } else {
                buildChart(data = tsDataFrame, 
                           x.axis = "date", 
                           y.axes = unlist(private$imp.vars$name)[1:4])
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

            return (response)
        }
        
    ),
    
    # Private variables
    private = list(
        
        folder = NULL,
        goal = NULL,
        imp.vars = NULL,
        analysis.type  = NULL,
        fit = NULL,
        gbm.best.iter = NULL,
        
        createDailyFrameFit = function(master) {
            y <-
                createGoalVariableVector(master, goal = private$goal)
            x <-
                createDependentVariableFrame(master, goal = private$goal)
            glm.fit <-
                glm(y ~ ., data = x, family = "gaussian")
            imp <- caret::varImp(glm.fit, scale = TRUE)
            imp$name <- rownames(imp)
            imp <- dplyr::arrange(imp, -Overall)
            private$fit <- glm.fit
            private$imp.vars <- imp
        },
        
        createIntraFit = function(master) {
            master$date <- NULL
            gbm.fit <-
                gbm::gbm(
                    formula = calories ~ .,
                    data = master,
                    distribution = "gaussian",
                    n.trees = 500,
                    shrinkage = .05,
                    interaction.depth = 5,
                    bag.fraction = .5,
                    train.fraction = .8,
                    cv.folds = 3,
                    verbose = FALSE
                )
            private$fit <- gbm.fit
            private$gbm.best.iter <-
                gbm::gbm.perf(gbm.fit, method = "test", plot.it = FALSE)
            private$imp.vars <-
                gbm::relative.influence(gbm.fit, n.trees = 500, scale = TRUE)
            private$imp.vars <- sort(private$imp.vars , decreasing = TRUE)
        }
    )
)


