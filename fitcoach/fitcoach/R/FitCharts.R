#' R6 class for displaying the relevant charts
#'
#' @docType class
#' @import R6
#' @format A \code{\link{R6Class}} generator object
#' @keywords data
#' 
#' @export DataLoader

MakeChart <- R6Class(
    "MakeChart",
    
    public = list (
        
        ###
        ### FUNCTION initialize
        ### Standard R6 Initialize function
        ###

        initialize = function() {
            message("Object MakeChart initialized")
        },
        
        ###
        ### FUNCTION showCharts
        ### Builds the relevant charts, with the master data frame
        ###

        showCharts = function(data, activity) {
            buildChart(data = data, x_axis = "date", y_axis = activity)
        }
        
    )
)