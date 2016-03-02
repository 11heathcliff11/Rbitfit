#' Plots charts that have been selected as most relevant
#' 
#' @param data Dataframe
#' @param x_axis Name of the X-axis data. Default is 'date'.
#' @param y_axis Name of the Y-axis data. 
#' 
#' @import ggplot2
#' @export

showCharts <- function(data, x_axis = "date", y_axis) {
    
    # Moving average
    data$Average <- movingAvg(data[[y_axis]]) 
    
    # Build graph
    graph <- ggplot(data = data, aes(x = data[[x_axis]])) +
        geom_area(aes(y = data[[y_axis]], fill = y_axis, color = y_axis), alpha = 0.5) +
        geom_line(aes(y = Average, color = "moving average"), na.rm = TRUE) +
        labs(title = "", x = x_axis, y = y_axis) + 
        scale_colour_manual(values = c("black", "red"), name = "") +
        guides(fill = "none")

    plot(graph)
}

#' Computes moving average for charts
#' 
#' @param data Vector of data
#' @param n Number of observations used for moving average. Default is 7.

movingAvg <- function(data, n = 7) {
    stats::filter(data, rep(1/n, n), sides = 2)
}



