#' Function to show charts that have been selected as most relevant
#' 
#' @param master Description to be done
#' @param vars Description to be done
#' 
#' @import ggplot2
#' @export

showCharts <- function(data, x_axis = "date", y_axis) {
    
    # FIX: need to remove warnings for missing values
    data$Average <- movingAvg(data[[y_axis]]) 
    
    graph <- ggplot(data = data, aes(x = data[[x_axis]])) +
        geom_area(aes(y = data[[y_axis]]), alpha = 0.5) +
        geom_line(aes(y = Average))

    plot(graph)
}

movingAvg <- function(data, n = 7) {
    stats::filter(data, rep(1/n, n), sides = 2)
}



