#' Function to show charts that have been selected as most relevant
#' 
#' @param master Description to be done
#' @param vars Description to be done
#' 
#' @import ggplot2
#' @export

showCharts <- function(master, vars) {
    print(vars)
    d <- ggplot2::qplot(x = master$datetime,
               y = master$minutesLightlyActive)
    plot(d)
}


