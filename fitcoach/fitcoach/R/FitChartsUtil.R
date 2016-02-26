library(ggplot2)

# need to evolve this. This is just a place holder
#' @export
showCharts <- function(master , vars){
  print(vars)
  d <- qplot(x = master$datetime ,y = master$minutesLightlyActive )
  plot(d)
}


