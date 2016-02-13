library(httr)
library(jsonlite)
#daily file load

help(jsonlite)

jsonFile <- 'C:/Users/webscale/Documents/GitHub/Rbitfit/fitcoach/fitcoach/inst/extdata/daily'
jsonFileTimeSeries <- 'C:/Users/webscale/Documents/GitHub/Rbitfit/fitcoach/fitcoach/inst/extdata/daily-time-series'


json <- stream_in(con, handler = NULL, pagesize = 500, verbose = TRUE, ...)

mydata <- stream_in(file("C:/Users/webscale/Documents/GitHub/Rbitfit/fitcoach/fitcoach/inst/extdata/daily/2015-12-10.json"))


df <- fromJSON(paste(jsonFileTimeSeries , "/sampletime.json" , sep = "") , simplifyDataFrame = TRUE , flatten = TRUE)
mdf <- as.data.frame(df$`activities-tracker-calories`)



