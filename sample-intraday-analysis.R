library(jsonlite)
libarry(httr)

# Loading all the intradaya files
days <- 1:5
year <- 2015
month <- 12
resources <- getIntradayResourcePathList()

createIntraTsURL <- function(resource , year , month , day){
  paste('https://api.fitbit.com/1/user/-/activities/', resource , '/date/',
        year , '-' , month , '-' , day ,
        '/1d/15min/time/00:00/23:59.json' , sep = "")
}

createIntraTsFilePath <- function(resource , year , month , day){
  fileName <- paste('intra-' , resource , '-' , year , '-' , month , '-' , day ,'.json' , sep = "")
}

# Note Fitbit rate limit is 150 api calls per hour.

for(r in resources){
  urls <- sapply(days , function(x) {createIntraTsURL(r , year , month , x) })
  files <- sapply(days , function(x) { createIntraTsFilePath(r , year , month , x) })
  files <- paste("inst/extdata/intra-daily-timeseries/" , files , sep = "")
  res <- lapply(1: length(urls) , function(x) { fetchAndStoreFile(urls[x] , token , files[x])})
}


# Data.frame conversion
intraTsFolder <- 'inst/extdata/intra-daily-timeseries/'
files <- list.files(intraTsFolder)
indexes <- grep("intra-+" , files)
files <- files[indexes]

indexes <- grep("-calories-" , files)

res.files <- files[indexes]
res.files <- paste(intraTsFolder , res.files , sep = "")
res.df <- as.data.frame(fromJSON(res.files[23] , simplifyDataFrame = TRUE ))


str(df)

intraColNames <- c("date",
                   "total.calorie",
                   "level",
                   "mets",
                   "time",
                   "intra.calorie",
                   "time.interval",
                   "dataset.type")

colnames(df) <- intraColNames


df$date <- as.Date(df$date)
df$total.calorie <- as.numeric(df$total.calorie)
df$level <- as.numeric(df$level)
df$mets <- as.numeric(df$mets)
df$time <- as.character(df$time)
df$intra.calorie <- as.numeric(df$intra.calorie)
df$time.interval <- as.numeric(df$time.interval)
df$dataset.type <- as.character(df$dataset.type)

str(df)

sum(df$intra.calorie)
