#GET https://api.fitbit.com/1/user/-/[resource-path]/date/[date]/[date]/[detail-level].json
# Loading all the intradaya files
library(jsonlite)
library(httr)
library(plyr)
library(dplyr)

days <- 16:30
year <- 2015
month <- 12
resources <- getIntradayResourcePathList()

createIntraTsURL <- function(resource , year , month , day){
  paste('https://api.fitbit.com/1/user/-/activities/', resource , '/date/',
        year , '-' , month , '-' , day ,
        '/1d/15min/time/00:00/23:59.json' , sep = "")
}

newURL <-  function(resource , year , month , day){
  paste('https://api.fitbit.com/1/user/-/activities/', resource , '/date/',
        year , '-' , month , '-' , day , '/' , year , '-' , month , '-' , day , '/' ,
        '15min.json' , sep = "")
}


createIntraTsFilePath <- function(resource , year , month , day){
  fileName <- paste('intra-' , resource , '-' , year , '-' , month , '-' , day ,'.json' , sep = "")
}




for(r in resources){
  urls <- sapply(days , function(x) {newURL(r , year , month , x) })
  files <- sapply(days , function(x) { createIntraTsFilePath(r , year , month , x) })
  files <- paste("inst/extdata/intra-daily-timeseries/" , files , sep = "")
  res <- lapply(1: length(urls) , function(x) { fetchAndStoreFile(urls[x] , token , files[x])})
  readline("Press <return to continue")
}



# Data.frame conversion
intraTsFolder <- 'fitcoach/inst/extdata/intra-daily-timeseries/'
folder <- intraTsFolder

a <- createIntraFrame("calories" , intraTsFolder)
b <- createIntraFrame("steps" , intraTsFolder)
merge(a , b)


a <- rbind(a, createIntraFrame("steps" , intraTsFolder))
a <- rbind(a, createIntraFrame("elevation" , intraTsFolder))
a <- rbind(a, createIntraFrame("distance" , intraTsFolder))
a <- rbind(a, createIntraFrame("floors" , intraTsFolder))



indexes <- grep("-calories-" , files)

res.files <- files[indexes]
res.files <- paste(intraTsFolder , res.files , sep = "")
res.df <- NULL
res.df <- as.data.frame(fromJSON(res.files[2] , simplifyDataFrame = TRUE ))
res.df$resource <- "calories"
intraColNames <- c("date",
                   "total.value",
                   "level",
                   "mets",
                   "time",
                   "intra.value",
                   "time.interval",
                   "dataset.type" ,
                   "resource")

colnames(res.df) <- intraColNames


indexes <- grep("-steps-" , files)
res.files <- files[indexes]
res.files <- paste(intraTsFolder , res.files , sep = "")
res.df <- as.data.frame(fromJSON(res.files[2] , simplifyDataFrame = TRUE ))
res.df$resource <- "steps"
intraColNames <- c("date",
                   "total.value",
                   "time",
                   "intra.value",
                   "time.interval",
                   "dataset.type" ,
                   "resource")
colnames(res.df) <- intraColNames

##  floors
indexes <- grep("-elevation-" , files)
res.files <- files[indexes]
res.files <- paste(intraTsFolder , res.files , sep = "")
res.df <- as.data.frame(fromJSON(res.files[2] , simplifyDataFrame = TRUE ))
res.df$resource <- "distance"
level <- rep(0 , nrow(res.df))
mets <- rep(NA , nrow(res.df))
res.df <- cbind(res.df[,1:2] , mets , level , res.df[,3:7])

intraColNames <- c("date",
                   "total.value",
                   "time",
                   "intra.value",
                   "time.interval",
                   "dataset.type" ,
                   "resource")
colnames(res.df) <- intraColNames



str(df)



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



#SWitch example








