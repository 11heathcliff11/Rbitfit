# Fit Analyser
#
library(jsonlite)
source(file = "FitUtil.R")

tsFileFolder <- '../inst/extdata/daily-time-series/'

master <- createTsMasterFrame(tsFileFolder)

resourcePath <- getResourcePathList()

dflist <- lapply(resourcePath, function(x){
                    df <- as.data.frame(fromJSON(paste(tsFileFolder , "max-" , x ,".json" , sep = "") , simplifyDataFrame = TRUE));
                    colnames(df)[1] <- "datetime";
                    colnames(df)[2] <- x;
                    return(df)
})


masterdf <- as.data.frame(dflist[1])
for(i in 2:length(dflist)){
  masterdf <- merge(masterdf , as.data.frame(dflist[i]) , by = "datetime")
}


hdf <- fromJSON(paste(paste(tsFileFolder , 'heart-1m.json', sep = "")) , simplifyDataFrame = TRUE , flatten = TRUE)
str(hdf)
