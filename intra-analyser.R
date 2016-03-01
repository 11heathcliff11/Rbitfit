library(dplyr)
library(gbm)


a<- cut(df$timeseq , breaks = c(0,23, 41 , 77 , 90 , 96) , labels = c("night" , "morning" , "day" ,"eve" , "latenight" ) )
df$slot <- a
mod<- transform(df,  cumsum.calorie = ave(intra.calorie , date, slot , FUN=cumsum)) 
mod<- transform(mod, cumsum.steps = ave(intra.steps , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.level = ave(intra.level , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.mets = ave(intra.mets , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.distance = ave(intra.distance , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.floors = ave(intra.floors , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.elevation = ave(intra.elevation , date, FUN=cumsum)) 

anaFrame <- mod

anaFrame$date = NULL
anaFrame$time = NULL
sl <- anaFrame$slot
anaFrame$slot <- NULL
anaMatrix <- data.matrix(anaFrame , rownames.force = NA)
anaFrame <- as.data.frame(anaMatrix)
anaFrame$slot <- sl

fit <- gbm(formula = calories~. , data = anaFrame , distribution = "gaussian" , n.trees = 500 ,
           shrinkage = .05 , interaction.depth = 5 , bag.fraction = .5 , train.fraction = .8 ,
           cv.folds = 3 , verbose = TRUE)

summary(fit)

str(df)

