library(dplyr)
library(gbm)

df <- intraMaster

a<- cut(df$timeseq , breaks = c(0,23, 41 , 77 , 90 , 96) , labels = c("night" , "morning" , "day" ,"eve" , "latenight" ) )
df$slot <- a
#mod<- transform(df,  cumsum.calorie = ave(intra.calorie , date, slot , FUN=cumsum)) 
mod<- transform(df,  cumsum.calorie = ave(intra.calorie , date,  FUN=cumsum)) 
mod<- transform(mod, cumsum.steps = ave(intra.steps , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.level = ave(intra.level , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.mets = ave(intra.mets , date, FUN=cumsum)) 
mod<- transform(mod, cumsum.distance = ave(intra.distance , date, FUN=cumsum)) 
#mod<- transform(mod, cumsum.floors = ave(intra.floors , date, FUN=cumsum)) 
#mod<- transform(mod, cumsum.elevation = ave(intra.elevation , date, FUN=cumsum)) 

anaFrame <- mod



str(anaFrame)

anaFrame$date = NULL
anaFrame$time = NULL
anaFrame$weekend <- as.factor(anaFrame$weekend)

fit <- gbm::gbm(formula = calories~. , data = anaFrame , distribution = "gaussian" , n.trees = 500 ,
           shrinkage = .01 , interaction.depth = 5 , bag.fraction = .5 , train.fraction = .8 ,
           cv.folds = 7 , verbose = FALSE)

imp <- relative.influence(fit , n.trees = 300 , scale = TRUE)
sort(imp , decreasing = TRUE)
nd <- anaFrame[2000 ,]
nd$calories <- NULL
best.iter <- gbm::gbm.perf(fit,method="test")
gbm::predict.gbm(fit , newdata = nd ,n.trees = best.iter)


summary(fit)

imp <- caret::varImp(fit)

imp <- caret::varImp(fit, scale = FALSE)
imp$name <- rownames(imp)
imp <- dplyr::arrange(imp,-Overall)


str(df)

