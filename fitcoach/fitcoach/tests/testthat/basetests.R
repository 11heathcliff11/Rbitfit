library(jsonlite)
library(glmnet)
source(file = "FitUtil.R")


tsFileFolder <- '../inst/extdata/daily-time-series/'
master <- createTsMasterFrame(tsFileFolder)
master <- markValidRows(master)
master <- master[master$valid == TRUE ,]
master <- augmentData(master)
str(master)
goal <- 'calories'
y <- createGoalVariableVector(master , goal)
x <- createDependentVariableFrame(master , goal)
x <- as.matrix(x)
x<- scale(x)

fit <- glmnet(x , y , family = "gaussian" , lambda = .01 )
cvfit <- cv.glmnet(x,y)
plot(cvfit)

f2 <- lm(formula = y~x)
f2$coefficients
f2$effects




summary(f2)






cat("rows for Predictive Model " , sum(master$valid))
master$valid = NULL



mm <- data.matrix(numMaster, rownames.force = NA)
str(mm)
x <- mm
fit <- glmnet(x , y , family = "gaussian")
summary(fit)

fit

f2 <- lm(formula = y~x)
summary(f2)

f3 <- glm(formula = y~x)
summary(f3)


plot(f2)


str(master)

