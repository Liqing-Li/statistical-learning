###Prepare the Boston Housing Data
library(MASS)
data(Boston)
head(Boston)
names(Boston)
#Change the response variable name to be “Y”. Next take some transformations on Y and X’s, suggested in the literature.
myData <- Boston
names(myData)[14] <- "Y"
iLog <- c(1, 3, 5, 6, 8, 9, 10, 14)
myData[, iLog] <- log(myData[, iLog])
myData[, 2] <- myData[, 2]/10
myData[, 7] <- myData[, 7]^2.5/10^4
myData[, 11] <- exp(0.4 * myData[, 11])/1000
myData[, 12] <- myData[, 12]/100
myData[, 13] <- sqrt(myData[, 13])

summary(myData)

###Fit a Linear Model
lmfit <-  lm(Y ~ ., data = myData)
names(lmfit)
lmfit$residuals[1]
sqrt(sum(lmfit$residuals^2)/(506 - 14))  # residual standard error
1 - sum(lmfit$residuals^2)/(var(myData$Y)*505)  # R-square
lmfit$coef    # 13 regression cofficients including the intercept
summary(lmfit)

#Predict the price for two new houses.
newx <-  apply(myData, 2, median)
newx <-  rbind(newx, newx)
newx
newx[, 4] <-  c(1,0)
newx
newx[, -14] %*% lmfit$coef[-1] + lmfit$coef[1]#prediction 
# or use the "predict" function, then new data should 
# be a data frame. 
row.names(newx) = NULL
newx <- data.frame(newx)
predict(lmfit, newdata = newx)
###Rank Deficiency
#If the design matrix (including the intercept) is not of full rank, the coefficient vector returned by R will have some elements to be NA. 
#A column has its LS estimate to be NA means that it can be written as a linear combination of some columns listed before it, that is, this is a redundant column.
#NA values do not mean error. It just means that in the LS fitting, R ignores the columns with NA coefficients. You can still use the fitted model to do prediction.
#The result should be the same as if you fit a linear regression model without those columns.
## Add a fake column named "junk"
myData$junk <-  myData$crim + myData$zn
tmp.lm <-  lm(Y ~ ., myData)
summary(tmp.lm)

## The fitted values (for the first 3 obs) are the same. 
tmp.lm$fitted[1:3]
lmfit$fitted[1:3]

## remove the "junk" column
myData = myData[,-15]

###Training Error vs Test Error
#For linear regression models, when we add more and more variables,
#the training error (e.g., RSS or MSE) is always decreasing,
#but the test error (prediction error on an independent test data) is not necessary decreasing.
# Go back to the Boston Housing Data
# Divide the data into training and test

# n: sample size
# col 1:p: predictors
# col (p+1): response (in this particular example) 
n <- dim(myData)[1]
p <-  dim(myData)[2] - 1

ntrain <-  round(n*0.6)
train.id <-  sample(1:n, ntrain)
train.MSE <-  rep(0, p)
test.MSE <-  rep(0, p)

for(i in 1:p){
  myfit <-  lm(Y ~ ., myData[train.id, c(1:i, (p+1))])
  train.Y <-  myData[train.id, (p+1)]
  train.Y.pred <-  myfit$fitted
  train.MSE[i] <-  mean((train.Y - train.Y.pred)^2)
  
  test.Y <-  myData[-train.id, (p+1)]
  test.Y.pred <-  predict(myfit, newdata = myData[-train.id, ])
  test.MSE[i] <-  mean((test.Y - test.Y.pred)^2)
}

## type="n": don't plot; just set the plotting region
plot(c(1, p), range(train.MSE, test.MSE), type="n", 
     xlab="# of variables", ylab="MSE")
points(train.MSE, col = "blue", pch = 1)
lines(train.MSE, col = "blue", pch = 1)
points(test.MSE, col = "red", pch = 2)
lines(test.MSE, col = "red", pch = 2)

diff(train.RSS) ## always negative
diff(test.RSS)  ## not always negative

###Partial Regression Coefficients
round(cor(myData), dig=2)
#Check how to retrieve the LS coefficient for “age” using Algorithm 3.1
y.star <-  lm(Y ~ ., data = subset(myData, select = -age))$res
age.star <-  lm(age ~ ., data = subset(myData, select = -Y))$res
tmpfit <-  lm(y.star ~ age.star)
#The LS cofficient for “age” (from lmfit) is the same as the one from tmpfit. 
#The residuals from the two LS models are also the same.
tmpfit$coef
sum((lmfit$res - tmpfit$res)^2) #almost close to 0


### F test
#Test a single predictor (in this case, F-test = t-test).
lmfit0 <- lm(Y ~ ., data = subset(myData, select = -age))
anova(lmfit0, lmfit)
#Test multiple predictors.
lmfit0 <- lm(Y ~ ., data = myData[, -c(1:3)])
anova(lmfit0, lmfit)

###Collinearity
library(faraway)
data(seatpos)
pairs(seatpos)
summary(lm(hipcenter ~ . , data=seatpos))
# t-test: conditional contribution
# F-test is small, reject null and current model is fine
# Large R^2, significant F and insignificant p value for t-test
# How to deal with it: combine some variables (PCA) if care about estimation;
# however with prediction, collinearity doesnt matter
