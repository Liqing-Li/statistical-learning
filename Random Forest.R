######## Random Forest ###########
library(randomForest)
## randomForest 4.6-14
## Type rfNews() to see new features/changes/bug fixes.

#Split the data into training and test.
load("BostonHousing1.Rdata")
mydata = Housing1
n = nrow(mydata)
ntest = round(n * 0.3)
set.seed(1234)
test.id = sample(1:n, ntest)

# Fit a random forest
rfModel = randomForest(Y ~ ., data = mydata[-test.id, ],
                       importance = T, ntree=400); 
names(rfModel)

## the default value for mtry is p/3 for regression
## p = ncol(mydata) - 1 = 15
## mtry = 15/3 = 5
rfModel$mtry

## Test Error
yhat.test = predict(rfModel, mydata[test.id, ])
sum((mydata$Y[test.id] - yhat.test)^2)/length(test.id)

## Two Training Errors
yhat.train = predict(rfModel, mydata[-test.id, ])
sum((mydata$Y[-test.id] - yhat.train) ^2)/(n - ntest)

sum((mydata$Y[-test.id] - rfModel$predicted) ^2)/(n - ntest)

#We can evaluate the training error by obtaining the prediction on the training set 
#(same as the regular training error, i.e., may underestimate the realerror). 
#But randomForest provides an estimate of the training error based on OOB samples, 
#which is similar to CV errors, i.e., an unbiased estimate of the real classification error.
rfModel$oob.times[1:5] # each obs, how many times you are out of sample bag
# the first obs is not built based on 141 trees, the predictor of first obs is based on avg of these 114 trees
length(rfModel$oob)
# prob (a random obs not in a bag) =(1-1/n)^n, goes to e^-1 when n is large enough
## oob.times --> ntree * exp(-1) = ntree * 0.368
rfModel$ntree * exp(-1)
mean(rfModel$oob.times)

## The plot function for randomForest
tmp = rfModel$mse
par(mfrow=c(1, 2))
plot(rfModel)
plot(c(0, rfModel$ntree), range(tmp), type="n",
     xlab = "Number of trees", ylab="Error")
lines(tmp)

par(mfrow=c(1, 1))

##Variable importance##

#%IncMSE: increase in MSE of predictions if variable j being permuted (for OOB samples) – shuffle the value of variable j, 
#record the change of MSE for each tree, and average over all trees. Can be normalize by standard error with option “scale = TRUE” (default is unscaled).

#IncNodePurity: total decrease of RSS from splitting on variable j, averaged over all trees.

## default %IncMSE is normalized
rfModel$importance

importance(rfModel, scale = F)

cbind(importance(rfModel, scale = TRUE), 
      importance(rfModel, scale = F)[,1]/rfModel$importanceSD)

par(mfrow = c(1,2))
varImpPlot(rfModel, type=1)
varImpPlot(rfModel, type=2)




