############## GBM for Regression ##############
#Use gbm package in R to fit a gradient boosting model. For more GBM examples, check how to use GBM to analyze the AmesHousing data
#http://uc-r.github.io/gbm_regression
#Use loss function gaussian (squared error) for regression.

#n.trees: number of trees (default 100)

#shrinkage: shrinkage factor for the step size (default 0.001)

#bag.fraction: subsampling rate (default 0.5)

#cv.folds: return CV error (default 0, i.e., no CV error returned)

#interaction.depth: depth of trees (default 1)
rm(list=objects())
library(gbm)
#split data
load("~/Dropbox/R code-referene/statitical learning/statistical learning/Data/BostonHousing1.Rdata")
mydata = Housing1
n = nrow(mydata)
ntest = round(n * 0.3)
set.seed(1234)
test.id = sample(1:n, ntest)
##
myfit1 = gbm(Y ~ . , data = mydata[-test.id, ], 
             distribution = "gaussian", 
             n.trees = 100,
             shrinkage = 1, #no shrinkage
             interaction.depth = 3, 
             bag.fraction = 1, #all data for traning, no subsampling
             cv.folds = 5)
myfit1

# cv plot
gbm.perf(myfit1) #only use 28 trees for prediction
#black line: trianing error, green: cv error

opt.size = gbm.perf(myfit1)

## compute test error across all trees
size = 1:myfit1$n.trees
test.err = rep(0, length(size))
for(i in 1:length(size)){
  y.pred = predict(myfit1, mydata[test.id, ], n.trees = size[i])
  test.err[i] = sum((mydata$Y[test.id] - y.pred)^2)
}    
plot(test.err, type = "n")
lines(size, test.err, lwd = 2)
abline(v = opt.size, lwd = 2, lty = 2, col = "blue")


### Add shrinkage
myfit2 = gbm(Y ~ . , data = mydata[-test.id, ], 
             distribution = "gaussian", 
             n.trees = 1000,
             shrinkage = 0.1, 
             interaction.depth = 3, 
             bag.fraction = 0.5,
             cv.folds = 5)
gbm.perf(myfit2)
opt.size = gbm.perf(myfit2)
## compute test error across all trees
size = 1:myfit2$n.trees
test.err = rep(0, length(size))
for(i in 1:length(size)){
  y.pred = predict(myfit2, mydata[test.id, ], n.trees = size[i])
  test.err[i] = sum((mydata$Y[test.id] - y.pred)^2)
}    
plot(test.err, type = "n")
lines(size, test.err, lwd = 2)
abline(v = opt.size, lwd = 2, lty = 2, col = "blue")

### Variable importance.
par(mfrow=c(1, 2))
summary(myfit1, cBars = 10,
        method = relative.influence, 
        las = 2)

summary(myfit1, cBars = 10,
        method = permutation.test.gbm, 
        las = 2)
par(mfrow = c(1,1))