
#glmnet: library for both lasso (alpha=1) and ridge (alpha=0).
mypackages = c("MASS", "glmnet")   # required packages
tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
if (length(tmp) > 0) install.packages(tmp)
lapply(mypackages, require, character.only = TRUE)
set.seed(2134)

#Prepare the Boston Housing Data:
myData = Boston
names(myData)[14] = "Y"
iLog = c(1, 3, 5, 6, 8, 9, 10, 14);
myData[, iLog] = log(myData[, iLog]);
myData[, 2] = myData[, 2] / 10;
myData[, 7] = myData[, 7]^2.5 / 10^4
myData[, 11] = exp(0.4 * myData[, 11]) / 1000;
myData[, 12] = myData[, 12] / 100;
myData[, 13] = sqrt(myData[, 13]);

# Move the last column of myData, the response Y, to the 1st column.
myData = data.frame(Y = myData[,14], myData[,-14]);
names(myData)[1] = "Y";
names(myData)

n = dim(myData)[1]; 
p = dim(myData)[2]-1;
X = as.matrix(myData[, -1]);  # some algorithms need the matrix/vector 
Y = myData[, 1];              # input (instead of data frame)
#Split the data into two parts: 80% for training and 20% for testing
ntest = round(n*0.2)
ntrain = n - ntest;
test.id = sample(1:n, ntest);
Ytest = myData[test.id, 1];

#Full Model
full.model = lm( Y ~ ., data = myData[-test.id, ]);  
Ytest.pred = predict(full.model, newdata= myData[test.id, ]);
sum((Ytest - Ytest.pred)^2)/ntest # averaged MSE on the test set


######## Lasso ########

###Lasso using glmnet; lambda chosen by 10-fold CV.
mylasso = glmnet(X[-test.id,], Y[-test.id], alpha = 1)
summary(mylasso)

par(mfrow = c(1, 2))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")
#lambda get larger, all coefficients shrink to zero at the end
par(mfrow=c(1,1))
mylasso$df
##cross-validation (first dashline- Lambda_min, second-lambda_1se(within one standard error from the minumum))
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
plot(cv.out) # suggest a small lambda

##Check how lambda.min and lambda.1se are computed.
cv.out$lambda.min
tmp.id=which.min(cv.out$cvm)
cv.out$lambda[tmp.id]

cv.out$lambda.1se
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])
##Retrieve Lasso coefficients.
mylasso.coef.min = predict(mylasso, s=cv.out$lambda.min, type="coefficients")
mylasso.coef.1se = predict(mylasso, s=cv.out$lambda.1se, type="coefficients")
cbind(mylasso.coef.min, mylasso.coef.1se)

# number of variables selected (including the intercept)
sum(mylasso.coef.1se != 0)
# names of selected non-intercept variables
row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]

##Apply the fitted model for prediction on the test data.
mylasso = glmnet(X[-test.id, ], Y[-test.id], alpha = 1)
Ytest.pred = predict(mylasso, s = cv.out$lambda.min, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

Ytest.pred = predict(mylasso, s = cv.out$lambda.1se, newx = X[test.id,])
mean((Ytest.pred - Y[test.id])^2)# for this example, not as good as lambda_min

##Refit the model selected by Lasso to reduce bias.

# Variables selected by lambda.1se 
mylasso.coef.1se = predict(mylasso, s = cv.out$lambda.1se, type="coefficients")
var.sel = row.names(mylasso.coef.1se)[nonzeroCoef(mylasso.coef.1se)[-1]]

var.sel; 

tmp.X = X[, colnames(X) %in% var.sel]
mylasso.refit = coef(lm(Y[-test.id] ~ tmp.X[-test.id, ]))#training
Ytest.pred = mylasso.refit[1] + tmp.X[test.id,] %*% mylasso.refit[-1]
mean((Ytest.pred - Y[test.id])^2)


######## Ridge ########

###Ridge using glmnet; lambda chosen by 10-fold CV

myridge = glmnet(X[-test.id, ], Y[-test.id], alpha = 0)
plot(myridge, label = TRUE, xvar = "lambda")

##Check the output from glmnet for ridge regression.
summary(myridge)

length(myridge$lambda)  # retrieve the lambda value
dim(myridge$beta)       # coefficients for 13 non-intercept predictors
length(myridge$a0)      # intercept

# the 13 coefficients (including intercept) can also be retrieved using
# coef(myridge)
dim(coef(myridge))

# The two coefficient matrices should be the same
sum((coef(myridge) - rbind(myridge$a0, myridge$beta))^2)

##Ridge regression coefs could change sign along the path
round(myridge$beta[8, ], dig = 2)

##How are the intercepts computed?
k = 2; 
my.mean = apply(X[-test.id, ], 2, mean)  # 13x1 mean vector for training X
mean(Y[-test.id]) - sum(my.mean * myridge$beta[, k])

myridge$a0[k]  # intercept for lambda = myridge$lambda[k]

# Check whether our intercept formula is true for all intercepts 
sum((mean(Y[-test.id]) - my.mean %*% myridge$beta  - myridge$a0)^2)

##Selection lambda by 10-fold CV. 
#The CV results are stored in:
#cv.out$cvm: mean CV error
#cv.out$cvsd: estimate of standard error of cvm
#Two choices for lambda
#lambda.min: the value of lambda that gives the minimum cvm
#lambda.1se: the largest value of lambda (i.e., the largest regularization, the smallest df) whose cvm is within 1 standard error of the  cvm of lambda.min.
cv.out = cv.glmnet(X[-test.id, ], Y[-test.id], alpha = 0) 
plot(cv.out)
lam.seq = exp(seq(-6, 2, length=100))
cv.out = cv.glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda=lam.seq)  
plot(cv.out)
names(cv.out)
#how lambda.min and lambda.1se is received
cv.out$lambda[which.min(cv.out$cvm)]
cv.out$lambda.min
tmp.id = which.min(cv.out$cvm)
max(cv.out$lambda[cv.out$cvm < cv.out$cvm[tmp.id] + cv.out$cvsd[tmp.id]])
cv.out$lambda.1se

##Evaluate prediction performance
myridge = glmnet(X[-test.id,], Y[-test.id], alpha=0, lambda = lam.seq)
Ytest.pred = predict(myridge, s = cv.out$lambda.1se, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

Ytest.pred=predict(myridge, s = cv.out$lambda.min, newx=X[test.id,])
mean((Ytest.pred - Y[test.id])^2)

############ Variable selection : AIC/BIC#############
#Prepare the Boston Housing Data:

library(MASS)
myData = Boston
names(myData)[14] = "Y"
iLog = c( 1,3,5,6,8,9,10,14 );
myData[,iLog] = log( myData[,iLog] );
myData[,2] = myData[,2] / 10;
myData[,7] = myData[,7]^2.5 / 10^4
myData[,11] = exp( 0.4 * myData[,11] ) / 1000;
myData[,12] = myData[,12] / 100;
myData[,13] = sqrt( myData[,13] );

## Fit the full model
full.model = lm( Y ~ ., data = myData);  
n = dim(myData)[1];
summary(full.model)
p = dim(myData)[2]-1

###Level-wise search
library(leaps)
b = regsubsets(Y ~ ., data=myData, nvmax = p)
rs = summary(b)
rs$which
# calculate AIC and BIC
msize = 1:p;
par(mfrow=c(1,2))
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");

#For this particular data set, AIC and BIC end up selecting the same model.
#Sometimes, the models selected by different criteria may be different.
cbind(rs$which[which.min(Aic),], rs$which[which.min(Bic), ])

#leaps does not return AIC, but BIC. Its BIC differs from what has been computed above,
#but the difference is a constant, so the two BIC formulae (ours and the one used by leaps)
#are essentially the same.
cbind(rs$bic, Bic, rs$bic - Bic)

#the 2nd and 3rd best models in terms of AIC/BIC
?regsubsets
b = regsubsets(Y ~ ., data=myData, nbest = 3, nvmax = p)
rs = summary(b)
rs$which

# calculate AIC and BIC
msize = apply(rs$which, 1, sum) - 1
par(mfrow=c(1,2)) #mfrow=c(nrows, ncols)
Aic = n*log(rs$rss/n) + 2*msize;
Bic = n*log(rs$rss/n) + msize*log(n);
plot(msize, Aic, xlab="No. of Parameters", ylab = "AIC");
plot(msize, Bic, xlab="No. of Parameters", ylab = "BIC");

#Although the top model returned by AIC and the one by BIC are the same;
#the 2nd and the 3rd best models returned by the two criteria are different:
#AIC favors larger models while BIC favors smaller models
plot(msize[msize > 3], Aic[msize > 3], xlab="No. of Parameters", ylab = "AIC");
plot(msize[msize > 3], Bic[msize > 3], xlab="No. of Parameters", ylab = "BIC");

# top three models by AIC
rs$which[order(Aic)[1:3],]

# top three models by BIC
rs$which[order(Bic)[1:3],]

###Stepwise AIC##
stepAIC = step(full.model, direction="both")

###Stepwise BIC
n = dim(myData)[1]
stepBIC = step(full.model, direction="both", k=log(n)) 

# report variable selection
sel.var.AIC = attr(stepAIC$terms, "term.labels")
sel.var.BIC = attr(stepBIC$terms, "term.labels")
sel.var.AIC
length(sel.var.AIC)
length(sel.var.BIC)
sel.var.BIC %in% sel.var.AIC # AIC tends to pick bigger model, BIC pick smaller

############
##########Variable Selection – A Toy Example############
############
mypackages = c("leaps", "glmnet")   
tmp = setdiff(mypackages, rownames(installed.packages())) 
if (length(tmp) > 0) install.packages(tmp)

library(leaps)  # regsubsets
library(glmnet)  # glmnet for lasso
#This example is from simulation example 1 of Zhao and Yu (2006), which shows that LASSO could fail to pick the correct variable set when the so-called Irrepresentable Condition is violated.
#Consider a simple linear regression model with three predictors X1,X2, and X3, and the response variable Y is modeled as
#Y=2⋅X1+3⋅X2+N(0,1),
#that is, the last predictor X3 is irrelevant. The three predictors are generated as follows: X1∼N(0,1), X2∼N(0,1), and
#X3=2/3X1+2/3X2+N(0,1/3^2).
#*This Irrepresentable Condition, which depends mainly on the covariance of the predictor vari- ables, 
#*states that Lasso selects the true model consistently if and (almost) only if the predictors that are not in the true model are “irrepresentable” (in a sense to be clarified) by predictors that are in the true model.


#Generate n=1000 samples from this model.
# set random seed in case you want to reproduce the result
set.seed(542)  
n = 1000
x1 = rnorm(n)
x1 = rnorm(n)
x2 = rnorm(n)
e = rnorm(n)
x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * e
epsilon = rnorm(n)
beta = c(2, 3)
y = beta[1] * x1 + beta[2] * x2 + epsilon
myData = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

##Use AIC/BIC to select the best sub-model
IC = regsubsets(y ~ ., myData, method = "exhaustive")
sumIC = summary(IC)
sumIC$bic

sumIC

msize = apply(sumIC$which, 1, sum) #1 is row, 2 is column
AIC = n * log(sumIC$rss/n) + 2 * msize
BIC = n * log(sumIC$rss/n) + log(n) * msize
AIC; BIC

###Use LASSO
#Next, we use LASSO with lambda.min and lambda.1se to check if it can select the correct model.
mylasso.cv = cv.glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))
plot(mylasso.cv)

coef(mylasso.cv, s = 'lambda.1se')

coef(mylasso.cv, s = 'lambda.min')

mylasso = glmnet(as.matrix(myData[, c('x1', 'x2', 'x3')]), as.vector(myData$y))
par(mfrow = c(2, 1))
plot(mylasso, label=TRUE, xvar = "norm")
plot(mylasso, label=TRUE, xvar = "lambda")
# Impossible to drop X3, since it requires log-lambda is going to −∞  (or equivalently λ→0).
# Lasso bias: shrinkage due to λ,bias from x1 and x2 are take care of by x3
# it is an issue for variable selection, but not for prediction

##Compare prediction performance
N = 1000
mytestData = matrix(0, N, 4)
colnames(mytestData) = c("x1", "x2", "x3", "y")
mytestData = as.data.frame(mytestData)

mytestData$x1 = rnorm(N)
mytestData$x2 = rnorm(N)
mytestData$x3 = 2/3 * x1 + 2/3 * x2 + 1/3 * rnorm(N)
mytestData$y = beta[1] * mytestData$x1 + 
  beta[2] * mytestData$x2 + rnorm(N)

#For Lasso, you can form prediction using one of the following approaches:
  
 # Use the coefficients from lambda.1se;
tmp = predict(mylasso.cv, s="lambda.1se", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)
#Use the coefficients from lambda.min;

tmp = predict(mylasso.cv, s="lambda.min", 
              newx=data.matrix(mytestData)[, -4])
mean((mytestData$y - tmp)^2)
#Refit a LS model using variables selected by lambda.1se;
#Refit a LS model using variables selected by lambda.min.
#In this example, lambda.1se and lambda.min select the same model, the full model.

#There are two commands for Lasso prediction: one takes the output from cv.glmnet as input (where you can use lambda.min and lambda.1se), and the other one takes the output from glmnet as input.
?predict.cv.glmnet
?predict.glmnet
#The results below indicate that for this example, the test error from the Refit procedure is not bad even though the model is wrong.
#AIC
myfit.AIC = lm(y ~ x1 + x2, myData)
tmp = predict(myfit.AIC, newdata=mytestData)
mean((mytestData$y - tmp)^2)
#Full
myfit.full = lm(y ~ ., myData)
tmp = predict(myfit.full, newdata=mytestData)
mean((mytestData$y - tmp)^2)




