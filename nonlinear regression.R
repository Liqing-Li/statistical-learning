
#### ########### Nonlinear Regression ###### #######

#### Polynomial Regression ######
#Most code is from Lab 7.8 in ISLR.
library(ISLR) 
attach(Wage) # dont need $ when recall a variable in the model
dim(Wage)

names
range(age)

##Fit a Polynomial Regression Model
#poly( , 3) returns an n-by-3 matrix: each column has mean zero and sample variance 1, and they are orthogonal to each other. 
#The 1st column is a linear combination of age and intercept, the 2nd column is a linear combination of age^2, age, and intercept, 
#and the 3rd column is a linear combination of age^3, age^2, age, and intercept.
tmp = poly(age, 3)
dim(tmp)

colMeans(tmp) #mean zero
round(t(tmp) %*% tmp, dig=4)

fit = lm(wage ~ poly(age, 3), data = Wage)
round(summary(fit)$coef, dig = 3)

#Alternatively we can use the default design matrix where the j-th column corresponds to age^j.
fit2 = lm(wage ~ age + I(age^2) + I(age^3), data=Wage)
round(summary(fit2)$coef, dig = 3)
# The default design matrix can also be generated by poly
# with option raw = TRUE. 
# fit3 should return the same set of coefficients as fit2
# fit3 = lm(wage ~ poly(age, 3, raw = T), data = Wage)
# round(summary(fit2)$coef, dig = 3)

#Note that although the coefficients from fit and the ones from fit2 are different, the t-value and p-value for the last predictor are always the same.
#so if only care about the coefficient of highest order, then doesnt matter which model to choose

#Different ways to fit a polynomial regression model in R. The coefficients might not be the same but the fitted curves are the same.

##Predict the wage at age = 82. fit and fit2 should give you the same answer.
predict(fit, newdata = list(age=82))
predict(fit2, newdata = list(age=82))

##The fitted curve from the all three models should be the same.
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se=TRUE)
plot(age, wage, xlim = agelims, pch = '.', cex = 2, col="darkgrey")
title("Degree -3 Polynomial ")
lines(age.grid, preds$fit, lwd=2, col="blue")


##Forward Selection on d
#Forward selection for the polynomial order d based on the significance of 
#the coefficient of the highest order starting with quardratic polynomial function, and we finally pick d=3.
summary(lm(wage ~ poly(age, 2), data=Wage))$coef
summary(lm(wage ~ poly(age, 3), data=Wage))$coef
summary(lm(wage ~ poly(age, 4), data=Wage))$coef#highest order insignificant, stop here and pick d=3

##Backward Selection on d
#Back selection for the polynomial order d based on the significance of 
#the coefficient of the highest order starting with d=6, and we finally pick d=3. For this data, 
#the forward and the backward approaches happen to pick the same d value, but in general, the two choices (backward or forward) for d could differ.
summary(lm(wage ~ poly(age, 6), data=Wage))$coef
summary(lm(wage ~ poly(age, 5), data=Wage))$coef
summary(lm(wage ~ poly(age, 4), data=Wage))$coef
summary(lm(wage ~ poly(age, 3), data=Wage))$coef

#* select d and includes all lower order terms by default because when use ploy, the highest order is the linear combination of lower orders

#### Regression Splines ######
#Load packages and read the help files for the two major commands
library(splines);
library(ggplot2)
help(bs)
help(ns)

##Spline Basis Functions
#Basis functions for cubic splines with 5 knots and df = 9 (m knots, the df=m+4)
# in bs function in R, df=real_df-1
x = (1:199)/100;
n = length(x)
m = 5;
myknots = 2*(1:m)/(m+1)
myknots

X = cbind(1, x, x^2, x^3);
for(i in 1:m){
  tmp = (x-myknots[i])^3;
  tmp[tmp<0] = 0;
  X = cbind(X, tmp);
}
plot(c(0,2), range(X), type="n", xlab="", ylab="")
title("Truncated Power Basis")

for(i in 1:(m+4)){
  tmp = X[,i];
  if (i<=4) mylty=1 else mylty=2;
  lines(x[tmp!=0], tmp[tmp!=0], col=i, lty=mylty, lwd=2)
}
for(i in 1:m){
  points(myknots[i], 0, pty="m", pch=19, cex=2)
}

F = bs(x,knots = myknots, intercept = TRUE) # 5+4 m=df-2
dim(F)

mydf = m+4; 
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) + 
  geom_path()

mydf = m+4; 
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) + 
  geom_path()

#If we do not set intercept = TRUE, then bs will return 9-1 = 8 columns.

F = bs(x, knots = myknots)
dim(F)
mydf = m+3; 
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) +
  geom_path()

###Basis functions for NCS with 7 knots (5 interior knots and 2 boundary knots) and df = 7
F = ns(x, knots=myknots, Boundary.knots=c(0,2), intercept=TRUE)
dim(F)
#if intercept = TRUE,thenweneedm=df−2 knots, otherwise we need m = df − 1 knots.

mydf = 7
tmpdata = data.frame(t = rep(1:n, mydf),
                     basisfunc=as.vector(F), 
                     type=as.factor(rep(1:mydf, each=n)))
ggplot(tmpdata, aes(x=t, y=basisfunc, color=type)) +
  geom_path()

### Example: The Birthrates Data #####
#This dataset lists the number of live births per 10,000 23-year-old women in the United States between 1917 and 2003.

source("birthrates.txt");
birthrates = as.data.frame(birthrates)
names(birthrates) = c("year", "rate")
ggplot(birthrates, aes(x=year, y=rate)) + 
  geom_point() + geom_smooth(method="lm", se=FALSE)

#Understand how R counts the df.
#fit1=fit2=fit3
fit1 = lm(rate~bs(year, knots=quantile(year, c(1/3, 2/3))),
          data=birthrates);
fit2 = lm(rate~bs(year, df=5), data=birthrates);
fit3 = lm(rate~bs(year, df=6, intercept=TRUE), data=birthrates) 
fit4 = lm(rate~bs(year, df=5, intercept=TRUE), data=birthrates)

plot(birthrates$year, birthrates$rate, ylim=c(90,280))
lines(spline(birthrates$year, predict(fit1)), col="red", lty=1)
lines(spline(birthrates$year, predict(fit2)), col="blue", lty=2)
lines(spline(birthrates$year, predict(fit3)), col="green", lty=3)
lines(spline(birthrates$year, predict(fit4)), lty=2, lwd=2)


# Alternatively, you can predict the spline fit on a fine grid, and then connect them

# plot(birthrates$year, birthrates$rate, ylim=c(90,280))
# year.grid = seq(from=min(birthrates$year), to=max(birthrates$year), length=200)
# ypred = predict(fit1, data.frame(year=year.grid))
# lines(year.grid, ypred, col="blue", lwd=2)

fit1=lm(rate~ns(year, knots=quantile(year, (1:4)/5)), data=birthrates);
fit2=lm(rate~ns(year, df=5), data=birthrates);
fit3=lm(rate~ns(year, df=6, intercept=TRUE), data=birthrates) 

plot(birthrates$year, birthrates$rate, ylim=c(90,280))
lines(spline(birthrates$year, predict(fit1)), col="red", lty=1)
lines(spline(birthrates$year, predict(fit2)), col="blue", lty=2)
lines(spline(birthrates$year, predict(fit3)), col="green", lty=3)


###Try cubic splines with different degree-of-freedoms
plot(birthrates$year, birthrates$rate, ylim=c(90,280));
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=7), data=birthrates))), col="blue");
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=14), data=birthrates))), col="red");
lines(spline(birthrates$year, predict(lm(rate~bs(year, df=19), data=birthrates))), col="black");
legend("topright", lty=rep(1,3), col=c("blue", "red", "black"), legend=c("df=8", "df=15", "df=20"))

##Make prediction outside the data range
#cubic knots
new = data.frame(year=1905:2015);
fit1=lm(rate~bs(year, df=7), data=birthrates);
pred1=predict(fit1, new);
#natural cubit knots
fit2=lm(rate~ns(year, df=7), data=birthrates);
pred2=predict(fit2, new);
plot(birthrates$year,birthrates$rate, xlim=c(1905,2015),
     ylim=c(min(pred1,pred2), max(pred1,pred2)), 
     ylab="Birth Rate", xlab="Year") 
lines(new$year, pred1, col="red")
lines(new$year, pred2, col="blue")
legend("bottomleft", lty=rep(1,2),  col=c("red",  "blue" ), legend=c("CS with df=7", "NCS with df=7"))

###Use 10-fold CV to select df (or equivalently the number of knots)
#The location of knots will affect the performance of a spline model. But selecting the location of knots is computationally too expensive. Instead, we place knots equally at quantiles of x, and then select just the number of knots, or equivalently, the df. Can we use F-test to select the number of knots?

#For each df, we use 10-fold CV to calculate the CV error. When doing 10-fold CV, each time, based on 90% of the data, we place the (df-4) knots at the corresponding quantiles and then fit a regression spline,

#First, we need to divide the data into K folds.

K=10
n = nrow(birthrates)
fold.size = c(rep(9, 7), rep(8, 3))
fold.id = rep(1:K, fold.size)
fold.id

fold.id = fold.id[sample(1:n, n)]
fold.id

mydf = 10:30
mycv = rep(0, length(mydf))

for(i in 1:length(mydf)){
  m = mydf[i]-4;  
  for(k in 1:K){
    id = which(fold.id == k);
    myknots = quantile(birthrates$year[-id], (1:m)/(m+1))
    myfit = lm(rate ~ bs(year, knots=myknots),
               data=birthrates[-id,])
    ypred = predict(myfit, newdata=birthrates[id,])
    mycv[i]=mycv[i] + sum((birthrates$rate[id] - ypred)^2)
  }
}
plot(mydf, mycv)

#Re-run the 10-fold CV. The plot of mydf versus mycv may vary, but shouldn’t be too different.
fold.id = rep(1:K, fold.size)
fold.id = fold.id[sample(1:n, n)]

mydf = 10:30
mycv = rep(0, length(mydf))

for(i in 1:length(mydf)){
  m = mydf[i]-4;  
  for(k in 1:K){
    id = which(fold.id == k);
    myknots = quantile(birthrates$year[-id], (1:m)/(m+1))
    myfit = lm(rate ~ bs(year, knots=myknots),
               data=birthrates[-id,])
    ypred = predict(myfit, newdata=birthrates[id,])
    mycv[i]=mycv[i] + sum((birthrates$rate[id] - ypred)^2)
  }
}
plot(mydf, mycv)


################ Smoothing Splines ##############
#Load packages and read the help file.

library(splines)
help(smooth.spline)
options(digits = 4)

#A Simulated Example (ESL, chap 5.5.2)
#(x_i,y_i): data points (i=1:30)
#(fx_j, fy_j): true function evaluated on a fine grid (j=1:50)
set.seed(1234)
n = 30 
err = 1
x = sort(runif(n))
y = sin(12*(x+0.2))/(x+0.2) + rnorm(n, 0, err);
plot(x, y, col="red");

fx = 1:50/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2);

#####Fit a smoothing spline model
#Fit smoothing spline models with various dfs.
par(mfrow=c(2,2));      # 2x2 (totally 4) subplots
plot(x,y, xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=5),fx),  lty=2, col='blue', lwd=1.5);
title('df=5');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=9),fx),  lty=2, col='blue', lwd=1.5);
title('df=9');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=15),fx),  lty=2, col='blue', lwd=1.5);
title('df=15');

plot(x,y,xlab='x', ylab='y');
lines(fx, fy, col=8, lwd=1.5);
lines(predict(smooth.spline(x, y, df=20),fx),  lty=2, col='blue', lwd=1.5);
title('df=20')

###Demmler & Reinsch Basis
#Video: 2018-1_STAT542_6.9_Smoothing Splines in R- NL_SS_R
#Here is how we obtain the DR basis: we first obtain the smoother matrix S 
#(which is not returned y R, so we write our own script to compute it), and then the eigen-vectors of S are basically the DR basis functions.
smooth.matrix = function(x, df){
  # return the smoother matrix with knots x and degree of freedom = df
  # this function is for x having unique values
  n = length(x);
  A = matrix(0, n, n);
  for(i in 1:n){
    y = rep(0, n); y[i]=1;
    yi = smooth.spline(x, y, df=df)$y;
    A[,i]= yi;
  }
  return((A+t(A))/2)
}
fx = 1:50/50
S4 = smooth.matrix(fx, df=4);#smooth matrix with df4
S9 = smooth.matrix(fx, df=9);#smooth matrix with df9. smaller lambda value
tmp = ns(fx, df=9, intercept=TRUE) #projection matrix for regression model with degree 9
H9 = tmp%*%solve(t(tmp)%*%tmp)%*%t(tmp);

#Obtain the eigen value and eigen vector of the smoother/projection matrices. 
#The eigen vectors of S4 and S9 should be the same, up to a sign flip.
#smoother matrix, lambda value irrelavant to df
# when Di increases, shrinkage more
eigen.S4 = eigen(S4);
eigen.S9 = eigen(S9);
eigen.H9 = eigen(H9);

v4 = eigen.S4$ve;
v9=  eigen.S9$ve;

par(mfrow=c(3,5));
for(i in 1:15) {
  plot(c(min(x), max(x)),c(min(v4, v9), max(v4, v9)), xlab="x", ylab="", 
       type="n");
  lines(fx, v4[,i], col=2,lty=1, lwd=2.5);
  lines(fx, v9[,i], col=3, lty=2, lwd=2.5);}

#Plot the eigen values: Note the first two eigen values are always 1.
plot(eigen.H9$va, pch=5, col="black", cex=1);
points(eigen.S4$va, , col="red", xlab='', ylab='eigen values',
       cex=1.5);
points(eigen.S9$va, pch=4, col="blue", cex=1);
lines(c(0,n), c(1, 1), col=8, lty=2, lwd=1);
legend("topright", pch=c(1,4,5), col=c("red", "blue", "black"),
       legend=c("SS with df=4", "SS with df=9", "NCS with df=9"))

#Check for the effective degree of freedom
sum(diag(S4))
sum(diag(S9))
sum(diag(H9))

#####LOO-CV and GCV to pick lambda value ######
fit = smooth.spline(x, y, df=9);
fit$df

sum(fit$lev) 
fit$lev  # leveage = diagnal entries of the smoother matrix
diag(smooth.matrix(x, df =9)) #same as leveage
fit$cv  # default: GCV

sum((y-fit$y)^2)/(1-fit$df/n)^2/n
fit=smooth.spline(x, y, df=9, cv=T) # set 'cv=T' to return CV 
fit$cv #ordinary leave one out cv
sum(((y-fit$y)/(1-fit$lev))^2)/n

####  Use LOO-CV and GCV to select df. (find the df corrospondinng to the smalleast gcv)
#Note that same as Ridge regression, smoothing splines could have fractional dfs.
df = 2+(1:40)/2
m = length(df)
mycv = rep(0,m)
mygcv = rep(0,m)
for (i in 1:m){
  fit = smooth.spline(x, y, df=df[i]);
  mygcv[i] = fit$cv;
  fit = smooth.spline(x, y, df=df[i], cv=T);
  mycv[i] = fit$cv
}

plot(c(1,20), range(mycv, mygcv)+c(-0.5,0.5), xlab='df', 
     ylab='CV scores', type='n')
points(df, mycv, col="red", cex=2);
points(df, mygcv, col="blue", cex=2);

# pick df with min cv
optdf = df[mygcv==min(mygcv)]
optdf

#refit model
fitopt = smooth.spline(x, y, df=optdf);
plot(x, y, xlab="x", ylab="y")
lines(predict(fitopt, (1:100)/100),col="red", lwd=2)
lines(fx, fy, col="gray", lwd=2)


#################### Local regression #########
# one example is KNN (local constant)
#extend: weighted (find weights by kernel(fitting local regression only with intercept))
#extend: fit local linear or local poly

#################### Kernel Smoothing#########

#Check the three examples from the faraway package.
library(faraway)
par(mfrow=c(1,3))
data(exa)
#?exa # info for this simulated dataset
plot (y ~ x, exa, main="Example A")
lines(m ~ x, exa)
data(exb)
#?exb
plot(y ~ x, exb, main="Example B")
lines(m ~ x, exb)
data(faithful)
#?faithful
plot(waiting ~ eruptions, faithful,main="old Faithful")

##Kernel smoother with different bandwidths for the Old Faithful data. 
# bandwidth get larger, get flatter
?ksmooth

par(mfrow=c(1,3))
plot(waiting ~ eruptions,
     faithful,main="bandwidth=0.1", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", 0.1), lwd=2)

plot(waiting ~ eruptions, faithful, main="bandwidth=0.5", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting,"normal", 0.5), lwd=2)

plot(waiting ~ eruptions, faithful, main="bandwidth=2", col="gray", cex=0.5)
lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", 2), lwd=2)

# Use CV to select bandwidth




library(sm)
?hcv
?sm.options  # check the options
?sm.regression

par(mfrow=c(1,2))
hm = hcv(faithful$eruptions,faithful$waiting,display="lines")
sm.regression(faithful$eruptions, faithful$waiting, h=hm,
              xlab="duration", ylab="waiting")
#optimal bandwidth
hm

## apply cv on example a
par(mfrow=c(1,2))
hm=hcv(exa$x, exa$y, display="lines")
sm.regression(exa$x, exa$y, h=hm, xlab="x", ylab="y")


## apply cv on example b
# hcv(exb$x,exb$y)

# try a smaller hstart, still error
#par(mfrow=c(1,2))
#hm = hcv(exb$x, exb$y, dislay = "lines", hstart=0.005)
#hm
#sm.regression(exb$x,exb$y,h=0.005)
# local regression/kernel smooth is not right method for example b

####### Loess #####
#fit local linear or local poly 
#?loess
# default: span = 0.75, degree = 2 (quaradic fcn)
par(mfrow=c(1,3))
plot(waiting ~ eruptions, faithful,col="gray", cex=0.5)
f=loess(waiting ~ eruptions, faithful)
i = order(faithful$eruptions)
lines(f$x[i],f$fitted[i], lwd=1.5, col="red")

plot(y ~ x, exa, col="gray", cex=0.5)
lines(exa$x,exa$m,lty=1)
f = loess(y ~ x,exa)
lines(f$x,f$fitted,lty=2, lwd=1.5, col="red")
f = loess(y ~ x, exa, span=0.22)
lines(f$x,f$fitted,lty=5, lwd=1.5, col="blue")

plot(y ~ x, exb, col="gray", cex=0.5)
lines(exb$x,exb$m, lty=1)
f =loess(y ~ x, exb)
lines(f$x,f$fitted,lty=2,lwd=1.5, col="red")
f = loess(y ~ x, exb,span=1)
lines(f$x,f$fitted,lty=5, lwd=1.5, col="blue")


## use cv to select spam
lo.lev <- function(x1, sp){
  
  ## YOUR CODE: compute the diagonal entries of 
  ##            the smoother matrix S
  
}

onestep_CV <- function(x1, y1, sp){
  
  ## YOUR CODE: 
  ## 1) fit a loess model y1 ~ x1 with span = sp, and extract 
  ##    the corresponding residual vector
  ## 2) call lo.lev to obtain the diagonal entries of S
  ## 3) compute LOO-CV and GCV
  
  return(list(cv = cv, gcv = gcv))
}

myCV <- function(x1, y1, span){
  ## x1, y1: two vectors
  ## span: a sequence of values for "span"
  
  m = length(span)
  cv = rep(0, m)
  gcv = rep(0, m)
  for(i in 1:m){
    tmp = onestep_CV(x1, y1, span[i])
    cv[i] = tmp$cv
    gcv[i] = tmp$gcv
  }
  return(list(cv = cv, gcv = gcv))