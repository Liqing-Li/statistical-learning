############# Regression Tree #############
library(ggplot2)
library(rpart)
library(rpart.plot)
library(tree) 
load("~/Dropbox/R code-referene/statitical learning/statistical learning/Data/BostonHousing1.Rdata")  # data: Housing1

#There are two R packages for tree models, tree and rpart. We will mainly use rpart.
#The package tree is called for its command  partition.tree, which we use to generate the first figure.

###Fit a regression tree using just two predictors
trfit= tree(Y ~ lon + lat, data=Housing1)
small.tree = prune.tree(trfit, best=7)
small.tree

par(mfrow=c(1,2))
plot(small.tree)
text(small.tree, cex=.75) #add text to a plot

price.quantiles = cut(Housing1$Y, quantile(Housing1$Y, 0:20/20),
                      include.lowest=TRUE)
plot(Housing1$lat, Housing1$lon, col=grey(20:1/21)[price.quantiles],
     pch=20, ylab="Longitude", xlab="Latitude") #darker dots, more expensive
partition.tree(small.tree, ordvars=c("lat","lon"), add=TRUE)
detach("package:tree")


###Fit a regression tree using all predictors

set.seed(1234)
tr1 = rpart(Y ~ ., data = Housing1)
par(mfrow=c(1,2))
plot(tr1)
rpart.plot(tr1)

##Prunning
printcp(tr1)
#rel error is a relative error, using root note error as reference
# xerror-cv error
prune(tr1, cp=0.3) #scale version of alpha, penalty

prune(tr1, cp=0.2)

prune(tr1, cp=0.156)

# long and detailed output on each node of the tree
# summary(tr1) 

plotcp(tr1)

##Not sure whether xerror has reached the bottom. Let’s start with a bigger tree.
tr2 = rpart(Y ~ ., data = Housing1, 
            control = list(cp = 0, xval = 10))
plot(tr2)

printcp(tr2)

plotcp(tr2)

# get index of CP with lowest xerror
opt = which.min(tr2$cptable[, "xerror"])  # 28
# get the optimal CP value
tr2$cptable[opt, 1]

# upper bound for equivalent optimal xerror
tr2$cptable[opt, 4] + tr2$cptable[opt, 5]

# row IDs for CPs whose xerror is equivalent to min(xerror)
tmp.id = which(tr2$cptable[, 4] <= tr2$cptable[opt, 4] +
                 tr2$cptable[opt, 5])
# CP.1se = any value between row (tmp.id) and (tmp.id-1)
CP.1se = 0.0032

# Prune tree with CP.1se
tr3 = prune(tr2, cp = CP.1se)


##Connection between α and CP
#RSS(T)+α|T|,RSS(T)/RSS(root)+CP⋅|T|.

#Understand the relationship between the 1st column and the 3rd column of the CP table.
cbind(tr2$cptable[, 1], c(-diff(tr2$cptable[, 3]), 0))
# when improvement of RSS is enough to pay the cost (>CP), do one more slip.
#Prediction
#?predict.rpart  # check use the fitted tree to do prediction

###Handle categorical predictors###

set.seed(1234)
n = nrow(Housing1); 
m = 30 
X = as.factor(sample(1:m, n, replace = TRUE))
tmp = data.frame(Y = Housing1$Y, X = X)
myfit = rpart(Y ~ X, data = tmp)
myfit

group.mean = as.vector(tapply(tmp$Y, tmp$X, mean))
order(group.mean)

group.mean[order(group.mean)] #same as sort(group.mean)
## with one categorical predictor, no need to update group mean, this is not true if you have more than one predictor

## using categorical +mumerical variables as pridictor
#* 2018-1_STAT542_5.7 Handle Categorical Predictors
tmp$Z= rnorm(n)  # add a numerical feature
myfit = rpart(Y ~ X + Z, data = tmp)
rpart.plot(myfit)

myfit

id1 = which(! (tmp$X %in% c(1, 22, 25, 26, 6, 21)))
length(id1)  # 419


id2 = id1[which(tmp$Z[id1] > 0.95)]
length(id2)  # 54

group.mean = as.vector(tapply(tmp$Y[id2], tmp$X[id2], mean))
order(group.mean)

group.mean



################
