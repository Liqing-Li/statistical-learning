###Multidimensional Scaling
# Letter Recognition and Confusion.The table below is from Wolford and Hollingsworth (1974),
# where each entry shows the frequency with which each letter was mistakenly called something else.

D = matrix(0, 8, 8)
letters = c("C", "D", "G", "H", "M", "N", "Q", "W")
colnames(D) = letters
rownames(D) = letters
D[2:8, 1] = c(5, 12, 2, 2, 2, 9, 1)
D[3:8, 2] = c(2, 4, 3, 4, 20, 5)
D[4:8, 3] = c(3, 2, 1, 9, 2)
D[5:8, 4] = c(19, 18, 1, 5)
D[6:8, 5] = c(16, 2, 18)
D[7:8, 6] = c(8, 13)
D[8, 7] = 4
D = (D+t(D))
D

#Change the similarity matrix to distance matrix.
D0 = 21 - D # distance matrix now
diag(D0) = 0
tmp = cmdscale(D0) # Multi dentional scaling 
par(mfrow=c(1,2))
plot(tmp[, 1], tmp[, 2], type="n", xlab="", ylab="", 
     xlim = c(-15, 15), ylim=c(-15, 15))
text(tmp[, 1], tmp[, 2], label = letters)

D1 = 41 - D # another way of changing similarity to distance
diag(D1) = 0
tmp = cmdscale(D1)
plot(tmp[, 1], tmp[, 2], type="n", xlab="", ylab="", 
     xlim = c(-20, 20), ylim=c(-20, 20))
text(tmp[, 1], tmp[, 2], label = letters)

#### K-means #######

## Run K-means with K=2 and 3.
source("sportsranks.txt");
head(sportsranks)

km2=kmeans(sportsranks, centers=2, nstart=10);
km3=kmeans(sportsranks, centers=3, nstart=10);

## View the clusters in MDS plots.
D=dist(sportsranks)
Xmds=cmdscale(D);

par(mfrow=c(1,2));
plot(Xmds[,1], Xmds[,2], type="n", xlab="", ylab="")
points(Xmds[,1], Xmds[,2], pch=km2$cluster, col=km2$cluster);
title("MDS plot for K=2")
plot(Xmds[,1], Xmds[,2], type="n", xlab="", ylab="");
points(Xmds[,1], Xmds[,2], pch=km3$cluster, col=km3$cluster);
title("MDS plot for K=3")

#How is cMDS computed? show how cmdscale() calculte it
D2 = as.matrix(D^2)
n = dim(D2)[1]
tmp = D2 - matrix(colMeans(D2), nrow=n, ncol=n, byrow=TRUE) - 
  matrix(rowMeans(D2), nrow=n, ncol=n, byrow=FALSE)
tmp = -(tmp + mean(D2))/2
tmp.svd = svd(tmp)
F = tmp.svd$u[, 1:2]  %*% diag(sqrt(tmp.svd$d[1:2]))
cor(F, Xmds)

### Gap Statistics (to choose the right k)

##You can use clusGap from cluster package to compute gap statistic.
library(cluster)
set.seed(123)
gap_stat = clusGap(sportsranks, FUN = kmeans, nstart = 25,
                   K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 2, lty = 2)

## Next we compute gap statistic step by step.
#*reference: Video: 2018-1_STAT542_7.4.2_Gap Statistics- Cluster_Choice_of_K_Gap
#Step 1. Calculate Within-Cluster-SS for different K
n=130;
SS=rep(0,10)
SS[1]=sum(diag(var(sportsranks)))*(n-1);
for(k in 2:10){
  kms=kmeans(sportsranks, centers=k, nstart=10);
  SS[k]=sum(kms$withinss);
}
lss=log(SS);

#Step 2. Calculate SS_0(K) under the null hypothesis
n = dim(sportsranks)[1] 
m = 7  # each review is a permutation of numbers from 1 to 7
B = 50 # number of iterations
lss0 = matrix(0,B,10)
for(b in 1:B){
  xstar=NULL
  for(i in 1:n){
    xstar = rbind(xstar, sample(m))
  }
  lss0[b,1] = log(sum(diag(var(xstar)))*(n-1))
  for(k in 2:10){
    kms = kmeans(xstar, centers=k, nstart=10)
    lss0[b,k] = log(sum(kms$withinss))
  }
}

plot(1:10, lss, type="l", col="red", xlab="k", 
     ylab="log(Within SS)");
for(b in 1:B){
  points(1:10, lss0[b,], type="l", col=8)
}

##Step 3. Gap curve. Based on the 1se rule, the optimal K = 2.
lss0m = apply(lss0, 2, mean);
lss0sd = sqrt(apply(lss0, 2, var))*sqrt(1+1/B);
diff = lss0m-lss;
matplot(1:10, cbind(diff, diff+lss0sd, diff-lss0sd), type="l",
        xlab="k", ylab="Gap")
points(1:10, diff);
points(1:10, diff-lss0sd, pch="+", col="green");

#### Silhouette Plots#
# the larger the (average) silhouette, the better the cluster
# s(i) close to 1 -well classified, 0 is lies between two cluster, -1 then objet is badly classified
sil2=silhouette(km2$cluster,D);
sil3=silhouette(km3$cluster,D);
par(mfrow=c(1,2))
plot(sil2)
plot(sil3)

#Compute and plot average silhouette over a range of K values.
k.max = 10
sil = rep(0, k.max)

# Compute the average silhouette width for 
# k = 2 to k = 15
for(i in 2:k.max){
  tmp = kmeans(sportsranks, centers = i, nstart = 10)
  ss <- silhouette(tmp$cluster, dist(sportsranks))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:k.max, sil, type = "b", pch = 19, 
     frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)


###Prediction Strength
# measures the worst performance of predicting pairwise co-membership

library(fpc)
ps = prediction.strength(sportsranks, Gmax=10,
                         clustermethod=kmeansCBI)
plot(1:10, ps$mean.pred, type='b', ylim=c(0,1), 
     xlab='Number of clusters', ylab='Prediction strength')
abline(h=ps$cutoff, col="red", lty=2, lwd=2)

###PAM (partition around medoids)
pam2=pam(sportsranks, k=2);
plot(pam2)

pam3=pam(sportsranks, k=3);
plot(pam3)

par(mfrow=c(1,2));
plot(Xmds[,1], Xmds[,2], type="n");
points(Xmds[,1], Xmds[,2], pch=pam2$clustering, col=pam2$clustering);
title("MDS plot for K=2")
plot(Xmds[,1], Xmds[,2], type="n");
points(Xmds[,1], Xmds[,2], pch=pam3$clustering, col=pam3$clustering);
title("MDS plot for K=3")

# compare with Kmeans
table(pam2$clustering, km2$cluster)
table(pam3$clustering, km3$cluster)


### Hierarchical Clustering (bottom-up)
plot(hclust(dist(sportsranks)), xlab="Complete Linkage", sub="");

plot(hclust(dist(sportsranks), method="single"), xlab="Single Linkage", sub="");

plot(hclust(dist(sportsranks), method="average"), xlab="Average Linkage", sub="");

# Do not need to specify K at begining, but clustering results are nested
clusters = hclust(dist(sportsranks), method="average")
clusterCut = cutree(clusters, 3)
table(clusterCut)

#Check how hclust results differ from the ones from K-means.
table(cutree(clusters, 2), km2$cluster)
table(cutree(clusters, 3), km3$cluster)
clusters = hclust(dist(sportsranks))
table(cutree(clusters, 2), km2$cluster)
table(cutree(clusters, 3), km3$cluster)
