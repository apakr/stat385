#################### Read Data into R from local file #######################

getwd()
setwd("/home/allen/Desktop/stat385/lab6")
chromoX<-read.table(file="ChromoXmicroarray.txt",header=TRUE)
BLClabels<-c(rep(1,27),rep(2,20))

############################################################################
#
# Question 1: Perform t-test to select the most relevant genes for classification
# 
############################################################################

ttests<-function(x,group)
{
  x<-as.numeric(x)
  pvalue<-t.test(x ~ group)$p.value;
  return(pvalue)
}
pvalues<-apply(chromoX[,c(3:49)],1,ttests,group=BLClabels)
smallpvals<-order(pvalues)[1:2]
features2<-chromoX[smallpvals,c(3:49)]

plot(t(features2),col=BLClabels,pch=BLClabels)
legend("topleft",c("non-BLC","BLC"),col=c(1,2),pch=c(1,2))


# Perform normalization on two feature variables, and define training and
# test data sets


normalization<-function(x)
{
  norms<-(x-min(x))/(max(x)-min(x))
  return(norms)
}
microdata2<-apply(features2,1,normalization)
microdata2<-data.frame(gene20475=microdata2[,1],gene20947=microdata2[,2],group=BLClabels)


#############################################################################
#
# Question 2: select training and test data sets
#
#############################################################################

set.seed(20232024) 
train.set <- sample(c(1:47), 30) 
training.dat <- microdata2[train.set,]
test.dat <- microdata2[-train.set,]


#############################################################################
#
# Question 3: using LDA, Naive Bases, and nearest neighbor method to perform classifications
# 
############################################################################

## LDA method for classification
library(MASS)
z<-lda(group~ ., microdata2,prior=c(1/2,1/2),subset=train.set)

## LDA step-by-step
groupmeans<-aggregate(training.dat[,1:2], list(training.dat$group), mean)
group1<-(training.dat$group==1)
group2<-(training.dat$group==2)
S1<-cov(training.dat[group1,1:2])
S2<-cov(training.dat[group2,1:2])
n1<-length(group1)
n2<-length(group2)
Spooled<-((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
invSn<-solve(Spooled)
ahat<-as.numeric(groupmeans[1,2:3]-groupmeans[2,2:3])%*%invSn
mhat<-ahat%*%as.numeric(groupmeans[1,2:3]+groupmeans[2,2:3])/2

## Naive Bayes for classification (without smoothing) 
## install.packages("e1071")
library(e1071)
nbmodel <- naiveBayes(group ~ ., data = microdata2, subset=train.set) 

## KNN method for classification
library(class)
knn.pred3<- knn(train=training.dat[,1:2], test=test.dat[,1:2], cl=training.dat[,3], k=3)
knn.pred8<- knn(train=training.dat[,1:2], test=test.dat[,1:2], cl=training.dat[,3], k=8)

############################################################################
#
# Question 4: Plot the separation curves for LDA, KNN and NB
# 
############################################################################

par(mfrow=c(2,2))

## separation line for LDA
plot(training.dat[,1:2], col=as.numeric(training.dat[,3])+1, pch = 19, main="LDA method")
abline(a=mhat/ahat[2],b=-ahat[1]/ahat[2])

## separation curve for NB
px1<-seq(min(microdata2$gene20475),max(microdata2$gene20475),length=100)
px2<-seq(min(microdata2$gene20947),max(microdata2$gene20947),length=100)
xgrid<-expand.grid(gene20475=px1,gene20947=px2)
nbpred <- predict(nbmodel, xgrid) 
plot(xgrid, col = as.numeric(nbpred)+1, pch = 20, cex = .2, main="NB method")
points(training.dat[,1:2], col=as.numeric(training.dat[,3])+1, pch = 19)

## separation plane for KNN (k=3)
knn.pred.grid3 <- knn(train=training.dat[,1:2], test=xgrid, cl=training.dat[,3], k=3)
plot(xgrid, col = as.numeric(knn.pred.grid3)+1, pch = 20, cex = .2, main="KNN k=3")
points(training.dat[,1:2], col=as.numeric(training.dat[,3])+1, pch = 19)

## separation plane for KNN (k=8)
knn.pred.grid8 <- knn(train=training.dat[,1:2], test=xgrid, cl=training.dat[,3], k=8)
plot(xgrid, col = as.numeric(knn.pred.grid8)+1, pch = 20, cex = .2, main="KNN k=8")
points(training.dat[,1:2], col=as.numeric(training.dat[,3])+1, pch = 19)


############################################################################
#
# Question 5: predictions using LDA, naive Bayes and KNN
# 
############################################################################

## prediction using LDA
truth <- test.dat[,3]
predgroups<-predict(z,microdata2[-train.set,])$class
table(truth,predgroups)

## prediction using Naive Bayes
nbpred.test <- predict(nbmodel, test.dat)
table(truth, nbpred.test)

## prediction using KNN (k=3)
table(truth, knn.pred3)

## prediction using KNN (k=8)
table(truth, knn.pred8)
