getwd()
setwd("~/Desktop/stat385/lab7")
chromoX<-read.table(file="ChromoXmicroarray.txt",header=TRUE)
BLClabels<-c(rep(1,27),rep(2,20))

## Question 1: Perform t-test to select the most relevant genes for classification

ttests<-function(x,group)
{
  x<-as.numeric(x)
  pvalue<-t.test(x ~ group)$p.value;
  return(pvalue)
}
pvalues<-apply(chromoX[,c(3:49)],1,ttests,group=BLClabels)
smallpvals<-order(pvalues)[1:8]
features8<-chromoX[smallpvals,c(3:49)]

## Perform normalization on two feature variables

normalization<-function(x)
{
  norms<-(x-min(x))/(max(x)-min(x))
  return(norms)
}
microdata8<-apply(features8,1,normalization)

## Question 2: Set training and test data sets

set.seed(20240214)
training <- sample(1:47,30)
test <- c(1:47)[-training]
train.set <- microdata8[training,]
train.label <- BLClabels[training]
test.set <- microdata8[test,]
test.label <- BLClabels[test]

## Question 3 (using built-in function): 10-folds Cross-Validation Using a built-in function

# install.packages("Rfast")
# library(Rfast)
# x <- as.matrix(train.set)
# y <- train.label
# cv.results <- Rfast::knn.cv(folds = NULL, nfolds = 10, stratified = FALSE, seed = 20240214, y = y, x = x, k = c(3:20))
# kvec <- c(3:20)
# plot(kvec,cv.results$crit, type="l")
# selected.k <- kvec[which(cv.results$crit==max(cv.results$crit))]

## Question 3 (CV by hand): Using nearest neighbor method to perform classification; Split the data
## into training and test data sets, and use 10-fold cross-validation method to 
## select the tuning parameter k (the number of nearest neighborhood)

library(class)
nfold<-10
nsample<-ceiling(nrow(microdata8)/nfold)
kvec<-c(3:20)
AccuTab<-matrix(0,length(kvec),nfold)
orderandom<-sample(1:nrow(microdata8), nrow(microdata8))
intfold <- nrow(microdata8)%/%nsample
remainder <- nrow(microdata8)%%nsample
for (j in 1:length(kvec))
{
  for (i in 1:intfold)
  {
    test<-orderandom[(i-1)*nsample+c(1:nsample)]
    train.set.cv<-microdata8[-test,]
    test.set.cv<-microdata8[test,]
    microdata8.pred <- class::knn(train=train.set.cv, test=test.set.cv, cl=BLClabels[-test], k=kvec[j])
    truth<-BLClabels[test]
    AccuTab[j,i] <- sum(truth==microdata8.pred)
  } 
  if (remainder!=0)
  {
    test<-orderandom[intfold*nsample+c(1:remainder)]
    train.set.cv<-microdata8[-test,]
    test.set.cv<-microdata8[test,]
    microdata8.pred <- class::knn(train=train.set.cv, test=test.set.cv, cl=BLClabels[-test], k=kvec[j])
    truth<-BLClabels[test]
    AccuTab[j,nfold] <- sum(truth==microdata8.pred)
  }
}

AveAccuray<-apply(AccuTab,1,sum)/nrow(microdata8)
plot(kvec,AveAccuray,xlab="Value of K",ylab="Classification Accuracy")
hand.selected.k <- kvec[which(AveAccuray==max(AveAccuray))]


## Question 4: evaluate prediction accuracy using test dat set

pred.selected.k <- class::knn(train=train.set, test=test.set, cl=train.label, k=hand.selected.k[1])
TestAccuracy <- table(pred.selected.k,test.label)

## Question 5: use CV to select useful models with three feature variables among 8 feature variables

allcomb <- combn(1:8, 3) ## generate all combinations of submodels
nfold<-10
nsample<-ceiling(nrow(microdata8)/nfold)
kvec <- c(3:10)
AccuArray <- array(0,c(dim(allcomb)[2],nfold,length(kvec)))
orderandom <- sample(1:nrow(microdata8), nrow(microdata8))
intfold <- nrow(microdata8)%/%nsample
remainder <- nrow(microdata8)%%nsample
for (j in 1:dim(allcomb)[2])
{
  for (k in 1:length(kvec))
  {
    
    for (i in 1:intfold) { 
      test <- orderandom[(i-1) * nsample + c(1:nsample)]
      train.set.cv <- microdata8[-test, allcomb[, j]] 
      test.set.cv <- microdata8[test, allcomb[, j]]
      microdata8.pred <- class::knn(train=train.set.cv, test=test.set.cv, cl=BLClabels[-test], k=kvec[k])
      truth <- BLClabels[test]
      AccuArray[j, i, k] <- sum(truth == microdata8.pred)  
    }
    
  }
}

AveAccuracyTable <- apply(AccuArray,c(1,3),sum)/nrow(microdata8)
MaxAccuracyModels <- apply(AveAccuracyTable,1,max)
ModelAndAccuracy <- cbind(t(allcomb), MaxAccuracyModels)

BestModels <- which(ModelAndAccuracy[,"MaxAccuracyModels"] == max(ModelAndAccuracy[,"MaxAccuracyModels"]))
BestModels
# [1] 38

ModelAndAccuracy[38,]
#                                               MaxAccuracyModels 
# 3.0000000         4.0000000         6.0000000         0.9574468 




