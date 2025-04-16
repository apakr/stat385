# Al Pakrosnis
# Lab 11
# STAT 385, SP25

## Q1: Read data into R and standardize the data set

setwd("~/Desktop/stat385/lab11")
redwinequality<-read.table(file="winequality-red.csv",head=TRUE,sep=";")
standizedata<-function(x)
{
  stdX<-(x-mean(x))/sqrt(var(x))
  return(stdX)
}
stdredwineX<-apply(redwinequality[,-12],2,standizedata)
stdredwine<-cbind(stdredwineX,redwinequality[,12])
colnames(stdredwine) <- colnames(redwinequality)
stdredwine <- as.data.frame(stdredwine)
stdredwine$quality <- as.factor(stdredwine$quality)

## Q2: Split the sample into training data and test data sets. Perform classification tree using
## training data set and predict it using test data set
##

alpha<-0.8
inTrain<-sample(1:nrow(stdredwine), alpha*nrow(stdredwine))
train.set <- stdredwine[inTrain,]
test.set  <- stdredwine[-inTrain,]

## Grow decision tree to its entirety
## Post pruning the tree by cross-validation
## xval: # of cross-validation

library(rpart)
library(rpart.plot)
rpart.tree.full <- rpart(quality ~ ., data=train.set, control=rpart.control(minsplit = 2, minbucket = 1, cp=0))
bestcp <- rpart.tree.full$cptable[which.min(rpart.tree.full$cptable[,"xerror"]),"CP"]

tree.pruned <- prune(rpart.tree.full, cp = bestcp)
prune.predictions <- predict(tree.pruned, test.set, type="class")

treetable<-table(test.set$quality, prune.predictions)
tree.errate<-(sum(treetable)-sum(diag(treetable)))/sum(treetable)
tree.errate
prp(tree.pruned,type=2,extra=1)


## Q3: Bagging for classification tree (without pruning)
##install.packages("ipred")

library(ipred)
bagtree<-bagging(quality ~., data=train.set,coob=TRUE)
# prune.bagtree<-prune(bagtree)
print(bagtree)

bag.predictions<-predict(bagtree, test.set)

bagtable<-table(test.set$quality, bag.predictions)
bagtable
#   bag.predictions
#     3   4   5   6   7   8
# 3   0   0   3   1   0   0
# 4   0   0   7   4   0   0
# 5   0   0 102  29   0   0
# 6   0   0  30  92  10   0
# 7   0   0   4   9  27   0
# 8   0   0   0   2   0   0
bag.errate<-(sum(bagtable)-sum(diag(bagtable)))/sum(bagtable)
bag.errate
# [1] 0.309375

## Q4: Boosting method

library("adabag")
mfinal <- c(10:15)
maxdepth <- c(3:10)
Errmatrix <- matrix(0,length(mfinal),length(maxdepth))
for (i in 1:length(mfinal))
  for (j in 1:length(maxdepth))
  {
    Iono.adaboost <- boosting(quality ~.,data=train.set,mfinal=mfinal[i], coeflearn="Zhu",
                              control=rpart.control(maxdepth=maxdepth[j]))
    Iono.adaboost.pred <- predict.boosting(Iono.adaboost,newdata=test.set)
    Errmatrix[i,j] <- Iono.adaboost.pred$error
  }
hist(Errmatrix)

Errmatrix

## Q5: Random forest method

library(randomForest)
num.var <- 11

## Choose m=sqrt(p) feature variables
rf.mod.sqrt <- randomForest(quality ~ ., data = train.set,
                            mtry = floor(sqrt(num.var - 1)), # only difference from bagging is here
                            ntree = 300,
                            proximity = TRUE,
                            importance = TRUE)

## Choose m=p/2 feature variables
rf.mod.half <- randomForest(quality ~ ., data = train.set,
                            mtry = floor((num.var - 1)/2), # only difference from bagging is here
                            ntree = 300,
                            proximity = TRUE,
                            importance = TRUE)

bagging.mod<-randomForest(quality ~ ., data = train.set,
                          mtry = num.var - 1, # try all variables at each split, except the response variable
                          ntree = 300,
                          proximity = TRUE,
                          importance = TRUE)

# Out-of-bag (OOB) error rate as a function of num. of trees:
plot(rf.mod.sqrt$err.rate[,1], type = "l", lwd = 3, lty=1, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")
lines(rf.mod.half$err.rate[,1],lwd=3, lty=2, col="red")
lines(bagging.mod$err.rate[,1],lwd=3, lty=3, col="green")
legend("topright",c("m=sqrt(p)","m=p/2","m=p(bagging)"),col=c("blue","red","green"), lty=c(1,2,3))


# Prediction on the test data set
rf.pred.half <- predict(rf.mod.half,subset(test.set,select = -quality), type="class")

# confusion matrix
print(rf.pred.results <- table(rf.pred.half, test.set$quality))
# Accuracy of our RF model:
rf.err <- 1 - sum(diag(rf.pred.results)) / sum(rf.pred.results)

## Q6 Prediction Comparison

# Classification Tree from Q2
tree.errate
# [1] 0.4

# Bagging method from Q3
bag.errate
# [1] 0.309375

# Boosting method form Q4
Errmatrix

# Random Forest method from Q5




