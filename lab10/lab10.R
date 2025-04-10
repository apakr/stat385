## Set working directory
setwd("~/Desktop/stat385/lab10/email spam/")

## Read data sets
features=scan(file="features.txt", what="character")

## Reform data sets
len_features=length(features)
nlen=len_features/2
featureframe=data.frame(feature=features[2*(1:nlen)-1],type=features[2*(1:nlen)])

## Extract the first feature
featureframe[1,1]
feature1vec=strsplit(as.character(featureframe[1,1]),split="_")
feature1=feature1vec[[1]][3]
feature1F=strsplit(feature1,split=":")[[1]][1]

## Extract the 55th feature
featureframe[55,1]
feature55vec=strsplit(as.character(featureframe[55,1]),split="_")
feature55=feature55vec[[1]][4]
feature55F=strsplit(feature55,split=":")[[1]][1]

## Extract all the features
allfeatures=matrix(0,nlen,1)
for (i in 1:(nlen-3))
{
  feature1vec=strsplit(as.character(featureframe[i,1]),split="_")
  feature1=feature1vec[[1]][3]
  feature1F=strsplit(feature1,split=":")[[1]][1]
  allfeatures[i]=feature1F
}
for (i in (nlen-2):nlen)
{
  feature55vec=strsplit(as.character(featureframe[i,1]),split="_")
  feature55=feature55vec[[1]][4]
  feature55F=strsplit(feature55,split=":")[[1]][1]
  allfeatures[i]=feature55F
}

## Spam data set: combining feature names with the relative frequencies; Organize them into an appropriate format

Spamdata=read.table(file="spam.data.txt")
dim(Spamdata)
columnames=c(allfeatures,"SpamOrNot")
colnames(Spamdata)=columnames

## Delete the 55th-57th feature variables and create a new data set for Lab 3

newSpamdata=Spamdata[,-c(55:57)]
nnlen=dim(newSpamdata)[2]

## Q1: Find the averages frequencies of words or characters in spam and good emails
Spamemails=newSpamdata[newSpamdata$SpamOrNot==1,]
NonSpams=newSpamdata[newSpamdata$SpamOrNot==0,]
SpamAves=colMeans(Spamemails)
NonSpamAves=colMeans(NonSpams)
AbsDifferences=abs(SpamAves-NonSpamAves)
SortbyAbsDifferences=sort(AbsDifferences[-(nnlen)],decreasing=TRUE)
indexafterorder=order(AbsDifferences[-(nnlen)],decreasing=TRUE)
summaryinfo=rbind((SpamAves[-(nnlen)])[indexafterorder],(NonSpamAves[-(nlen)])[indexafterorder],SortbyAbsDifferences)
rownames(summaryinfo)=c("spam","email","abs diff")

## Bar plot for the average frequcies of words or characters in spam or non-spam

barplotinfo1=summaryinfo[1:2,1:27]
barplotinfo2=summaryinfo[1:2,28:54]
par(mfrow=c(2,1))
barplot(barplotinfo1, main="Average frequencies of features by spam and non-spam",
        xlab="Feature words or characters", col=c("darkblue","red"),
        legend = rownames(barplotinfo1), beside=TRUE, ylim=c(0,2.3))
barplot(barplotinfo2, xlab="Feature words or characters", col=c("darkblue","red"),
        legend = rownames(barplotinfo2), beside=TRUE, ylim=c(0,2.3))

## Q2: Extract the top four features ranked by the average frequencies

georgehp=(newSpamdata$george+newSpamdata$hp)/2
youyour=(newSpamdata$you+newSpamdata$your)/2
newSpamdatasub=cbind(georgehp,youyour,newSpamdata$SpamOrNot)
par(mfrow=c(1,1))
y=newSpamdata$SpamOrNot
plot(newSpamdatasub[,1:2], col=y+1, pch=y+1, xlab="georgehp", ylab="youyour")

## Define a function that can compute entropy gain for given feature variable, class label and threshold value

EntropyIG<-function(threshold,feature,classlabel,unit=exp(1))
{
  nlen<-length(classlabel)
  probs0<-table(classlabel)/nlen
  entropyparent<-sum(-probs0*log(probs0,base=unit))
  featurecat<-(feature<threshold)*1+0
  Classtable<-table(featurecat,classlabel)
  props<-rowSums(Classtable)/sum(Classtable)
  probs<-Classtable/rowSums(Classtable)
  entropyele<-probs^(-probs)
  entropyprod<-apply(entropyele,1,prod)
  logentropy<-log(entropyprod,base=unit)
  entropychild<-sum(props*logentropy)
  IG<-entropyparent-entropychild
  return(IG)
}

newSpamdatasub0<-data.frame(SpamOrNot=newSpamdata$SpamOrNot,georgehp=newSpamdatasub[,1], youyour=newSpamdatasub[,2])
#newSpamdatasub0<-newSpamdatasub[,-1]

## Q3: Compute information gain for both features "georegehp" and "youyour" at a sequence of threholds

th1seq<-matrix(seq(0,max(newSpamdatasub0$georgehp),length=500),500,1)
IGgeorgehp<-apply(th1seq,1,EntropyIG,feature=newSpamdatasub0$georgehp,classlabel=newSpamdatasub0$SpamOrNot)
th1IGs<-cbind(th1seq,IGgeorgehp)
plot(th1seq,IGgeorgehp,type="l",xlab="Threshold for feature georgehp",ylab="Entropy Information Gain")

th2seq<-matrix(seq(0,max(newSpamdatasub0$youyour),length=500),500,1)
IGyouyour<-apply(th2seq,1,EntropyIG,feature=newSpamdatasub0$youyour,classlabel=newSpamdatasub0$SpamOrNot)
th2IGs<-cbind(th2seq,IGgeorgehp)
plot(th2seq,IGyouyour,type="l",xlab="Threshold for feature youyour",ylab="Entropy Information Gain")

max(IGyouyour)
max(IGgeorgehp)

## Q4: Apply the classification tree to the data set newSpamdatasub0
library(rpart)
library(rpart.plot)
treefit<-rpart(SpamOrNot~., data=newSpamdatasub0, method="class", parms=list(split = "information"),control = rpart.control(minsplit=10))
treefit$split
prp(treefit,type=2,extra=1)

## Q5: Partition the feature space according to the above classification tree

th1seq<-matrix(seq(0,2,length=100),100,1)
th2seq<-matrix(seq(0,4,length=100),100,1)
xgrid<-expand.grid(georgehp=th1seq,youyour=th2seq)
predtions<-predict(treefit,newdata=xgrid,type="prob")[,2]
ygrid<-(predtions>0.5)*1+0
plot(xgrid, col=as.numeric(ygrid)+2, pch = 20, cex = .2,xlim=c(0,2),ylim=c(0,4))
points(newSpamdatasub0$georgehp,newSpamdatasub0$youyour,pch=20, cex=1.2,col=newSpamdatasub0$SpamOrNot+2)
contour(th1seq,th2seq,matrix(predtions,100,100),levels = .5,add=TRUE)

## Q6: Fit the classification tree by present minsplit=10 and cost-complexity parameter=0.03

treefit1<-rpart(as.factor(SpamOrNot)~., data=newSpamdatasub0, method="class", parms=list(split = "information"),control = rpart.control(minsplit=10,cp=0.03))
printcp(treefit1)
par(mfrow=c(2,1))
prp(treefit1,type=2,extra=1)
plotcp(treefit1)

newemail <- data.frame(georgehp = 0.175, youyour = 0)
pred_label <- predict(treefit1, newemail, type = "class")
pred_label
# 1
# 0
# Levels: 0 1

# If an email has 6 dollar-signs among
# 100 characters and the word “hp” appeared 7 times among 20 words, what is the
# predicted class label for this email (spam or non-spam) based on the produced
# classification tree?

# If an email had these characteristics, based on the results from our prediction it would be classified as
# a non-spam email.


## Q7: Cost-complexity parameter verification

cptable <- treefit1$cptable
cptable
#          CP nsplit rel error    xerror       xstd
# 1 0.2294539      0 1.0000000 1.0000000 0.01828190
# 2 0.2266961      1 0.7705461 0.8257033 0.01752862
# 3 0.0300000      2 0.5438500 0.5521236 0.01543634

cp_check1 <- (cptable[1, "rel error"] - cptable[2, "rel error"]) /
  (cptable[2, "nsplit"] - cptable[1,"nsplit"])
cp_check1 # 0.2294539


cp_check2 <- (cptable[2, "rel error"] - cptable[3, "rel error"]) /
  (cptable[3, "nsplit"] - cptable[2,"nsplit"])
cp_check2 # 0.2266961



## Q8: Post-pruning of the classification tree

treefit2<- rpart(as.factor(SpamOrNot)~ ., data=newSpamdatasub0, control=rpart.control(minsplit = 2, minbucket = 1, cp=0))
bestcp <- treefit2$cptable[which.min(treefit2$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(treefit2, cp = bestcp)
prp(tree.pruned,type=2,extra=1)
predvals<-predict(treefit2,newdata=newSpamdatasub0, type="class")
truth<-newSpamdatasub0$SpamOrNot
table(truth,predvals)

## Q9: Preference adjustment
lmat <- matrix(c(0,1,10,0), ncol = 2)
lmat
#       [,1] [,2]
# [1,]    0   10
# [2,]    1    0

## Fit classification tree with loss matrix
treefit_weighted <- rpart(as.factor(SpamOrNot) ~ ., data = newSpamdatasub0,
                          parms = list(loss = lmat),
                          control = rpart.control(minsplit = 2, minbucket = 1, cp = 0))

## Find the best cp and prune the tree
bestcp_weighted <- treefit_weighted$cptable[which.min(treefit_weighted$cptable[, "xerror"]), "CP"]
tree_weighted_pruned <- prune(treefit_weighted, cp = bestcp_weighted)

## Plot the weighted (cost-sensitive) classification tree
prp(tree_weighted_pruned, type = 2, extra = 1, main = "Cost-Sensitive Pruned Tree")

## Predict with the weighted tree
predvals_weighted <- predict(tree_weighted_pruned, newdata = newSpamdatasub0, type = "class")
truth <- newSpamdatasub0$SpamOrNot

## Confusion matrix for weighted tree
confmat_weighted <- table(Actual = truth, Predicted = predvals_weighted)

## Confusion matrix for unweighted tree (from Q8)
predvals_unweighted <- predict(tree.pruned, newdata = newSpamdatasub0, type = "class")
confmat_unweighted <- table(Actual = truth, Predicted = predvals_unweighted)

## Print and compare
cat("Unweighted Tree Confusion Matrix:\n")
print(confmat_unweighted)

cat("\nWeighted Tree Confusion Matrix (Cost-Sensitive):\n")
print(confmat_weighted)
