## Set working directory
setwd("~/Desktop/stat385/lab12/email spam/")

# Question 1

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
# [1] 4601   58
columnames=c(allfeatures,"SpamOrNot")
colnames(Spamdata)=columnames

## Delete the 55th-57th feature variables and create a new data set for Lab 3

newSpamdata=Spamdata[,-c(55:57)]
nnlen=dim(newSpamdata)[2]

## Find the averages frequencies of words or characters in spam and good emails
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

# Question 2

## Extract the top four features ranked by the average frequencies

georgehp=(newSpamdata$george+newSpamdata$hp)/2
youyour=(newSpamdata$you+newSpamdata$your)/2
newSpamdatasub=cbind(georgehp,youyour,newSpamdata$SpamOrNot)
par(mfrow=c(1,1))
y=newSpamdata$SpamOrNot
plot(newSpamdatasub[,1:2], col=y+1, pch=y+1, xlab="georgehp", ylab="youyour")

# Question 3

## Fit a linear SVM to data and find the separation line
newSpamdatasub0<-data.frame(group=as.factor(newSpamdata$SpamOrNot),X1=newSpamdatasub[,1], X2=newSpamdatasub[,2])
library("e1071")
svmmodel=svm(group~., data=newSpamdatasub0, kernel="linear",scale=F,cost=100)
print(svmmodel)

beta=drop(t(svmmodel$coefs)%*%as.matrix(newSpamdatasub0[svmmodel$index,c(2:3)]))
beta0=svmmodel$rho
plot(newSpamdatasub0[,2:3], col=y+1, pch=y+1, xlab="georgehp", ylab="youyour")
abline(a=beta0/beta[2], b=-beta[1]/beta[2],col=3)
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 3,col=3)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 3,col=3)
legend("topright", legend=c("Non-spam","Spam"),col=c(1,2), pch=c(1,2))

## Evaluate performance 20 times
set.seed(385)
df <- NULL
for (i in 1:20) {
  train = sample(1:dim(newSpamdatasub0)[1], 4000)
  svmmodeltrain = svm(group ~ ., data = newSpamdatasub0, subset = train, kernel = "linear", scale = F, cost = 100)
  predvals = predict(svmmodeltrain, newdata = newSpamdatasub0[-train,], decision.values = T)
  truth = newSpamdatasub0[-train, 1]
  res = c(table(truth, predvals)[1,2], table(truth, predvals)[2,1])
  df = rbind(df, res)
}
cat("Average false positives (linear):", mean(df[,1]))
# Average false positives (linear): 66.95
cat("Average false negatives (linear):", mean(df[,2]))
# Average false negatives (linear): 70.95

# Question 4

## Tune hyperparameters for polynomial and RBF kernels using cross-validation
set.seed(385)

# Polynomial tuning
poly_tune <- tune(svm, group ~ ., data = newSpamdatasub0, kernel = "polynomial",
                  ranges = list(cost = c(0.1, 1, 10), degree = c(2, 3, 4)), scale = FALSE)
summary(poly_tune)  # Best: cost = 10, degree = 2

# RBF tuning
rbf_tune <- tune(svm, group ~ ., data = newSpamdatasub0, kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10), gamma = c(0.01, 0.1, 1)), scale = FALSE)
summary(rbf_tune)  # Best: cost = 10, gamma = 1

## Evaluate tuned models 20 times using best parameters from tuning
fp_poly <- fn_poly <- fp_rbf <- fn_rbf <- numeric(20)

set.seed(385)
for (i in 1:20) {
  train = sample(1:nrow(newSpamdatasub0), 4000)
  test_indices = setdiff(1:nrow(newSpamdatasub0), train)
  truth = newSpamdatasub0[test_indices, 1]

  # Polynomial using tuned parameters: cost=10, degree=2
  poly_model <- svm(group~., data=newSpamdatasub0, subset=train, kernel="polynomial",
                    cost=10, degree=2, scale=FALSE)
  poly_preds <- predict(poly_model, newdata=newSpamdatasub0[test_indices,])
  poly_conf <- table(truth, poly_preds)
  fp_poly[i] <- ifelse("0" %in% rownames(poly_conf) & "1" %in% colnames(poly_conf), poly_conf["0","1"], 0)
  fn_poly[i] <- ifelse("1" %in% rownames(poly_conf) & "0" %in% colnames(poly_conf), poly_conf["1","0"], 0)

  # RBF using tuned parameters: cost=10, gamma=1
  rbf_model <- svm(group~., data=newSpamdatasub0, subset=train, kernel="radial",
                   cost=10, gamma=1, scale=FALSE)
  rbf_preds <- predict(rbf_model, newdata=newSpamdatasub0[test_indices,])
  rbf_conf <- table(truth, rbf_preds)
  fp_rbf[i] <- ifelse("0" %in% rownames(rbf_conf) & "1" %in% colnames(rbf_conf), rbf_conf["0","1"], 0)
  fn_rbf[i] <- ifelse("1" %in% rownames(rbf_conf) & "0" %in% colnames(rbf_conf), rbf_conf["1","0"], 0)
}

cat("Tuned Polynomial Kernel - Avg FP:", mean(fp_poly), ", Avg FN:", mean(fn_poly))
# Tuned Polynomial Kernel - Avg FP: 40.4 , Avg FN: 115.4
cat("Tuned RBF Kernel - Avg FP:", mean(fp_rbf), ", Avg FN:", mean(fn_rbf))
# Tuned RBF Kernel - Avg FP: 85 , Avg FN: 47.5

