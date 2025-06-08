## Q1 and Q2: Read data set and delete missing values
CrimeData<- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"), header=FALSE)
# setwd("/Users/pszhong/Library/CloudStorage/Box-Box/UIC Teaching/STAT 385-2023-Fall/Lab Material/Lab 6")
setwd("~/Desktop/stat385/lab8/")

DataInfo<-read.table(file="crime-data-info.txt",header=FALSE)
SampleInfo<-c("State","County","CommCode","CommName","fold")
colnameinfo<-c(SampleInfo,as.character(DataInfo[,1]))
Missingcols<-which(DataInfo[,9]>0)+5
CompleteCrimeData<-CrimeData[,-c(1,2,3,5,Missingcols)]
colnames(CompleteCrimeData)<-colnameinfo[-c(1,2,3,5,Missingcols)]
CompleteCrimeData$ViolentCrimesPerPop<-CompleteCrimeData$ViolentCrimesPerPop-mean(CompleteCrimeData$ViolentCrimesPerPop)

## Q3:  Set the training and test data set
trainset0<-CompleteCrimeData[1:1800,]
testset0<-CompleteCrimeData[1801:1994,]

## Q3: Perform ridge regression

library("MASS")
trainset<-as.data.frame(trainset0[,-1])
lmridge<-lm.ridge(ViolentCrimesPerPop ~., data=trainset, lambda = seq(0,10,length=500))
lmridgecoefs<-lmridge$coef
matplot(seq(0,10,length=500),t(lmridgecoefs),type="l",xlab=expression(lambda),ylab="ridge coefficients estimate")

## Q4: Choose the tuning parameter using cross-validation

dataindex<-sample(nrow(trainset))
folds <- cut(dataindex,breaks=10,labels=FALSE) #Create 10 equally size folds
prederrs<-matrix(0,length(trainset$ViolentCrimesPerPop),800)
for(i in 1:10){
  cvtestset<-which(folds==i,arr.ind=TRUE)
  cvtrainset<-c(1:nrow(trainset))[-cvtestset]
  lmridge1<-lm.ridge(ViolentCrimesPerPop ~., subset=cvtrainset, data=trainset,lambda = seq(0,600,length=800))
  prederrs[cvtestset,]<-as.matrix(trainset[cvtestset,-100])%*%lmridge1$coef-trainset$ViolentCrimesPerPop[cvtestset] 
}
sse.cv<-apply(prederrs,2,norm,type="2")  
plot(seq(0,600,length=800),sse.cv,type="l",lty=2,xlab=expression(lambda),ylab="Prediction Errors")
abline(v=seq(0,600,length=800)[which.min(sse.cv)],col=2)

cv.lam<-seq(0,600,length=800)[which.min(sse.cv)]
lmridge2<-lm.ridge(ViolentCrimesPerPop ~., data=trainset, lambda = cv.lam)
lmridgecoefs2<-lmridge2$coef
predtesterrs<-as.matrix(testset0[,-c(1,101)])%*%lmridgecoefs2-testset0[,101]
sse.testerr<-norm(predtesterrs,type="2")

## Q5: Linear regression model for prediction

lm1<-lm(ViolentCrimesPerPop ~.-1, data=trainset)
lmpredbias<-as.matrix(testset0[,-c(1,101)])%*%lm1$coefficients-testset0[,101]
lmsse<-norm(lmpredbias,type="2")

## Q6: Model selection using both backward and forward algorithms, and choose the model using AIC and BIC

AICmod<-stepAIC(lm1,direction="both",trace=1)
summary(AICmod)
subsetindex<-colnames(testset0)%in%names(AICmod$coefficients)
newdata<-data.frame(as.matrix(testset0[,subsetindex]))
AICpredbias<-predict(AICmod,newdata=newdata)-testset0[,101]
AICmodsse<-norm(AICpredbias,type="2")

## Stepwise BIC selection prediction

BICmod=stepAIC(lm1,k=log(dim(trainset)[1]),trace=1)
summary(BICmod)
subsetindexBIC<-colnames(testset0)%in%names(BICmod$coefficients)
newdataBIC<-data.frame(as.matrix(testset0[,subsetindexBIC]))
BICpredbias<-predict(BICmod,newdata=newdataBIC)-testset0[,101]
BICmodsse<-norm(BICpredbias,type="2")

## Checking prediction

sse.testerr
lmsse
AICmodsse
BICmodsse

sum(subsetindex)
sum(subsetindexBIC)

# Based on the outputs, I'd say choose BIC, but not by a signficant margin. 

## Q8: Find the least angle regression estimate of coefficients	

library("lars")
xmat<-as.matrix(trainset0[,-c(1,101)])
y<-trainset0[,101]
lmLAR<-lars(xmat,y,type="lar")
plot(lmLAR)

## Q9: Find the coefficients having sign changes

coefsigns<-sign(coef(lmLAR))
coefsignsums<-colSums(abs(coefsigns))
coefsignsumabs<-abs(colSums(coefsigns))
signchanges<-(coefsignsums!=coefsignsumabs)
sigchangecols<-which(signchanges==TRUE)
coefsigns[,sigchangecols]
nlam<-dim(coef(lmLAR))[1]
smalltuning<-sum(abs(coef(lmLAR)[9,]))/sum(abs(coef(lmLAR)[nlam,]))
# > smalltuning
# 0.06321938


## Q10: Comparing the LAR estimate with the LASSO estimate

lmLASSO<-lars(xmat,y)
par(mfrow=c(1,2))
plot(lmLAR,xlim=c(0,0.08))
plot(lmLASSO,xlim=c(0,0.08))
coef(lmLASSO)[1:9,]==coef(lmLAR)[1:9,]
# Based on the graphs and looking around the tuning parameter of ~.063, it looks like at feature variable 5 the
# two methods begin to deviate in their solution paths

lmLASSO<-lars(xmat,y)
par(mfrow=c(1,2))
plot(lmLAR)
plot(lmLASSO)
# Looking at the plots we can see that LASSO takes more steps than LAR, as the right sides of the graphs 
# contain different numbers, with LASSO going up to 106 and LAR going up to only 98. 

## Q11: Choose the tuning parameter for LASSO using cross-validation
##install.packages("glmnet")

library("glmnet")
cvfit<-cv.glmnet(xmat,y,alpha=1)
plot(cvfit)
lambest<-cvfit$lambda.min
# > lambest
# [1] 0.0002588822


## Q12: Target Variable Prediction

lassobest<-glmnet(xmat,y,alpha=1,lambda=lambest)
lassobest$beta
x_test <- as.matrix(testset0[,-c(1,101)]) 
y_test <- testset0[,101]
lasso_pred <- predict(lassobest, s = lambest, newx = x_test)
lasso_pred

sse_lasso <- sum((lasso_pred - y_test)^2)
sse_lasso
# 3.573916

sse.testerr
# 2.534359, lower

# Since the sse for ridge is lower than the sse for lasso,
# that means that ridge performed better in terms of prediction
# accuracy.