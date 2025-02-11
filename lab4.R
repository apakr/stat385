## Q1 and Q2: Read data set and delete missing values
CrimeData<- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/communities/communities.data"), header=FALSE)
setwd("~/Desktop/stat385/")
install.packages("MASS")

DataInfo<-read.table(file="crime-data-info.txt",header=FALSE)
SampleInfo<-c("State","County","CommCode","CommName","fold")
colnameinfo<-c(SampleInfo,as.character(DataInfo[,1]))
Missingcols<-which(DataInfo[,9]>0)+5
CompleteCrimeData<-CrimeData[,-c(1,2,3,5,Missingcols)]
colnames(CompleteCrimeData)<-colnameinfo[-c(1,2,3,5,Missingcols)]
CompleteCrimeData$ViolentCrimesPerPop<-CompleteCrimeData$ViolentCrimesPerPop-mean(CompleteCrimeData$ViolentCrimesPerPop)

## Q3:  Set the training and test data set
trainset0<-CompleteCrimeData[1:1800,-1]
testset0<-CompleteCrimeData[1801:1994,]

## Q4: Linear regression model for prediction

lm1<-lm(ViolentCrimesPerPop ~.-1, data=trainset0)
summary(lm1)
prediction <- predict(lm1, testset0[,-c(1,101)])

errors <- prediction - testset0[,101]
errors

sqrt(sum(errors^2))
# 1.9244


## Q5: Model selection using both backward and forward algorithms, and choose the model using AIC and BIC

library(MASS)
AICmod<-stepAIC(lm1,direction="both",trace=FALSE)
summary(AICmod)

subsetindex<-colnames(testset0)%in%names(AICmod$coefficients)
newdata<-data.frame(as.matrix(testset0[,subsetindex]))
AICpredbias<-predict(AICmod,newdata=newdata)-testset0[,101]
AICmodsse<-norm(AICpredbias,type="2")
AICmodsse
# 1.915227
  
# The error for the linear regression model is 1.9244. The error for the AIC is 1.915227. The second method gives the lowest prediction error. 
# This is because it excludes less significant variables and only includes the most informative ones, thus reducing the error and providing a better fit.

getwd()


