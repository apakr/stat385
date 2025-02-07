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

# need code for evaluating the errors
# to do that we need to do the prediction, minus prediction(lm1, testset) - actual values
# i need to write code like the stuff in q5 for here the stuff in q4, for all the variables, but we want the mean squared error for this

# you got an errors vector, which is actual minus predicted
# sqrt(sum(errors^2)) - gives you the errors

# you can be fancy and just use the norm function like in part 5. if you do type = 2 it squares them, adds them all up together, then square roots them


## Q5: Model selection using both backward and forward algorithms, and choose the model using AIC and BIC

library(MASS)
AICmod<-stepAIC(lm1,direction="both",trace=1)
summary(AICmod)
subsetindex<-colnames(testset0)%in%names(AICmod$coefficients)
newdata<-data.frame(as.matrix(testset0[,subsetindex]))
AICpredbias<-predict(AICmod,newdata=newdata)-testset0[,101]
AICmodsse<-norm(AICpredbias,type="2")


getwd()


