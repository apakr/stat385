##################################################################################################################
#
# The R code is used to demonstrate the application of the logistic regression models to presidential election data set
#
##################################################################################################################

## Question 1: read data into R
setwd("lab5")
polls2008<-read.csv(file="2008-polls.csv",header=TRUE)
polls2012<-read.csv(file="2012-polls.csv",header=TRUE)
results2008<-read.csv(file="2008-results.csv",header=TRUE)

## Select pollsters that conducted at least five polls 

pollsters20085<-table(polls2008$Pollster)[table(polls2008$Pollster)>=5]
pollsters20125<-table(polls2012$Pollster)[table(polls2012$Pollster)>=5]
subset1<-names(pollsters20085)[names(pollsters20085)%in%names(pollsters20125)]
pollers<-names(pollsters20125)[names(pollsters20125)%in%subset1]
subsamplesID2008<-polls2008[,5]%in%pollers
polls2008sub<-polls2008[subsamplesID2008,]
subsamplesID2012<-polls2012[,5]%in%pollers
polls2012sub<-polls2012[subsamplesID2012,]

## Question 2: define responses, state edges, lag time 
## and reformatting the 2008 poll and true results data set as desired

winers2008<-(results2008[,2]-results2008[,3]>0)+0
StateID2008<-results2008[,1]
Allresponses<-NULL
for (sid in 1:51)
{
  polls2008substate<-polls2008sub[polls2008sub$State==StateID2008[sid],]
  pollwiners2008state<-(polls2008substate[,2]-polls2008substate[,3]>0)+0
  pollwinersIND<-(pollwiners2008state==winers2008[sid])+0
  Allresponses<-c(Allresponses,pollwinersIND)
}
margins<-abs(polls2008sub[,2]-polls2008sub[,3])
lagtime<-rep(0,dim(polls2008sub)[1])
electiondate2008<-c("Nov 04 2008")
for (i in 1:dim(polls2008sub)[1])
{
  lagtime[i]<-as.Date(electiondate2008, format="%b %d %Y")-as.Date(as.character(polls2008sub[i,4]), format="%b %d %Y")
}
dataset2008<-cbind(Allresponses,as.character(polls2008sub[,1]),margins,lagtime,as.character(polls2008sub[,5]))

## Question 3: Focusing on the states with both successes and failures

stateslist<-unique(dataset2008[which(dataset2008[,1]=="0"),2])
subdataset2008<-dataset2008[dataset2008[,2]%in%stateslist,]
resp<-as.integer(subdataset2008[,1])
statesFAC<-as.factor(subdataset2008[,2])
margins<-as.double(subdataset2008[,3])
lagtime<-as.double(subdataset2008[,4])
pollersFAC<-as.factor(subdataset2008[,5])


## Question 4: Fit a logistic regression model without considering states as covariates

logitreg1<-glm(resp~margins+lagtime+pollersFAC,family="binomial")
summary(logitreg1)

## Question 5: reformating the 2012 poll data for prediction purpose

## Create data for prediction

pollwiners2012<-(polls2012sub[,2]-polls2012sub[,3]>0)+0
margins2012<-abs(polls2012sub[,2]-polls2012sub[,3])
lagtime2012<-rep(0,dim(polls2012sub)[1])
electiondate2012<-c("Nov 06 2012")
for (i in 1:dim(polls2012sub)[1])
{
  lagtime2012[i]<-as.Date(electiondate2012, format="%b %d %Y")-as.Date(as.character(polls2012sub[i,4]), format="%b %d %Y")
}
dataset2012<-cbind(pollwiners2012,as.character(polls2012sub[,1]),margins2012,lagtime2012,as.character(polls2012sub[,5]))

## Focusing on the states in the state list of 2008

subdataset2012<-dataset2012[dataset2012[,2]%in%stateslist,]

## Do the prediction for the state "MI" using the logistic regression model in Q4

margins2012<-as.double(subdataset2012[,3])
lagtime2012<-as.double(subdataset2012[,4])
pollersFAC2012<-as.factor(subdataset2012[,5])

NOpolls<-sum(subdataset2012[,2]=="MI")
locations<-which(subdataset2012[,2]=="MI")
MIPredictresults1<-cbind(as.double(subdataset2012[locations,1]),rep(0,NOpolls))
counts<-0
for (i in locations)
{
  counts<-counts+1
  MIdatapoints<-data.frame(margins=margins2012[i], lagtime=lagtime2012[i], pollersFAC=pollersFAC2012[i])
  MIPredictresults1[counts,2]<-predict(logitreg1, MIdatapoints, type="response") 
}

MIprobDemwin1<-MIPredictresults1[,1]*MIPredictresults1[,2]+(1-MIPredictresults1[,1])*(1-MIPredictresults1[,2])
MImeanProbDemwin1<-mean(MIprobDemwin1)
MIprobGopwin1<-(1-MIPredictresults1[,1])*MIPredictresults1[,2]+MIPredictresults1[,1]*(1-MIPredictresults1[,2])
MImeanProbGopwin1<-mean(MIprobGopwin1)
