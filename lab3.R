setwd("~/Desktop/stat385/")

## Question 1: Load the data

load(file="Alldata.Rdata")

## Find the average of the gene expressions of gene with IDENTIFIER="RFC2" as responses

subsetGenes<-sapply(Alldata$originaldata[Alldata$originaldata[,2]=="RFC2",3:110], as.numeric)
averageRFC2<-colMeans(subsetGenes)
resp <- averageRFC2

## Question 2: Create predictor variables: Beverage and Hours 

samIDs<-names(averageRFC2)
Beverages<-(samIDs%in%Alldata$trt1)*1+(samIDs%in%Alldata$trt2)*2+(samIDs%in%Alldata$trt3)*3+(samIDs%in%Alldata$trt4)*4
hours<-(samIDs%in%Alldata$time_h0)*0+(samIDs%in%Alldata$time_h1)*1+(samIDs%in%Alldata$time_h2)*2+(samIDs%in%Alldata$time_h4)*4+(samIDs%in%Alldata$time_h12)*12
Beverages <- factor(Beverages)

## Question 3: Plot the curves: average gene expression versus hours by beverages (should have numbers that come as a reuslt of summary from lm)

colvec<-NULL
pchvec<-NULL
ltyvec<-NULL
plot(hours,resp,xlab="Hours",ylab="gene expression average",type="n")
for (bever in 1:4)
{
  BeverHourAve<-tapply(resp[(Beverages==bever)],hours[(Beverages==bever)],mean)
  points(c(0,1,2,4,12),BeverHourAve,col=bever,pch=bever)
  lines(c(0,1,2,4,12),BeverHourAve,col=bever)
  colvec<-c(colvec,bever)
  pchvec<-c(pchvec,bever)
  ltyvec<-c(ltyvec,bever)
}
legendtext<-c("Alcohol","Grape juice","Red wine","Water")
legend(8,4.5,legendtext,col=colvec,pch=pchvec,lty=ltyvec)

## Question 4: Construct linear model for gene expression
lm1 <- lm(averageRFC2 ~ Beverages)


## Question 5: Analyze coefficient(s)
summary(lm1)
# Least squares estimate of different effect of alcohol and water on average gene expression of RFC2 is 0.02851, 
# but since the p-value is not significant, this difference that we observe is not significantly different from 0.

## Question 6: Construct linear model for gene expression using both beverages and hours
hours3 <- hours^3
lm3 <- lm(averageRFC2 ~ Beverages + hours3)

## Question 7: Define new data set for prediction

newdata<-as.data.frame(list(Beverages="1",hours=6, hours2=36, hours3=216))
newprediction<-predict(lm3,newdata,interval="predict", level = .95)
#    fit      lwr      upr
#1 4.294241 4.092725 4.495756








