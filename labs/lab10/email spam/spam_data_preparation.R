## Set working directory
setwd("/Users/pszhong/Box Sync/UIC Teaching/STAT 385-2021/Data sets/email spam")

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

## Averages frequencies of words in spam and good emails
Spamemails=Spamdata[Spamdata$SpamOrNot==1,]
NonSpams=Spamdata[Spamdata$SpamOrNot==0,]
SpamAves=colMeans(Spamemails)
NonSpamAves=colMeans(NonSpams)
AbsDifferences=abs(SpamAves-NonSpamAves)
SortbyAbsDifferences=sort(AbsDifferences[-(nlen+1)],decreasing=TRUE)
indexafterorder=order(AbsDifferences[-(nlen+1)],decreasing=TRUE)
summaryinfo=rbind((SpamAves[-(nlen+1)])[indexafterorder],(NonSpamAves[-(nlen+1)])[indexafterorder],SortbyAbsDifferences)
rownames(summaryinfo)=c("spam","email","abs diff")







