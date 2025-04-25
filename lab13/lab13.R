#############################################################################
#
# The following code is for clustering for two types of breast cancers using
# microarray data set. The "BLClabels" provides the patient information:
# 1 for the normal and non-BLC patients and 2 for BLC patients.
#
# Please email me at pszhong@uic.edu if you have any question
#
#################### Read Data into R from local file #######################

setwd("~/Desktop/stat385/lab13/")

chromoX<-read.table(file="ChromoXmicroarray.txt",header=TRUE)
BLClabels<-c(rep(1,27),rep(2,20))

############################################################################
#
# Q1. Perform t-test to select the most relevant genes for classification
#
############################################################################

ttests<-function(x,group)
{
  x<-as.numeric(x)
  pvalue<-t.test(x ~ group)$p.value;
  return(pvalue)
}
pvalues<-apply(chromoX[,c(3:49)],1,ttests,group=BLClabels)
smallpvals<-order(pvalues)[1:2]
features2<-chromoX[smallpvals,c(3:49)]

plot(t(features2),col=BLClabels,pch=BLClabels)
legend("topleft",c("non-BLC","BLC"),col=c(1,2),pch=c(1,2))

############################################################################
#
# Perform normalization on two feature variables
#
############################################################################


normalization<-function(x)
{
  norms<-(x-min(x))/(max(x)-min(x))
  return(norms)
}
microdata2<-apply(features2,1,normalization)
microdata2<-data.frame(gene20475=microdata2[,1],gene20947=microdata2[,2])



############################################################################
#
# Q2. Elbow method: find total within-cluster sum of squares for k = 1 to 10
#
############################################################################

kmeans.winss <- rep(0, 10)
kval <- 1:10

for (k in kval) {
  kmeans.cluster.k <- kmeans(microdata2, k)
  kmeans.winss[k] <- kmeans.cluster.k$tot.withinss
}

# Now plot it
plot(kval, kmeans.winss, type = "b", xlab = "Number of Clusters k", ylab = "Total Within-Cluster SS",
     main = "Elbow Method for Choosing k")

# The elbow point is at 2 clusters. We should use k = 2


############################################################################
#
# Q3. K-means clustering with k=2
#
############################################################################


kmeans.cluster2<-kmeans(microdata2,2)
plot(microdata2, col=kmeans.cluster2$cluster, main="K-means clustering")
points(kmeans.cluster2$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=BLClabels, main="true labels")


############################################################################
#
# Hierarchical clustering using manhattan distance and centroid-linkage merging
#
############################################################################


par(mfrow=c(1,1))

DistMat<-dist(microdata2,method="manhattan")

hclustCen<-hclust(DistMat,method="centroid")

plot(hclustCen)

cuttree2<-cutree(hclustCen,k=2)
plot(hclustCen)
rect.hclust(hclustCen,k=2,border="red")



############################################################################
#
# Hierarchical clustering versus K-means clustering
#
############################################################################


plot(microdata2, col=kmeans.cluster2$cluster, main="K-means clustering (K=2)")
points(kmeans.cluster2$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=cuttree2, main="Hierarchical clustering (K=2)")




