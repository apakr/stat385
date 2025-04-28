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

kmeans_vs_true <- table(kmeans.cluster2$cluster, BLClabels)
kmeans_vs_true
#   BLClabels
#    1  2
# 1  1 18
# 2 26  2

# The k-means clustering with k=2 performs fine, but the groups are switched. Cluster 1 contains mostly BLC patients,
# while Cluster 2 contains mostly non-BLC patients.

############################################################################
#
# Additional: K-means clustering with k=3 and evaluate performance
#
############################################################################

kmeans.cluster3<-kmeans(microdata2,3)
plot(microdata2, col=kmeans.cluster3$cluster, main="K-means clustering (k=3)")
points(kmeans.cluster3$centers,cex=2,pch=20, col=c(1:3))

# Evaluate K-means clustering performance with k=3
kmeans3_vs_true <- table(kmeans.cluster3$cluster, BLClabels)
kmeans3_vs_true
#   BLClabels
#    1  2
# 1 11  0
# 2 15  2
# 3  1 18

# Looks a arguably better if we use an elbow of 3.

############################################################################
#
# Q4. Hierarchical clustering using manhattan distance and centroid-linkage merging
#
############################################################################


par(mfrow=c(1,1))

DistMat<-dist(microdata2,method="manhattan")

hclustCen<-hclust(DistMat,method="centroid")

plot(hclustCen)

cuttree2<-cutree(hclustCen,k=2)
plot(hclustCen)
rect.hclust(hclustCen,k=2,border="red")

hierarchical_vs_true <- table(cuttree2, BLClabels)
hierarchical_vs_true
#         BLClabels
# cuttree2  1  2
#         1 26  2
#         2  1 18

# The hierarchical clustering using Manhattan distance and centroid linkage performs well. Cluster 1
# groups mostly non-BLC patients, and Cluster 2 groups mostly BLC patients, with very little misclassification
# compared to k-means. This method works better for our data.

############################################################################
#
# Additional: Hierarchical clustering with k=3 and evaluate performance
#
############################################################################

cuttree3 <- cutree(hclustCen, k=3)
plot(hclustCen)
rect.hclust(hclustCen, k=3, border="blue")

# Evaluate Hierarchical clustering performance with k=3
hierarchical3_vs_true <- table(cuttree3, BLClabels)
hierarchical3_vs_true
# BLClabels
# cuttree3  1  2
# 1  9  0
# 2 17  2
# 3  1 18

# Pretty much the same accuracy as k = 2.

############################################################################
#
# Hierarchical clustering versus K-means clustering
#
############################################################################


plot(microdata2, col=kmeans.cluster2$cluster, main="K-means clustering (K=2)")
points(kmeans.cluster2$centers,cex=2,pch=20, col=c(1:2))
plot(microdata2, col=cuttree2, main="Hierarchical clustering (K=2)")

# good looks :D



