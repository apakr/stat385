# Al Pakrosnis
# Prof. Dale Embers
# STAT 385
# Homework 4

setwd("~/Desktop/stat385/homework4/")
getwd()

chromoX <- read.table(file="ChromoXmicroarray.txt",header=TRUE)

head(chromoX)

BLClabels <- c(rep(1,13),rep(2,14),rep(3,20))
BLClabels

# Q1. Setup
set.seed(2024)
train.set<-sample(c(1:47),27)
test.set <- setdiff(1:47, train.set)

training.dat <- chromoX[train.set,3:49]
test.dat <- chromoX[test.set,3:49]

# Q2. Perform ANOVA to select most relevant genes
anova_test <- function(x, group) {
  x <- as.numeric(x)
  aov_result <- aov(x ~ factor(group))
  return(summary(aov_result)[[1]]["Pr(>F)", 1])
}

pvalues <- apply(t(training.dat), 1, anova_test, group=BLClabels[train.set])
smallpvals <- order(pvalues)[2:3]
features2 <- chromoX[smallpvals, 3:49]

# Normalize selected features
normalization <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
microdata2 <- apply(features2, 1, normalization)
microdata2 <- data.frame(gene1=microdata2[,1], gene2=microdata2[,2], group=BLClabels)

# Define training and test sets
training.dat <- microdata2[train.set, ]
test.dat <- microdata2[test.set, ]

# Q3 Classification Methods







