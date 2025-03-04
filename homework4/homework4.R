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

# Q3. Classification methods
library(MASS)  # LDA
library(e1071) # Naive Bayes
library(class) # KNN

# Logistic Regression
training.dat$group <- as.numeric(training.dat$group == 1)
logit_model <- glm(group ~ ., data=training.dat, family=binomial, control=list(epsilon=1e-8, maxit=50))

# LDA
lda_model <- lda(group ~ ., data=training.dat)

# Naive Bayes
nb_model <- naiveBayes(group ~ ., data=training.dat)

# KNN for k=4, k=8, k=12
knn_pred4 <- knn(train=training.dat[,1:2], test=test.dat[,1:2], cl=training.dat$group, k=4)
knn_pred8 <- knn(train=training.dat[,1:2], test=test.dat[,1:2], cl=training.dat$group, k=8)
knn_pred12 <- knn(train=training.dat[,1:2], test=test.dat[,1:2], cl=training.dat$group, k=12)

# Q4. Plot decision boundaries
par(mfrow=c(3,2))

# Logistic Regression decision boundary
plot(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19, main="Logistic Regression")
px1 <- seq(min(training.dat$gene1), max(training.dat$gene1), length=100)
px2 <- seq(min(training.dat$gene2), max(training.dat$gene2), length=100)
xgrid <- expand.grid(gene1=px1, gene2=px2)
xgrid$prob <- predict(logit_model, xgrid, type="response")

# Reshape probability matrix correctly
prob_matrix <- matrix(xgrid$prob, length(px1), length(px2))
contour(px1, px2, prob_matrix, levels=0.5, add=TRUE, col="red")

# LDA boundary
plot(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19, main="LDA")
abline(lda_model)

# Naive Bayes boundary
nb_pred_grid <- predict(nb_model, xgrid)
plot(xgrid$gene1, xgrid$gene2, col=as.numeric(nb_pred_grid), pch=20, cex=0.2, main="Naive Bayes")
points(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19)

# KNN boundary for k=4
knn_pred_grid4 <- knn(train=training.dat[,1:2], test=as.matrix(xgrid[,1:2]), cl=training.dat$group, k=4)
plot(xgrid$gene1, xgrid$gene2, col=as.numeric(knn_pred_grid4), pch=20, cex=0.2, main="KNN k=4")
points(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19)

# KNN boundary for k=8
knn_pred_grid8 <- knn(train=training.dat[,1:2], test=as.matrix(xgrid[,1:2]), cl=training.dat$group, k=8)
plot(xgrid$gene1, xgrid$gene2, col=as.numeric(knn_pred_grid8), pch=20, cex=0.2, main="KNN k=8")
points(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19)

# KNN boundary for k=12
knn_pred_grid12 <- knn(train=training.dat[,1:2], test=as.matrix(xgrid[,1:2]), cl=training.dat$group, k=12)
plot(xgrid$gene1, xgrid$gene2, col=as.numeric(knn_pred_grid12), pch=20, cex=0.2, main="KNN k=12")
points(training.dat$gene1, training.dat$gene2, col=training.dat$group + 1, pch=19)

# Q5. Predictions & Performance Evaluation
truth <- test.dat$group

# Logistic Regression Predictions
logit_pred <- predict(logit_model, test.dat, type="response")
logit_pred_class <- ifelse(logit_pred > 0.5, 1, 0)
table(truth, logit_pred_class)
#       logit_pred_class
# truth 0 1
#     1 7 1
#     2 7 0
#     3 4 1

# LDA Predictions
lda_pred <- predict(lda_model, test.dat)$class
table(truth, lda_pred)
#       lda_pred
# truth 0 1
#     1 8 0
#     2 7 0
#     3 5 0

# Naive Bayes Predictions
nb_pred <- predict(nb_model, test.dat)
table(truth, nb_pred)
#       nb_pred
# truth 0 1
#     1 8 0
#     2 6 1
#     3 4 1

# KNN Predictions

# K = 4
table(truth, knn_pred4)
#       knn_pred4
# truth 0 1
#     1 8 0
#     2 7 0
#     3 4 1

# K = 8
table(truth, knn_pred8)
#       knn_pred8
# truth 0 1
#     1 8 0
#     2 7 0
#     3 5 0

# K = 12
table(truth, knn_pred12)
#       knn_pred12
# truth 0 1
#     1 8 0
#     2 7 0
#     3 5 0

# So, based on how these methods performed, they all did a pretty bad job in predicting. Only the logistic regression and the naive bayes did a little bit "less bad"
# than the others. 



