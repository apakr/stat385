# Al Pakrosnis
# Homework 7
# Prof. Dale Embers
# 4/15/25

library(ISLR2)
library(rpart)
library(rpart.plot)
library(ipred)
library(adabag)
library(randomForest)

# Question 1
set.seed(20240407)
data <- OJ

train_indices <- sample(1:nrow(data), nrow(data)*.75)
train<-data[train_indices,]
test<-data[-train_indices,]
rownames(train) <- NULL
rownames(test) <- NULL


# Question 2

tree.full <- rpart(Purchase ~ ., data=train, control=rpart.control(minsplit = 2, minbucket = 1, cp=0))
prp(tree.full,type=2,extra=1)

tree.predictions<- predict(tree.full, test, type="class")
treetable<-table(test$Purchase, tree.predictions)
tree.errate<-(sum(treetable)-sum(diag(treetable)))/sum(treetable)
tree.errate
# 0.2574627


# Question 3
bestcp <- tree.full$cptable[which.min(tree.full$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree.full, cp = bestcp)
prune.predictions <- predict(tree.pruned, test, type="class")

ptreetable<-table(test$Purchase, prune.predictions)
ptree.errate<-(sum(ptreetable)-sum(diag(ptreetable)))/sum(ptreetable)
prp(tree.pruned,type=2,extra=1)
# 0.1902985


# Question 4
bagtree<-bagging(Purchase ~., data=train,coob=TRUE)
prune.bagtree<-prune(bagtree)
bagtree
# Bagging classification trees with 25 bootstrap replications 
# 
# Call: bagging.data.frame(formula = Purchase ~ ., data = train, coob = TRUE)
# 
# Out-of-bag estimate of misclassification error:  0.1958

bag.predictions<-predict(prune.bagtree, test)
bagtable<-table(test$Purchase, bag.predictions)
bag.errate<-(sum(bagtable)-sum(diag(bagtable)))/sum(bagtable)
bagtable
#   bag.predictions
#     CH  MM
# CH 129  29
# MM  21  89
# 0.2126866


# Question 5
bootstrap_seq <- seq(10, 300, by = 10)
oob_errors <- numeric(length(bootstrap_seq))
for (i in seq_along(bootstrap_seq)) {
  model <- bagging(Purchase ~ ., data = train, coob = TRUE, nbagg = bootstrap_seq[i])
  oob_errors[i] <- model$err
}
plot(bootstrap_seq, oob_errors, type = "b", pch = 19, col = "red")

# Around 100 bootstraps gives a local minimum OOB error. Beyond that point, additional bootstraps yield only marginal improvement in performance.
# Increasing the number of bootstraps does not lead to overfitting. Since OOB error is used as a validation measure, it typically levels off or decreases slightly, rather than increasing with more trees.


# Question 6
adaboost <- boosting(Purchase ~.,data=train,mfinal=50, coeflearn="Zhu",
                     control=rpart.control(maxdepth=15))
adaboost.pred <- predict.boosting(adaboost,newdata=test)
errate <- adaboost.pred$error
adaboost.table <- adaboost.pred$confusion
adaboost.table
#                 Observed Class
# Predicted Class  CH  MM
#               CH 128  29
#               MM  30  81


# Question 7 
mfinal <- seq(5, 25, by = 5)
maxdepth <- c(4:10)
Errmatrix <- matrix(0,length(mfinal),length(maxdepth))
for (i in 1:length(mfinal))
  for (j in 1:length(maxdepth))
  {
    multi.adaboost <- boosting(Purchase ~.,data=train,mfinal=mfinal[i], coeflearn="Zhu",
                               control=rpart.control(maxdepth=maxdepth[j]))
    multi.adaboost.pred <- predict.boosting(multi.adaboost,newdata=test)
    Errmatrix[i,j] <- multi.adaboost.pred$error
  }

# 0.1753731
# A model with 5 iterations and a depth of 3 provides the smallest error rate. If the number of iterations are increased overfitting is possible.


# Question 8

num.var <- 17

rf.mod.sqrt <- randomForest(Purchase ~ ., data = train, mtry = floor(sqrt(num.var - 1)), ntree = 250, proximity = TRUE, importance = TRUE)

# Choosing m=p/2 feat vars
rf.mod.half <- randomForest(Purchase ~ ., data = train, mtry = floor((num.var - 1)/2), ntree = 250, proximity = TRUE, importance = TRUE)

bagging.mod<-randomForest(Purchase ~ ., data = train, mtry = num.var - 1, ntree = 250, proximity = TRUE, importance = TRUE)
plot(rf.mod.sqrt$err.rate[,1], type = "l", lwd = 3, lty=1, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")
lines(rf.mod.half$err.rate[,1],lwd=3, lty=2, col="red")
lines(bagging.mod$err.rate[,1],lwd=3, lty=3, col="green")
legend("topright",c("m=sqrt(p)","m=p/2","m=p(bagging)"),col=c("blue","red","green"), lty=c(1,2,3))
# m = sqrt(p) is best
# OOB stabilizes at around 125


# Question 9

varImpPlot(rf.mod.sqrt, main = "Variable Importance Plot", type = 1, pch = 19, col = "blue") 
varImpPlot(rf.mod.half, main = "Variable Importance Plot", type = 1, pch = 19, col = "blue")
varImpPlot(bagging.mod, main = "Variable Importance Plot", type = 1, pch = 19, col = "blue")

importance_values <- importance(rf.mod.sqrt, type = 1)
top_vars <- head(sort(importance_values[, 1], decreasing = TRUE), 4)
top_vars

# Most important vars by order are LoyalCH, StoreID, PriceDiff, and STORE.

tuneRF(subset(train, select = -Purchase), train$Purchase, ntreeTry = 125)
# m=4





# Question 10

final.forest <- randomForest(Purchase ~ ., data = train,
                             mtry = 4, 
                             ntree = 250,
                             proximity = TRUE,
                             importance = TRUE)
forest.pred <- predict(final.forest, test)
foresttable<-table(test$Purchase, forest.pred)
forest.errate<-(sum(foresttable)-sum(diag(foresttable)))/sum(foresttable)
foresttable
#.1978



# Question 11

# Summary of Error Rates Across Methods:

# - The unpruned classification tree had the highest error rate at 25.7%.
# - Pruning reduced the tree’s error rate to 19.03%.
# - The bagged tree (unpruned) had an error rate of 21.3%, which improved to 17.9% after pruning.
# - Varying the number of bootstraps in the bagging model showed the best performance (~19% error) at around 100 bootstraps.
# - Boosting achieved the lowest error rate of 17.5% using 5 iterations and a max depth of 4.
# - The tuned random forest model with m = 4 had an error rate of 19.8%.
