# Question: Do the two schools have any difference in predictors?

## CLEANING ##

math <- read.csv('math.csv')
portuguese <- read.csv('portuguese.csv')

join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus",
             "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")
data <- merge(math, portuguese, by = join_by, suffixes = c("_math", "_portuguese"))
drop_cols <- grep("_portuguese", names(data), value = TRUE)
drop_cols <- setdiff(drop_cols, c("G1_portuguese", "G2_portuguese", "G3_portuguese"))

data <- data[, !(names(data) %in% drop_cols)]
## EDA ##

male <- data[data$sex ==  "M",]
female <- data[data$sex ==  "F",]
male = subset(male, select = -c(sex))
female = subset(female, select = -c(sex))
names(male)
names(female)
barplot(table(data$sex), 
        ylim=c(0,200),
        names.arg = c("Female", "Male"), 
        col = c("pink", "lightblue"),
        main = "Number of Students by Sex",
        ylab = "Count")

boxplot(G3_math ~ sex, data = data,
        main = "Final Math Grades by Sex",
        xlab = "Sex", ylab = "G3 Math",
        col = c("pink", "lightblue"))


summary(male$G3_math)
summary(female$G3_math)


mean_scores <- tapply(data$G3_math, list(Age = data$age, Sex = data$sex), mean)
mean_scores <- t(mean_scores)

barplot(mean_scores,
        beside = TRUE,
        col = c("pink", "dodgerblue"),
        xlab = "Age",
        ylab = "Mean Final Math Grade",
        ylim = c(0,20),
        main = "Mean G3_math by Age and Sex",
        legend.text = rownames(mean_scores),
        args.legend = list(title = "Sex", x = "topright"))

## MODELING PHASE ##

##########male##########
set.seed(04302025)
## Dividing male dataset into train and test
mmtrain_ind <- sample(1:nrow(male), nrow(male)*.75)
mm.train<-male[mmtrain_ind,]
mm.test<-male[-mmtrain_ind,]
rownames(mm.train) <- NULL
rownames(mm.test) <- NULL
## Multiple linear regression for male
mm.mlr <- lm(G3_math~., data=mm.train)
summary(mm.mlr)

male.lmpred <- predict(mm.mlr, newdata= mm.test)
mse_malelm <- mean((mm.test$G3_math - male.lmpred)^2)
mse_malelm
## stepwise regression for male
library(MASS)
mm.stepwise<-stepAIC(mm.mlr,direction="both", trace=1)
summary(mm.stepwise)

male.steppred <- predict(mm.stepwise, newdata= mm.test)
mse_malestep <- mean((mm.test$G3_math - male.steppred)^2)
mse_malestep
## LASSO for Male
library(glmnet)
male.xtrain <- model.matrix(G3_math ~ ., data = mm.train)[, -1]
male.xtest <- model.matrix(G3_math ~ ., data = mm.test)[, -1]
male.y_train <- mm.train$G3_math
male.y_test <- mm.test$G3_math

mm.lasso_cv <- cv.glmnet(male.xtrain, male.y_train, alpha=1, standardize= TRUE)
mm.lambest<- mm.lasso_cv$lambda.min
plot(mm.lasso_cv)

mm.lassobest<-glmnet(male.xtrain,male.y_train,alpha=1,lambda=mm.lambest)
mm.lassobest$beta

mm.lassopred <- predict(mm.lassobest, s=mm.lambest, newx=male.xtest)
mm.mse_lasso <- mean((male.y_test - mm.lassopred)^2)
mm.mse_lasso
########## female ##########
## Data Cleaning for female
fmtrain_ind <- sample(1:nrow(female), nrow(female)*.75)
fm.train<-female[fmtrain_ind,]
fm.test<-female[-fmtrain_ind,]
rownames(fm.train) <- NULL
rownames(fm.test) <- NULL
## Multiple linear regression for female
fm.mlr <- lm(G3_math~., data=fm.train)
summary(fm.mlr)

female.lmpred <- predict(fm.mlr, newdata= fm.test)
mse_femalelm <- mean((fm.test$G3_math - female.lmpred)^2)
mse_femalelm 
## Stepwise for female
fm.stepwise<-stepAIC(fm.mlr,direction="both", trace=1)
summary(fm.stepwise)

female.steppred <- predict(fm.stepwise, newdata= fm.test)
mse_femalestep <- mean((fm.test$G3_math - female.steppred)^2)
mse_femalestep
## LASSO for female
female.xtrain <- model.matrix(G3_math ~ ., data = fm.train)[, -1]
female.xtest <- model.matrix(G3_math ~ ., data = fm.test)[, -1]
female.y_train <- fm.train$G3_math
female.y_test <- fm.test$G3_math

fm.lasso_cv <- cv.glmnet(female.xtrain, female.y_train, alpha=1, standardize= TRUE)
fm.lambest<- fm.lasso_cv$lambda.min
plot(fm.lasso_cv)

fm.lassobest<-glmnet(female.xtrain,female.y_train,alpha=1,lambda=fm.lambest)
fm.lassobest$beta

fm.lassopred <- predict(fm.lassobest, s=fm.lambest, newx=female.xtest)
fm.mse_lasso <- mean((female.y_test - fm.lassopred)^2)
fm.mse_lasso
