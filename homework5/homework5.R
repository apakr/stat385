# Homework 5
# Al Pakrosnis
# Prof. Dale Reed
# STAT 385 Spring 2025

# Libraries
library(glmnet)
library(leaps) # for regsubsets

# Question 1
set.seed(2024)

X <- rnorm(100)

noise <- rnorm(100)

Y <- 1 + 3*X + 2*X^2 + -1*X^4 + noise

df <- data.frame(X,Y)

for(i in seq(2,10)){
  df[[paste0("X",i)]] <- X^i
}

best_subset <- regsubsets(Y ~ ., data = df, nvmax = 10)
summary(best_subset)
best_summary <- summary(best_subset)

# Plotting                               
par(mfrow = c(1,3))

plot(best_summary$cp, xlab="Number of Variables", ylab="Cp", type="b", main="Cp")
points(which.min(best_summary$cp), best_summary$cp[which.min(best_summary$cp)], col="red", cex=2, pch=20)

plot(best_summary$bic, xlab="Number of Variables", ylab="BIC", type="b", main="BIC")
points(which.min(best_summary$bic), best_summary$bic[which.min(best_summary$bic)], col="red", cex=2, pch=20)

plot(best_summary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="b", main="Adjusted R^2")
points(which.max(best_summary$adjr2), best_summary$adjr2[which.max(best_summary$adjr2)], col="red", cex=2, pch=20)


# Coefficients
best_mod_size <- which.max(summary(best_subset)$adjr2)
best_mod_size # 7
coef(best_subset, best_mod_size)
# (Intercept)           X          X2          X3          X4          X5          X7          X9         X10 
# 1.11032221  3.30182018  2.28861588 -1.82033162 -1.20216794  1.60413772 -0.42925047  0.03593944  0.00270339 

# Forward and backward stepwise selection

# Forward stepwise
fwd_model <- regsubsets(Y ~ ., data = df, nvmax = 10, method = "forward")
summary(fwd_model)

# Backward stepwise
bwd_model <- regsubsets(Y ~ ., data = df, nvmax = 10, method = "backward")
summary(bwd_model)

# Compare model sizes and coefficients
fwd_summary <- summary(fwd_model)
bwd_summary <- summary(bwd_model)

which.max(fwd_summary$adjr2) # 7, same as c
coef(fwd_model, which.max(fwd_summary$adjr2))
# (Intercept)            X           X2           X4           X5           X8           X9          X10 
# 1.087612992  2.630398607  2.554217925 -1.469154659  0.177942734  0.048153799 -0.008316900 -0.006233604 

which.max(bwd_summary$adjr2) # 8, one more than c
coef(bwd_model, which.max(bwd_summary$adjr2))
# (Intercept)           X          X2          X3          X4          X5          X7          X9         X10 
# 1.11032221  3.30182018  2.28861588 -1.82033162 -1.20216794  1.60413772 -0.42925047  0.03593944  0.00270339 

# Lasso and cross validation

# Create model matrix for X^1 to X^10
X_poly <- model.matrix(Y ~ poly(X, 10, raw=TRUE))[,-1]

lasso_cv <- cv.glmnet(X_poly, Y, alpha=1)
plot(lasso_cv)

# Best lambda and coefficients
best_lambda <- lasso_cv$lambda.min
best_lambda # 0.02819813
lasso_model <- glmnet(X_poly, Y, alpha=1)
coef(lasso_model, s = best_lambda)
# > coef(lasso_model, s = best_lambda)
# 11 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)                1.57345971
# poly(X, 10, raw = TRUE)1   2.72179647
# poly(X, 10, raw = TRUE)2   0.57703388
# poly(X, 10, raw = TRUE)3   0.18124881
# poly(X, 10, raw = TRUE)4  -0.44996827
# poly(X, 10, raw = TRUE)5   .         
# poly(X, 10, raw = TRUE)6  -0.03529412
# poly(X, 10, raw = TRUE)7   .         
# poly(X, 10, raw = TRUE)8   .         
# poly(X, 10, raw = TRUE)9   .         
# poly(X, 10, raw = TRUE)10  .         

# With a lambda of .0282, the model ended up selecting X1-X4, and X6.
# The largest coeffs, X1, X2, and X4, where all kind of close to the true model (not really X2), but 
# lasso did a somewhat good job of finding the underlying model.


# New response vector
Y_new <- 1 + X^5 + noise

# Create data frame with X^1 through X^10
df_new <- data.frame(X = X)
for (i in 2:10) {
  df_new[[paste0("X", i)]] <- X^i
}
df_new$Y <- Y_new

subset_new <- regsubsets(Y ~ ., data = df_new, nvmax = 10)
subset_summary <- summary(subset_new)

# Plot model selection criteria
par(mfrow = c(1,3))
plot(subset_summary$cp, xlab="Number of Variables", ylab="Cp", type="b", main="Cp")
points(which.min(subset_summary$cp), min(subset_summary$cp), col="red", pch=20, cex=2)

plot(subset_summary$bic, xlab="Number of Variables", ylab="BIC", type="b", main="BIC")
points(which.min(subset_summary$bic), min(subset_summary$bic), col="red", pch=20, cex=2)

plot(subset_summary$adjr2, xlab="Number of Variables", ylab="Adjusted R^2", type="b", main="Adjusted R^2")
points(which.max(subset_summary$adjr2), max(subset_summary$adjr2), col="red", pch=20, cex=2)

# Best model size
best_size <- which.max(subset_summary$adjr2)
best_size # 6
coef(subset_new, best_size)
# (Intercept)           X3           X5           X7           X8           X9          X10 
# 1.177641183 -1.465262838  2.616150495 -0.508840216 -0.026798262  0.050792726  0.007442316 

# Model matrix for predictors (excluding intercept)
X_poly_new <- model.matrix(Y_new ~ poly(X, 10, raw=TRUE))[,-1]

# Cross-validated lasso
lasso_cv_new <- cv.glmnet(X_poly_new, Y_new, alpha=1)
plot(lasso_cv_new)

# Coefficients at best lambda
best_lambda_new <- lasso_cv_new$lambda.min
best_lambda_new # 0.6435275
coef(lasso_cv_new, s = best_lambda_new)
# > coef(lasso_cv_new, s = best_lambda_new)
# 11 x 1 sparse Matrix of class "dgCMatrix"
# s1
# (Intercept)               1.0541683
# poly(X, 10, raw = TRUE)1  .        
# poly(X, 10, raw = TRUE)2  .        
# poly(X, 10, raw = TRUE)3  .        
# poly(X, 10, raw = TRUE)4  .        
# poly(X, 10, raw = TRUE)5  0.9854343
# poly(X, 10, raw = TRUE)6  .        
# poly(X, 10, raw = TRUE)7  .        
# poly(X, 10, raw = TRUE)8  .        
# poly(X, 10, raw = TRUE)9  .        
# poly(X, 10, raw = TRUE)10 .        

# Lasso did a really good job here finding the original sparse model, correctly picking the relevant intercept
# and X5 coefficient

# Question 2
library(ISLR2)
library(pls)

# (a) 
set.seed(2024)
data(College)
train_indices <- sample(1:nrow(College), nrow(College)/2)
train <- College[train_indices, ]
test <- College[-train_indices, ]

# Response variable
y_test <- test$Apps

# (b) Linear reg
lm_fit <- lm(Apps ~ ., data = train)
lm_preds <- predict(lm_fit, newdata = test)
lm_mse <- mean((lm_preds - y_test)^2)
lm_mse # 1253307

# (c) Ridge Regression
x_train <- model.matrix(Apps ~ ., data = train)[,-1]
x_test <- model.matrix(Apps ~ ., data = test)[,-1]
y_train <- train$Apps

ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_preds <- predict(ridge_cv, s = ridge_cv$lambda.min, newx = x_test)
ridge_mse <- mean((ridge_preds - y_test)^2)
ridge_mse # 2007560

# (d) Lasso Regression
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_preds <- predict(lasso_cv, s = lasso_cv$lambda.min, newx = x_test)
lasso_mse <- mean((lasso_preds - y_test)^2)
lasso_mse # 1280906

# Num of non-zero coefficients in lasso
lasso_coef <- coef(lasso_cv, s = lasso_cv$lambda.min)
nonzero_lasso <- sum(lasso_coef != 0) - 1  
nonzero_lasso # 15

# (e) PCR
pcr_fit <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")

# Choose number of components with lowest CV error
best_m_pcr <- which.min(pcr_fit$validation$PRESS)
pcr_preds <- predict(pcr_fit, newdata = test, ncomp = best_m_pcr)
pcr_mse <- mean((pcr_preds - y_test)^2)
pcr_mse # 1253307
best_m_pcr # 17

# (f) PLS
pls_fit <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
summary(pls_fit)
validationplot(pls_fit, val.type = "MSEP")

# Choose best number of components
best_m_pls <- which.min(pls_fit$validation$PRESS)
pls_preds <- predict(pls_fit, newdata = test, ncomp = best_m_pls)
pls_mse <- mean((pls_preds - y_test)^2)
pls_mse # 1251989
best_m_pls # 13

# (g)
# Linear Regression: 1253307
# Ridge Regression: 2007560
# Lasso Regression: 1280906 with 15 non-zero coefficients
# PCR: 1253307 with 17 components
# PLS: 1251989 with 13 components
#
# All models performed relatively equally ell, with only ridge doing much worse than the others. PLS, however,
# was able to have the relatively same error while being less complex than the other models. If I wanted 
# to try and predict number of college apps recieved, I'd select PLS

# Question 3
library(MASS)

# (a) 
set.seed(2024)
data(Boston)
train_idx <- sample(1:nrow(Boston), nrow(Boston)/2)
train <- Boston[train_idx, ]
test <- Boston[-train_idx, ]
y_test <- test$crim

# Custom predict function for regsubsets
predict_regsubsets <- function(object, newdata, id) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  vars <- names(coefi)
  mat[, vars] %*% coefi
}

# Best subset selection
regfit_full <- regsubsets(crim ~ ., data = train, nvmax = 13)
reg_summary <- summary(regfit_full)

# Select model with lowest BIC
best_size <- which.min(reg_summary$bic)
subset_preds <- predict_regsubsets(regfit_full, test, best_size)
subset_mse <- mean((subset_preds - y_test)^2)
subset_mse # 58.92725

# Ridge regression
x_train <- model.matrix(crim ~ ., data = train)[,-1]
x_test <- model.matrix(crim ~ ., data = test)[,-1]
y_train <- train$crim

ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
ridge_preds <- predict(ridge_cv, s = ridge_cv$lambda.min, newx = x_test)
ridge_mse <- mean((ridge_preds - y_test)^2)
ridge_mse # 57.19768

# Lasso regression
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
lasso_preds <- predict(lasso_cv, s = lasso_cv$lambda.min, newx = x_test)
lasso_mse <- mean((lasso_preds - y_test)^2)
nonzero_lasso <- sum(coef(lasso_cv, s = lasso_cv$lambda.min) != 0) - 1
lasso_mse # 58.50476
nonzero_lasso # 5

# PCR
pcr_fit <- pcr(crim ~ ., data = train, scale = TRUE, validation = "CV")
validationplot(pcr_fit, val.type = "MSEP")

best_m_pcr <- which.min(pcr_fit$validation$PRESS)
pcr_preds <- predict(pcr_fit, test, ncomp = best_m_pcr)
pcr_mse <- mean((pcr_preds - y_test)^2)
pcr_mse # 56.93315
best_m_pcr # 13

# (b) 

# Best subset selection: 58.92725 MSE on test data
# Ridge regression: 57.19768 MSE on test data
# Lasso: 58.50476 MSE on test data w/ 5 vars
# PCR: 56.93315 MSE on test data w/ 13 vars

# I propose using the Lasso model, based on all the models I tested having roughly similar MSE based on test
# data, Lasso achieved this with less complexity and only 5 variables, so whatever small loss it has from
# a marginally higher MSE, it makes up for with far less variables.

# (c)

# No, my chosen model does not include all of the features in the data set, only 5. It's like this because
# that's what Lasso chose when I ran it - it set the rest of the vars to 0. 

