# Homework 5
# Al Pakrosnis
# Prof. Dale Reed
# STAT 385 Spring 2025

# Libraries
library(glmnet)
library(leaps) # for regsubsets

# Question 1. Best Subset Selection
set.seed(2024)

X <- rnorm(100)

noise <- rnorm(100)

Y <- 1 + 3*X + 2*X^2 + -1*X^3 + noise

df <- data.frame(X,Y)

for(i in seq(2,10)){
  df[[paste0("X",i)]] <- X^i
}

best_subset <- regsubsets(Y ~ ., data = df, nvmax = 10)
summary(best_subset)

# Plotting Code                                                       # needs to be finished
par(mfrow = c(1,3))



# Coefficients
best_mod_size <- which.max(summary(best_subset)$adjr2)
best_mod_size # 7
coef(best_subset, best_mod_size)
# (Intercept)             X            X2            X3            X5            X7            X9           X10 
# 1.1894975340  3.3363713005  1.9006606852 -2.8115670150  1.5118402198 -0.3656923999  0.0251636883  0.0008454263 

# Forward and Backward Stepwise Selection
fwd_model



