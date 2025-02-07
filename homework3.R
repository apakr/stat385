# Al Pakrosnis
# Prof. Dale Embers
# STAT 385
# Homework 2

install.packages("ISLR2")
library("ISLR2")

# 1 (a)
m1 <- lm(Sales ~ Population + Income + US, data = Carseats)
summary(m1)

# (b)

# (c)

# (d)

# (e)

# (f)

# (g)


# 2 (a)
set.seed(1329)

x <- rnorm(150)

# (b)
epsilon <- rnorm(150, 0, .6)

# (c)
y = 5 + .7*x + epsilon

length(y)
# 150
# beta_0: 5
# beta_1: .8

# (d)
plot(x, y, col = "red", xlab = "Normal Dist (x)", ylab = "Model (y)", main = "Scatterplot of x and y")
# Just by simply looking at the plot I see a correlation between the two variables, with a slope of about .8, trending up and to the right.

# (e)
lsqrs_model <- lm(y ~ x)
summary(lsqrs_model)
Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  4.97942    0.03243  153.53   <2e-16 ***
#   x            0.67653    0.03155   21.44   <2e-16 ***
#
# The estimate for beta_0 is really close to the actual beta_0, but the estimate for the slope of the regression is honestly kind of far from the actual slope of .8.
# Cool to see. 

# (f) 
abline(lsqrs_model, col="blue")                                                                                  # some issue with this line
abline(a = 5, b = .8, col="green")  
legend("topright", legend=c("Least Squares", "Population Regression"), col=c("blue","green"), lty=1)
  
# (g)
quadr_model <- lm(y ~ x + I(x^2))
summary(quadr_model)  
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.03189    0.06408  78.523   <2e-16 ***
#   x            0.66651    0.05179  12.869   <2e-16 ***
#   I(x^2)       0.01307    0.03524   0.371    0.711  
#
#                                                                                                                   # not done here

# (h)
epsilon_less_noise <- rnorm(150, 0, .3)
y_less_noise <- 5 + .8*x + epsilon_less_noise
lsqrs_model_less_noise <- lm(y_less_noise ~ x)
summary(lsqrs_model_less_noise)
plot(x, y_less_noise, col = "red", xlab = "Normal Dist (x)", ylab = "Model with less noise (y_1)", main = "Scatterplot of x and y_1")
abline(lsqrs_model_less_noise, col = "blue")  
abline(a = 5, b = .8, col="green")
legend("topright", legend=c("Least Squares with less noise","Population Regression"), col=c("blue","green"),lty=1)

# The results look much better with a reduced error term. The regression lines almost fit each other perfectly, which indicates a lesser effect of the noise in 
# clouding results. 

# (i)

confint(lsqrs_model)
#                 2.5 %    97.5 %
# (Intercept) 4.9432829 5.1482567
# x           0.5709145 0.7703066

confint(lsqrs_model_less_noise)
#                 2.5 %    97.5 %
# (Intercept) 4.962570 5.0550608
# x           0.758773 0.8487452


# 3 (a)
library(MASS)
data("Boston")

m_nox <- lm(crim ~ nox, data=Boston)
m_tax <- lm(crim ~ tax, data=Boston)
m_dis <- lm(crim ~ dis, data=Boston)
m_ptratio <- lm(crim ~ ptratio, data=Boston)

summary(m_nox)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -13.720      1.699  -8.073 5.08e-15 ***
# nox           31.249      2.999  10.419  < 2e-16 ***
plot(Boston$nox, Boston$crim, main = "nox vs crim")
abline(m_nox, col="red")

summary(m_tax)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -8.528369   0.815809  -10.45   <2e-16 ***
# tax          0.029742   0.001847   16.10   <2e-16 ***
plot(Boston$tax, Boston$crim, main = "tax vs crim")
abline(m_tax, col="red")

summary(m_dis)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   9.4993     0.7304  13.006   <2e-16 ***
# dis          -1.5509     0.1683  -9.213   <2e-16 ***
plot(Boston$dis, Boston$crim, main = "tax vs crim")
abline(m_dis, col="red")

summary(m_ptratio)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.6469     3.1473  -5.607 3.40e-08 ***
# ptratio       1.1520     0.1694   6.801 2.94e-11 ***
plot(Boston$ptratio, Boston$crim, main = "tax vs crim")
abline(m_ptratio, col="red")

# Based on the summary results and plots for each predictor they are all statistically significant in predicting the crime rate. Each has a positive association 
# with crime rate except dis which has a negative association.

# (b)
m_mult <- lm(crim ~ nox + tax + dis + ptratio, data=Boston)
summary(m_mult)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -7.034997   4.483724  -1.569   0.1173    
# nox         -1.111543   4.891280  -0.227   0.8203    
# tax          0.026770   0.002786   9.607   <2e-16 ***
# dis         -0.427506   0.233146  -1.834   0.0673 .  
# ptratio      0.106150   0.165828   0.640   0.5224    

# Assuming a confidence level of 95%, we can only reject the null hypothesis for tax rate, and can almost reject it for distance. 

# (c)
#                                                                                                                                         # gotta compare the models

# (d) fit a polynomial model for each variable now XD

m_nox2 <- lm(crim ~ nox + I(nox^2) + I(nox^3), data=Boston)
summary(m_nox2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   233.09      33.64   6.928 1.31e-11 ***
# nox         -1279.37     170.40  -7.508 2.76e-13 ***
# I(nox^2)     2248.54     279.90   8.033 6.81e-15 ***
# I(nox^3)    -1245.70     149.28  -8.345 6.96e-16 ***

m_tax2

m_dis2

m_ptratio2


