# Al Pakrosnis
# Prof. Dale Embers
# STAT 385
# Homework 2

install.packages("ISLR2")
library("ISLR2")

# 1 (a)
m1 <- lm(Sales ~ Population + Income + US, data = Carseats)

# (b)
summary(m1)
#   Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 5.7134361  0.4657440  12.267  < 2e-16 ***
# Population  0.0007997  0.0009395   0.851  0.39519    
# Income      0.0139000  0.0049584   2.803  0.00531 ** 
# USYes       0.9562235  0.2901542   3.296  0.00107 ** 
# 
# The intercept of 5.7 represents the baseline level of sales. It is the y-intercept for a linear model.
# The coeff for pop is .0008, which represents for each unit increase in population the level of sales increases by .0008.
#   However, the p-value is not statistically significant so population may not be a good estimator of sales.
# The coeff for income is .0139, which means that for a one unit increase in income the level of sales increases by .0139
#   Given the p-value below .05, this is statistically significant and we can assume that income is a good predictor of sales.
# The coeff for USYes is .956, which means that stores located in the US are expected to have sales .956 units higher than 
#   foreign stores. Similar to income, the p-value is below .05 so it's implied that being in the US is a good predictor of sales.


# (c)
# Sales = 5.713 + .0008(population) + .0139(income) + .956(USYes)


# (d)
summary(m1)$coefficients
#               Estimate   Std. Error    t value     Pr(>|t|)
# (Intercept) 5.7134360720 0.4657440229 12.2673310 1.523378e-29
# Population  0.0007996769 0.0009395102  0.8511636 3.951929e-01
# Income      0.0138999869 0.0049583888  2.8033273 5.306579e-03
# USYes       0.9562234975 0.2901542068  3.2955700 1.070762e-03
#
# Considering a significance level of 95% we can reject the null hypothesis for all variables in the model excluding population

# (e)
m2 <- lm(Sales ~ Income + US, data = Carseats)
summary(m2)

# (f)
anova(m1, m2)
# Analysis of Variance Table
# 
# Model 1: Sales ~ Population + Income + US
# Model 2: Sales ~ Income + US
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    396 3017.5                           
# 2    397 3023.1 -1   -5.5206 0.7245 0.3952
summary(m1)$r.squared
# 0.05176533
summary(m2)$r.squared
# 0.05003054
#
# Both models fit the data relatively equally. I would still choose model 2 over model 1 though because it's less complex without
# really losing accuracy.

# (g)
confint(m2, level=0.95)
#                 2.5 %     97.5 %
# (Intercept) 5.137081100 6.70151976
# Income      0.004099784 0.02358724
# USYes       0.402263922 1.54056948


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
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.97942    0.03243  153.53   <2e-16 ***
# x             0.67653    0.03155   21.44   <2e-16 ***
#
# The estimate for beta_0 is really close to the actual beta_0, but the estimate for the slope of the regression is honestly kind of far from the actual slope of .8.
# Cool to see. 

# (f) 
abline(lsqrs_model, col="blue")
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
# Since the p-value for the new binomial term is not significant we know that it does not meaningfully contribute to the model 
# in explaining the variability in the response variable.

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
# In the individual models each predictor is statistically significant, whereas in the multimodel only tax is statistically signficant to a high 
# degree. Dis is almost significant, but assuming a 95% confidence level it doesn't make the cut. Using the multimodel I think we get a better
# outcome than individual models.

# (d) fit a polynomial model for each variable now XD

m_nox2 <- lm(crim ~ nox + I(nox^2) + I(nox^3), data=Boston)
summary(m_nox2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   233.09      33.64   6.928 1.31e-11 ***
# nox         -1279.37     170.40  -7.508 2.76e-13 ***
# I(nox^2)     2248.54     279.90   8.033 6.81e-15 ***
# I(nox^3)    -1245.70     149.28  -8.345 6.96e-16 ***

m_tax2 <- lm(crim ~ tax + I(tax^2) + I(tax^3), data=Boston)
summary(m_tax2)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  1.918e+01  1.180e+01   1.626    0.105
# tax         -1.533e-01  9.568e-02  -1.602    0.110
# I(tax^2)     3.608e-04  2.425e-04   1.488    0.137
# I(tax^3)    -2.204e-07  1.889e-07  -1.167    0.244

m_dis2 <- lm(crim ~ dis + I(dis^2) + I(dis^3), data=Boston)
summary(m_dis2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  30.0476     2.4459  12.285  < 2e-16 ***
# dis         -15.5543     1.7360  -8.960  < 2e-16 ***
# I(dis^2)      2.4521     0.3464   7.078 4.94e-12 ***
# I(dis^3)     -0.1186     0.0204  -5.814 1.09e-08 ***

m_ptratio2 <- lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3), data=Boston)
summary(m_ptratio2)
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  477.18405  156.79498   3.043  0.00246 **
# ptratio      -82.36054   27.64394  -2.979  0.00303 **
# I(ptratio^2)   4.63535    1.60832   2.882  0.00412 **
# I(ptratio^3)  -0.08476    0.03090  -2.743  0.00630 **

# Based on the summary results of each regression, nox, dis, and ptratio all have statistically significant non-linear association with the 
# response variable crim. Tax does not have a statistically significant association with crime rates. 
