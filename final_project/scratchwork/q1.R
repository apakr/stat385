# q1 stuff
setwd("~/Desktop/stat385/final_project/")

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(caret)



# Load datasets
math <- read.csv("math.csv")
portuguese <- read.csv("portuguese.csv")

# Merge datasets
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus",
             "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")
data <- merge(math, portuguese, by = join_by, suffixes = c("_math", "_portuguese"))

# Drop duplicate Portuguese columns except the G1/G2/G3 grades
drop_cols <- grep("_portuguese", names(data), value = TRUE)
drop_cols <- setdiff(drop_cols, c("G1_portuguese", "G2_portuguese", "G3_portuguese"))
data <- data[, !(names(data) %in% drop_cols)]

# Rename Portuguese grade columns for simplicity
names(data)[names(data) == "G1_portuguese"] <- "G1_port"
names(data)[names(data) == "G2_portuguese"] <- "G2_port"
names(data)[names(data) == "G3_portuguese"] <- "G3_port"

# Summary statistics
summary_stats <- data %>%
  select(G3_math, G3_port) %>%
  summary()
print(summary_stats)

# Histograms of final grades
g1 <- ggplot(data, aes(x = G3_math)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Final Math Grades", x = "Grade", y = "Count")

g2 <- ggplot(data, aes(x = G3_port)) +
  geom_histogram(binwidth = 1, fill = "darkorange", color = "white") +
  theme_minimal() +
  labs(title = "Final Portuguese Grades", x = "Grade", y = "Count")

grid.arrange(g1, g2, nrow = 1)

# Scatter plot: Math vs Portuguese final grades
g3 <- ggplot(data, aes(x = G3_math, y = G3_port)) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(title = "Math vs Portuguese Final Grades", x = "Math G3", y = "Portuguese G3")
g3

# include things here that check assumptions for both the histograms and the regression

# Correlation
cor_val <- cor(data$G3_math, data$G3_port)
cor_val


# ========== MODELING SECTION ==========

# LINEAR REGRESSION (SIGNIFICANT VARS ONLY, NO G1/G2 WITH TRAIN/TEST SPLIT)
set.seed(385)

# Split for math
math_idx <- createDataPartition(data$G3_math, p = 0.75, list = FALSE)
train_math <- data[math_idx, ]
test_math <- data[-math_idx, ]

lm_math <- lm(G3_math ~ ., data = train_math)
summary(lm_math)

lm_math_sig <- lm(G3_math ~ schoolsup_math + famrel_math + absences_math, data = train_math)
summary(lm_math_sig)
pred_math <- predict(lm_math_sig, newdata = test_math)
mse_math <- (mean((pred_math - test_math$G3_math)^2))
mse_math
# [1] 22.36964

# Split for portuguese
port_idx <- createDataPartition(data$G3_port, p = 0.75, list = FALSE)
train_port <- data[port_idx, ]
test_port <- data[-port_idx, ]

lm_port <- lm(G3_port ~ ., data = train_port)
summary(lm_port)

lm_port_sig <- lm(G3_port ~ Fjob + school + reason + failures_math, data = train_port)
summary(lm_port_sig)
pred_port <- predict(lm_port_sig, newdata = test_port)
mse_port <- (mean((pred_port - test_port$G3_port)^2))
mse_port
# [1] 8.169409

summary(lm_math_sig)
summary(lm_port_sig)


# Prepare data
features_math <- data %>% select(-G1_port, -G2_port, -G3_port)
features_port <- data %>% select(-G1_math, -G2_math, -G3_math)

# Linear regression
lm_math <- lm(G3_math ~ ., data = features_math)
lm_port <- lm(G3_port ~ ., data = features_port)
summary(lm_math)
summary(lm_port)

# Linear regression (signf vars only, excluding G1/G2)
lm_math_sig <- lm(G3_math ~ schoolsup_math + activities_math + famrel_math + absences_math, data = features_math)
lm_port_sig <- lm(G3_port ~ school + Fjob + reason + failures_math, data = features_port)
summary(lm_math_sig)
summary(lm_port_sig)


# RIDGE REGRESSION
library("MASS")

set.seed(123)  

n_math <- nrow(x_math)
train_index_math <- sample(1:n_math, size = 0.75 * n_math)
x_math_train <- x_math[train_index_math, ]
x_math_test <- x_math[-train_index_math, ]
y_math_train <- y_math[train_index_math]
y_math_test <- y_math[-train_index_math]

n_port <- nrow(x_port)
train_index_port <- sample(1:n_port, size = 0.75 * n_port)
x_port_train <- x_port[train_index_port, ]
x_port_test <- x_port[-train_index_port, ]
y_port_train <- y_port[train_index_port]
y_port_test <- y_port[-train_index_port]

cv_ridge_math <- cv.glmnet(x_math_train, y_math_train, alpha = 0)
best_lambda_math <- cv_ridge_math$lambda.min
ridge_math_model <- glmnet(x_math_train, y_math_train, alpha = 0, lambda = best_lambda_math)

pred_math <- predict(ridge_math_model, newx = x_math_test)
mse_math <- mean((y_math_test - pred_math)^2)

cv_ridge_port <- cv.glmnet(x_port_train, y_port_train, alpha = 0)
best_lambda_port <- cv_ridge_port$lambda.min
ridge_port_model <- glmnet(x_port_train, y_port_train, alpha = 0, lambda = best_lambda_port)

pred_port <- predict(ridge_port_model, newx = x_port_test)
mse_port <- mean((y_port_test - pred_port)^2)

