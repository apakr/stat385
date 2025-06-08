setwd("~/Desktop/stat385/final_project/")

# Load required libraries
library(ggplot2)
library(caret)
library(pdp)
library(GGally)
library(dplyr)
library(nestedcv)
library(future)
library(tibble)
library(pdp)

# 1. Data Preparation --------------------------------------------------------
# Load and combine datasets
math <- read.csv("math.csv", stringsAsFactors = TRUE)
portuguese <- read.csv("portuguese.csv", stringsAsFactors = TRUE)
plan(multisession, workers = 8)

# Verify matching column structure
if(identical(colnames(math), colnames(portuguese))) {
  combined <- bind_rows(math, portuguese) %>% 
    mutate(needs_support = factor(ifelse(G3 < 10, "Yes", "No"))) %>% 
    select(-G1, -G2, -G3)  # Remove intermediate grades
} else {
  stop("Column structure mismatch between datasets")
}

# 2. Preprocessing -----------------------------------------------------------
set.seed(123, "L'Ecuyer-CMRG")  # Reproducible parallel random numbers
train_index <- createDataPartition(combined$needs_support, p = 0.8, list = FALSE)
train_data <- combined[train_index, ]
test_data <- combined[-train_index, ]

# 3. Model Training with Nested CV -------------------------------------------
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

logit_model <- train(
  needs_support ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

rf_model <- train(
  needs_support ~ .,
  data = train_data,
  method = "rf",
  trControl = ctrl,
  metric = "ROC",
  tuneLength = 5
)

# 4. Model Evaluation --------------------------------------------------------
results <- resamples(list(Logistic = logit_model, RandomForest = rf_model))
summary(results)

# 5. Feature Importance ------------------------------------------------------
# Logistic Regression Coefficients
logit_imp <- varImp(logit_model, scale = FALSE)$importance %>%
  rownames_to_column("Feature") %>%
  arrange(desc(Overall))

ggplot(logit_imp, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "#2E86C1") +
  coord_flip() +
  labs(title = "Logistic Regression: Feature Importance",
       x = "Predictor",
       y = "Coefficient Magnitude") +
  theme_minimal()

# Random Forest Importance
rf_imp <- varImp(rf_model)$importance %>%
  rownames_to_column("Feature") %>%
  arrange(desc(Overall))

ggplot(rf_imp[1:10,], aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "#28B463") +
  coord_flip() +
  labs(title = "Random Forest: Top 10 Predictive Features",
       x = "Feature",
       y = "Importance Score") +
  theme_minimal()

# 6. Partial Dependence Plots ------------------------------------------------
pdp_failures <- partial(rf_model, pred.var = "failures", train = train_data)
ggplot(pdp_failures, aes(x = failures, y = yhat)) +
  geom_line(color = "#CB4335", linewidth = 1) +
  geom_ribbon(aes(ymin = yhat - sd(yhat), ymax = yhat + sd(yhat)),
              alpha = 0.2, fill = "#F1948A") +
  labs(title = "Effect of Course Failures on Support Need",
       x = "Number of Past Failures",
       y = "Predicted Probability") +
  theme_minimal()

# 7. Feature Interactions ----------------------------------------------------
ggplot(train_data, aes(x = failures, y = Medu, color = needs_support)) +
  geom_jitter(alpha = 0.6) +
  geom_smooth(method = "glm", se = FALSE) +
  scale_color_manual(values = c("#1F618D", "#CD6155")) +
  labs(title = "Interaction: Failures & Maternal Education",
       x = "Course Failures",
       y = "Mother's Education Level") +
  theme_minimal()

# Starting with 1 on x-axis 
ggplot(train_data, aes(x = failures, y = Medu, color = needs_support)) +
  geom_jitter(alpha = 0.6) +
  geom_smooth(method = "glm", se = FALSE) +
  scale_color_manual(values = c("#1F618D", "#CD6155")) +
  scale_x_continuous(limits = c(1, NA)) +
  labs(title = "Interaction: Failures & Maternal Education",
       x = "Course Failures",
       y = "Mother's Education Level") +
  theme_minimal()


# 8. Multivariate Relationships ----------------------------------------------
ggpairs(train_data[, c("failures", "Medu", "absences", "goout", "Dalc","needs_support")],
        aes(color = needs_support, alpha = 0.5)) +
  labs(title = "Multivariate Feature Relationships") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 9. Model Comparison --------------------------------------------------------
dotplot(results, metric = "ROC", main = "Model Performance Comparison (ROC AUC)")

# 10. Final Evaluation -------------------------------------------------------
predictions <- predict(rf_model, test_data)
confusionMatrix(predictions, test_data$needs_support)
