setwd("~/Desktop/stat385/final_project/")


# Load required libraries
library(dplyr)
library(caret)
library(e1071)
library(randomForest)

## 1. Data Preparation ##
# Load and merge datasets vertically
math <- read.csv("math.csv")
portuguese <- read.csv("portuguese.csv")
combined_data <- rbind(math, portuguese)

## 2. Preprocessing ##
# Create target variable: Needs support (1 if any failures or school support)
combined_data <- combined_data %>%
  mutate(needs_support = ifelse(failures > 0 | schoolsup == "yes", 1, 0) %>% 
           as.factor())

# Convert categorical variables to factors
categorical_cols <- c('school', 'sex', 'address', 'famsize', 'Pstatus', 
                      'Mjob', 'Fjob', 'reason', 'guardian', 'paid', 'higher', 
                      'internet', 'romantic')
combined_data[categorical_cols] <- lapply(combined_data[categorical_cols], factor)

# Remove original target variables and identifiers
df <- combined_data %>% 
  select(-G1, -G2, -G3, -schoolsup, -famsup)

## 3. Train/Test Split ##
set.seed(123)
index <- createDataPartition(df$needs_support, p = 0.8, list = FALSE)
train_data <- df[index, ]
test_data <- df[-index, ]

## 4. Model Training ##

# Logistic Regression
logit_model <- train(needs_support ~ ., 
                     data = train_data,
                     method = "glm",
                     family = "binomial",
                     trControl = trainControl(method = "cv", number = 5))

# Naive Bayes
nb_model <- naiveBayes(needs_support ~ ., data = train_data)

# Random Forest with Nested CV
rf_model <- train(needs_support ~ .,
                  data = train_data,
                  method = "rf",
                  tuneLength = 3,
                  trControl = trainControl(method = "cv", number = 5),
                  importance = TRUE)

## 5. Model Evaluation ##

# Function to evaluate models
evaluate_model <- function(model, test_data, model_name) {
  if(model_name != "Naive Bayes") {
    preds <- predict(model, test_data)
  } else {
    preds <- predict(model, test_data)
  }
  
  cm <- confusionMatrix(preds, test_data$needs_support)
  results <- list(
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"]
  )
  return(results)
}

# Compare performance
results <- list(
  Logistic_Regression = evaluate_model(logit_model, test_data, "Logistic"),
  Naive_Bayes = evaluate_model(nb_model, test_data, "Naive Bayes"),
  Random_Forest = evaluate_model(rf_model, test_data, "RF")
)

# Print results
print(results)

## 6. Feature Importance ##
varImp(rf_model)
