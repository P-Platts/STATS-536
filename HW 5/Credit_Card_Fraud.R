rm(list = ls())

### Credit Card Fraud ###
setwd("C:/Users/patri/Documents/Statistics/Stats 536/Unit 5/Homework 5")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(smotefamily)
library(xgboost)
library(gbm)
library(pROC)

# Read in the Credit Card Fraud data
fraud <- read.csv("CCFraud.csv")

# Preprocess the data
fraud <- fraud[-1]  # Remove the first column
fraud$Class <- as.factor(fraud$Class)
fraud$Class <- ifelse(fraud$Class == 0, "No", "Yes")  # Convert to "No" and "Yes"

# Split data into training and testing datasets
set.seed(1342)
training <- sample(seq_len(nrow(fraud)), size = floor(0.7 * nrow(fraud)))
train <- fraud[training, ]
test <- fraud[-training, ]

# Making the response a factor
train$Class <- as.factor(train$Class)
test$Class <- as.factor(test$Class)

# Display class distribution
cat("Class distribution in training set:\n")
print(table(train$Class))
cat("Class distribution in testing set:\n")
print(table(test$Class))

# Apply SMOTE to handle class imbalance
train.smote <- SMOTE(X = train[, -ncol(train)], target = train[, ncol(train)], K = 50, dup_size = 120)
smote.train_data <- train.smote$data
smote.train_data$class <- as.factor(smote.train_data$class)

# Prepare train data for XGBoost
train_matrix <- as.matrix(smote.train_data[, -ncol(smote.train_data)])  # Exclude class column
train_label <- as.numeric(smote.train_data$class) - 1  # Convert to binary (0, 1)

# Prepare test data for XGBoost
test_matrix <- as.matrix(test[, -ncol(test)])  # Exclude class column
test_label <- as.numeric(test$Class) - 1  # Convert to binary (0, 1)

# Prepare the test data as a DMatrix
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

# Define a watchlist for monitoring both training and test metrics
watchlist <- list(train = dtrain, test = dtest)

# Train an XGBoost model
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc"
)

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)

# Train the XGBoost model with the watchlist
train_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = 100,
  watchlist = watchlist,  # Specify the watchlist
  verbose = 1  # Display progress
)



# Threshold Analysis
Threshold <- seq(0, 1, by = 0.01)
error.rate <- numeric(length(Threshold))
false.positive.rate <- numeric(length(Threshold))
false.negative.rate <- numeric(length(Threshold))

for (i in seq_along(Threshold)) {
  y.hat <- ifelse(y_hat > Threshold[i], 1, 0)
  TP <- sum(test_labels == 1 & y.hat == 1)
  TN <- sum(test_labels == 0 & y.hat == 0)
  FP <- sum(test_labels == 0 & y.hat == 1)
  FN <- sum(test_labels == 1 & y.hat == 0)
  
  error.rate[i] <- mean(test_labels != y.hat)
  false.negative.rate[i] <- FN / (FN + TP)  # 1 - sensitivity
  false.positive.rate[i] <- FP / (FP + TN)  # 1 - specificity
}

# Plot Error Rates
plot(Threshold, error.rate, ylim = c(0, 1), type = "l", lwd = 2, ylab = "Rate", xlab = "Threshold")
lines(Threshold, false.negative.rate, col = "blue", lty = 2, lwd = 2)
lines(Threshold, false.positive.rate, col = "red", lty = 3, lwd = 2)
legend("topright", legend = c("Error Rate", "False Negative Rate", "False Positive Rate"),
       col = c("black", "blue", "red"), lty = c(1, 2, 3), lwd = 2)

y_pred <- ifelse(y_hat >= 0.7, 1, 0)  # Default threshold is 0.5
confusion_matrix <- table(Predicted = y_pred, Actual = test_labels)
cat("Confusion Matrix:\n")
print(confusion_matrix)

y_hat <- predict(train_model, newdata = test_matrix)
mean(y_hat)

# Step 1: Threshold predictions to binary
threshold <- 0.8
y_pred <- ifelse(y_hat >= threshold, 1, 0)

# Assuming you have true labels for the test data
y_true <- as.numeric(test$Class) - 1  # Replace with your actual true labels

# Step 2: Compute confusion matrix components
TP <- sum(y_true == 1 & y_pred == 1)
TN <- sum(y_true == 0 & y_pred == 0)
FP <- sum(y_true == 0 & y_pred == 1)
FN <- sum(y_true == 1 & y_pred == 0)

# Step 2.1: Putting the confusion matrix together
# Construct confusion matrix
confusion_matrix <- table(Predicted = y_pred, Actual = y_true)

# Display the confusion matrix
cat("Confusion Matrix:\n")
print(confusion_matrix)

# Step 3: Calculate Precision, Recall, and F1-Score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Output results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")


# Smote data again
# Step 1: Apply SMOTE to handle class imbalance
smote <- SMOTE(X = fraud[, -ncol(fraud)], target = fraud[, ncol(fraud)], K = 50, dup_size = 120)
smote_data <- smote$data
colnames(smote_data)[ncol(smote_data)] <- "class"  # Rename the target column for clarity
smote_data$class <- as.factor(smote_data$class)    # Ensure the target is a factor

# Step 2: Prepare data for xgboost
X <- as.matrix(smote_data[, -ncol(smote_data)])    # Features
y <- as.numeric(smote_data$class) - 1             # Convert factor to numeric (0, 1)

# Step 3: Train the xgboost model on SMOTE data
model <- xgboost(
  data = X,                       # Oversampled features
  label = y,                      # Oversampled labels
  objective = "binary:logistic",  # Binary classification
  nrounds = 100,                  # Number of boosting rounds
  verbose = 0                     # Suppress output during training
)




# Step 1: Load the data
fraud_question <- read.csv("IsFraudulent.csv")

# Step 2: Preprocess the data
test <- fraud_question[, -1]  # Remove the first column (assuming it's an ID or irrelevant)

# Confirm changes
cat("Data Structure:\n")
str(test)  # Check the structure of the dataset
cat("\nSummary of the Data:\n")
summary(test)  # Verify the data preprocessing

# Step 4: Prepare test data for XGBoost
test_matrix2 <- as.matrix(test)  # Exclude the Class column

# Step 5: Make predictions
y_hat <- predict(model, newdata = test_matrix2)

# Step 6: Threshold predictions to binary
threshold <- 0.8  # Customize threshold as needed
y_pred <- ifelse(y_hat >= threshold, 1, 0)

