library(smotefamily)
library(BART)
library(tidyverse)
library(rsample)

# Load and preprocess data
fraud <- read.csv("CCFraud.csv")
fraud <- fraud[-1]

set.seed(1234)
split <- initial_split(fraud, prop = 0.7, strata = "Class")  # 70% train
train_data <- training(split)
test_data <- testing(split)

# Apply SMOTE
smote_fraud <- SMOTE(X = train_data, target = train_data$Class, K = 50, dup_size = 50)

# Prepare x and y
x <- smote_fraud$data %>%
  select(-Class, -class) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.factor), as.numeric))

y <- smote_fraud$data$class
if (!is.numeric(y)) {
  y <- as.numeric(y) 
}

# Remove incomplete cases
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]

# # Optional: Subset data if it's too large
# if (nrow(x) > 10000) {
#   subset_indices <- sample(seq_len(nrow(x)), size = 10000)
#   x <- x[subset_indices, ]
#   y <- y[subset_indices]
# }

# Fit BART model
bart.mod <- gbart(x.train = x, y.train = y, x.test = test_data, type = "pbart", ndpost = 5, nskip = 100)

y_hat_pre <- bart.mod$yhat.train[,]
y_hat <- pnorm(y_hat_pre)
y_hat_mean <- colMeans(y_hat)

# Step 1: Threshold predictions to binary
threshold <- 0.6
y_pred <- ifelse(y_hat_mean >= threshold, 1, 0)

# Assuming you have true labels for the test data
y_true <- smote_fraud$data$class  # Replace with your actual true labels

# Step 2: Compute confusion matrix components
TP <- sum((y_pred == 1) & (y_true == 1))
FP <- sum((y_pred == 1) & (y_true == 0))
FN <- sum((y_pred == 0) & (y_true == 1))

# Step 3: Calculate Precision, Recall, and F1-Score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Output results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")


y_hat_pre_test <- bart.mod$yhat.test[,]
y_hat_test <- pnorm(y_hat_pre_test)
y_hat_mean_test <- colMeans(y_hat_test)

# Step 1: Threshold predictions to binary
threshold <- 0.6
y_pred <- ifelse(y_hat_mean_test >= threshold, 1, 0)

# Assuming you have true labels for the test data
y_true <- test_data$Class  # Replace with your actual true labels

# Step 2: Compute confusion matrix components
TP <- sum((y_pred == 1) & (y_true == 1))
FP <- sum((y_pred == 1) & (y_true == 0))
FN <- sum((y_pred == 0) & (y_true == 1))

# Step 3: Calculate Precision, Recall, and F1-Score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)

# Output results
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

names(bart.mod)
dim(bart.mod$yhat.train)
dim(bart.mod$yhat.test)
a = bart.mod$yhat.train[,1:10]
a
pnorm(a)
dim(y_hat)
