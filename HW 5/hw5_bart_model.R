library(smotefamily)
library(BART)
library(tidyverse)

# Load and preprocess data
fraud <- read.csv("CCFraud.csv")
fraud <- fraud[-1]

# Apply SMOTE
smote_fraud <- SMOTE(X = fraud, target = fraud$Class, K = 50, dup_size = 50)

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
bart.mod <- gbart(x.train = x, y.train = y, type = "pbart")
