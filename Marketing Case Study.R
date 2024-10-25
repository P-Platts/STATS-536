# Set the directory
setwd("C:/Users/patri/Documents/Statistics/Stats 536/Unit 4/Homework")

# load packages here
library(tidyverse)
library(corrplot)  # for the correlation matrix
library(bestglm)  # for variable selection
library(car)  # for the VIFs
library(pROC)  # for the ROC curve
library(ROCR)  # for the color-coded ROC curve
library(glmnet)  # for ridge, lasso, and elastic net

#### EDA on the Classsification ####
marketing <- read.csv("TargetedMarketing.csv", header = T, sep = ";")

# Checking the types of the data
summary(marketing)
marketing$education <- factor(marketing$education)
marketing$marital <- factor(marketing$marital)
marketing$job <- factor(marketing$job)
marketing$default <- factor(marketing$default)
marketing$housing <- factor(marketing$housing)
marketing$loan <- factor(marketing$loan)
marketing$contact <- factor(marketing$contact)
marketing$month <- factor(marketing$month)
marketing$day_of_week <- factor(marketing$day_of_week)
marketing$poutcome <- factor(marketing$poutcome)
# marketing$y <- factor(marketing$y)

# Making the response variable 0 or 1
marketing$y <- ifelse(marketing$y == "yes", 1, 0)

# Trying a Boxplot
boxplot(marketing$y ~ marketing$age)
boxplot(marketing$job ~ marketing$y)

# Putting a smooth fit to a scatterplot(NOT CURRENTLY HELPFUL)
# scatter.smooth(x = marketing$campaign, y = as.numeric(marketing$y) - 1)
# scatter.smooth(x = marketing$age, y = as.numeric(marketing$y) -1)
# scatter.smooth(x = marketing$pdays, y = as.numeric(marketing$y) - 1)
# scatter.smooth(x = marketing$previous, y = as.numeric(marketing$y) - 1)

# See if changing the variables makes it easier to use bestglm, first make a new dataset to store the values
marketing2 <- marketing
marketing2$education <- as.numeric(marketing$education)
marketing2$marital <- as.numeric(marketing$marital)
marketing2$job <- as.numeric(marketing$job)
marketing2$default <- as.numeric(marketing$default)
marketing2$housing <- as.numeric(marketing$housing)
marketing2$loan <- as.numeric(marketing$loan)
marketing2$contact <- as.numeric(marketing$contact)
marketing2$month <- dplyr::recode(marketing$month, 
                                "mar" = 1, "apr" = 2, "may" = 3,
                                "jun" = 4, "jul" = 5, "aug" = 6,
                                "sep" = 7, "oct" = 8, "nov" = 9,
                                "dec" = 10)
marketing2$day_of_week <- as.numeric(marketing$day_of_week)
marketing2$poutcome <- as.numeric(marketing$poutcome)

#### Looking at models now ####

# Selecting the best model through BIC (NOT WORKING; it has something to do with the factors)
marketing_best_subsets_bic <- bestglm(as.data.frame(marketing2),
                                  IC = "BIC",
                                  method = "exhaustive",
                                  TopModels = 1,
                                  family = binomial)
summary(marketing_best_subsets_bic$BestModel)


# Looking at subset selection and cross validation using lasso
set.seed(12345)  # make sure to set your seed when doing cross validation!

env_x <- model.matrix(~ .^2 - 1, data = marketing[, -which(names(marketing) == "y")]) 
env_y <- marketing2$y
# use cross validation to pick the "best" (based on MSE) lambda
env_lasso_cv <- cv.glmnet(x = env_x,
                          y = env_y,
                          type.measure = "mse",
                          alpha = 1)  # 1 is code for "LASSO"
coef(env_lasso_cv, s = "lambda.1se")

# Extract non-zero coefficients, including the intercept
coefficients <- as.matrix(coef(env_lasso_cv, s = "lambda.1se"))

# Filter non-zero coefficients and get their names
non_zero_coef <- coefficients[coefficients != 0, , drop = FALSE]
selected_predictors <- rownames(non_zero_coef)[-1]  # Exclude the intercept

# Creating a new data set to test
test_data <- env_x[]

# Selecting a threshold 

# Get fitted values for training data using lambda.1se
fitted_values <- predict(env_lasso_cv, newx = env_x, s = "lambda.1se")


Threshold = seq(0,1,by=0.001)
false.negative.rate <- false.positive.rate <- true.positive.rate <- error.rate <- (0*Threshold-99)

for(i in 1:length(Threshold)){
  y.hat = ifelse(fitted_values < Threshold[i],0,1)
  error.rate[i] = mean(env_y!=y.hat)
  TP = sum(env_y==1 & y.hat==1)
  TN = sum(env_y==0 & y.hat==0)
  FP = sum(env_y==0 & y.hat==1)
  FN = sum(env_y==1 & y.hat==0)
  false.negative.rate[i] = FN/(FN+TP) # 1-sensitivity
  false.positive.rate[i] = FP/(FP+TN) # 1-specificity
}

## ROC Curve
## Errors plot from slide
plot(Threshold,error.rate,ylim=c(0,1),ylab='Error Rate',type='l',lwd=2)
lines(Threshold,false.negative.rate,col=4,lty=2,lwd=2)
lines(Threshold,false.positive.rate,col=2,lty=3,lwd=2)

## ROC
plot(false.positive.rate,1 - false.negative.rate
     ,xlab = "False Positive Rate (1-Specificity)",ylab = "True Positive Rate (Sensitivity)"
     ,col=4,type='l',lwd=2
)
abline(0,1,lty=3)
# AUC curve
# Get an AUC
# library(pROC)
roc(marketing$y,fitted_values)


### Out of sample: use predict instead of model$fitted
# Make predictions on the test data
predictions <- predict(env_lasso_cv, newx = test_data, s = "lambda.1se")


####
# Looking at subset selection and cross validation using lasso
set.seed(12345)  # make sure to set your seed when doing cross validation!

env_x2 <- model.matrix(~ . - 1, data = marketing[, -which(names(marketing) == "y")]) 
env_y2 <- marketing$y
# use cross validation to pick the "best" (based on MSE) lambda
env_lasso_cv2 <- cv.glmnet(x = env_x2,
                          y = env_y2,
                          type.measure = "mse",
                          alpha = 1)  # 1 is code for "LASSO"
coef(env_lasso_cv2, s = "lambda.1se")



# Extract non-zero coefficients, including the intercept
coefficients2 <- as.matrix(coef(env_lasso_cv2, s = "lambda.1se"))

# Filter non-zero coefficients and get their names
non_zero_coe2f <- coefficients2[coefficients2 != 0, , drop = FALSE]
selected_predictors2 <- rownames(non_zero_coef2)[-1]  # Exclude the intercept

# Print the selected predictors
print(selected_predictors2)





# # Extract non-zero coefficients, including the intercept
# coefficients <- coef(env_lasso_cv, s = "lambda.1se")
# non_zero_coef <- coefficients[coefficients != 0]
# print(non_zero_coef)
# 
# # Get the names of non-zero coefficient predictors (excluding the intercept)
# selected_predictors <- rownames(coefficients)[-1]  # Exclude intercept if it exists
# # Subset the test data to only include these predictors
# test_data_subset <- env_x[, selected_predictors]
# 
# # Make predictions on the test data
# predictions <- predict(env_lasso_cv, newx = as.matrix(test_data_subset), s = "lambda.1se")
# 
# # Calculate Mean Squared Error (MSE)
# actual_values <- marketing$y  # Adjust to the actual response variable name
# rmse <- sqrt(mean((predictions - actual_values)^2))
# print(rmse)
