set.seed(13)

library(splines)
library(mgcv)
library(MASS)
library(Metrics)
library(tidyverse)
library(gridExtra)

# Exploratory Data Analysis
school <- read.table("STATS-536/HW 3/SchoolResults.txt", header = T, sep = " ")

cor(school)
hist(school$Score)
plot(school)


# Train test split
trainIndex <- sample(seq_len(nrow(school)), size = 0.8 * nrow(school))
train <- school[trainIndex, ]    
test <- school[-trainIndex, ] 

X_train = train[-1]
y_train = train$Score

X_test = test[-1]
y_test = test$Score


# GAM model
gam_model <- gam(Score ~ s(Lunch) + Computer + Expenditure + s(Income) + s(English) + STratio, data=train)


plot(model_gam, residuals = TRUE, se = TRUE, rug = TRUE)
summary(model_gam)

gam_train_predict <- predict(model_gam, newdata = X_train, type = "response")
gam_train_RMSE <- rmse(y_train, gam_train_predict)
gam_train_RMSE

gam_test_predict <- predict(model_gam, newdata = X_test, type = "response")
gam_test_RMSE <- rmse(y_test, gam_test_predict)
gam_test_RMSE

gam.test.errors = numeric(nrow(school))

for(i in 1:nrow(school)){
  train = school[-i,]
  test = school[i,]

  ### GAM
  # gam will cv itself, but we want to compare OOS MSE
  model = gam(Score ~ s(Lunch) + Computer + Expenditure + s(Income) + s(English) + STratio,data=train)
  yhat = predict(model,newdata = test)
  gam.test.errors[i] = test$Score - yhat
}

sqrt(mean(gam.test.errors^2))

# Linear Model
model_lm <- lm(Score ~ ., data = school)
summary(model_lm)


full_model <- lm(Score ~ (Lunch + Computer + Expenditure + Income + English + STratio)^2, data = school)
best_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(best_model)

lm_train_predict <- predict(best_model, newdata = X_train, type = "response")
lm_train_RMSE <- rmse(y_train, lm_train_predict)
lm_train_RMSE

lm_test_predict <- predict(best_model, newdata = X_test, type = "response")
lm_test_RMSE <- rmse(y_test, lm_test_predict)
lm_test_RMSE


# Initialize a vector to store the errors
lm.test.errors = numeric(nrow(school))

# LOOCV
for(i in 1:nrow(school)){
  # Training data excludes the ith observation
  train = school[-i,]
  # Test data is just the ith observation
  test = school[i,]
  
  # Fit the linear model using the training data
  model = lm(Score ~ Lunch + Computer + Expenditure + Income + English + 
               STratio + Lunch:Computer + Lunch:Expenditure + Lunch:Income + 
               Lunch:STratio + Computer:Expenditure + Computer:Income + 
               Expenditure:Income + Expenditure:STratio, data = train)
  
  # Predict on the test data
  yhat = predict(model, newdata = test)
  
  # Calculate the error for the ith observation
  lm.test.errors[i] = test$Score - yhat
}

# Compute the RMSE from the test errors
sqrt(mean(lm.test.errors^2))




# Polynomial Model
poly_model <- lm(Score ~ Lunch + Computer + Expenditure +
                   poly(Income) + poly(English, 2) + poly(STratio, 2), data = school)

summary(poly_model)

poly_train_predict <- predict(poly_model, newdata = X_train, type = "response")
poly_train_RMSE <- rmse(y_train, poly_train_predict)
poly_train_RMSE

poly_test_predict <- predict(poly_model, newdata = X_test, type = "response")
poly_test_RMSE <- rmse(y_test, poly_test_predict)
poly_test_RMSE

###
### Testing LOOCV to get the RMSE for a ploynomial fit model
###

poly.max.df = 7
poly.test.errors = matrix(NA, ncol=poly.max.df, nrow=nrow(school))

for(i in 1:nrow(school)){
  train2 = school[-i,]
  test2 = school[i,]
  
  for(j in 1:poly.max.df){
    model = lm(Score ~ Lunch + Computer + Expenditure +
                 poly(Income) + poly(English, degree = j) + poly(STratio, degree = j),,data=train)
    yhat = predict(model,newdata = test)
    poly.test.errors[i,j] = test$Score - yhat
  }

}

sqrt(colMeans(poly.test.errors^2))

### 
### Visualization
###

par(mfrow=c(1,3))  # Adjust plot layout to display 2x2
plot(gam_model, se=TRUE, shade=TRUE, col="blue")


# Set up the plotting area with increased margins and space for labels
par(mfrow=c(1,3), mar=c(5,4,2,2), oma=c(0,0,2,0))  # Adjust margins and layout

# Plot the GAM smooth for Lunch
plot(gam_model, select = 1, se = TRUE, shade = TRUE, 
     col = "green4", 
     shade.col = "lightgreen",
     xlab = "Lunch", 
     ylab = "Smooth Estimate",
     main = "Effect of Lunch on Score", 
     ylim = c(-100, 100),
     lwd = 2)  # Adjust y-limits for better visualization
grid()

# Plot the GAM smooth for Income
plot(gam_model, select = 2, se = TRUE, shade = TRUE, 
     col = "gold4", 
     shade.col = "lightgoldenrod",
     xlab = "Income", 
     ylab = "Smooth Estimate",
     main = "Effect of Income on Score", 
     ylim = c(-100, 100),
     lwd = 2)  # Adjust y-limits for better visualization
grid()

# Plot the GAM smooth for English
plot(gam_model, select = 3, se = TRUE, shade = TRUE, 
     col = "firebrick", 
     shade.col = "lightcoral",
     xlab = "English", 
     ylab = "Smooth Estimate",
     main = "Effect of English on Score", 
     ylim = c(-100, 100),
     lwd = 2)  # Adjust y-limits for better visualization
grid()

# Add a super title to the overall plot
mtext("GAM Smooth Plots", outer = TRUE, cex = 1.5)


par(mfrow=c(1,1))

# Create the Income vs. Score plot
plot_income <- ggplot(school, aes(x = Income, y = Score)) +
  geom_point(color = "gold4") +  # Set points color to gold4
  geom_smooth(method = "gam", formula = y ~ s(x), color = "gold4", se = FALSE) +  # Smooth line in gold4
  ggtitle("Income vs Score") +
  xlab("Income") +
  ylab("Score") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Create the English vs. Score plot
plot_english <- ggplot(school, aes(x = English, y = Score)) +
  geom_point(color = "firebrick") +  # Set points color to firebrick
  geom_smooth(method = "gam", formula = y ~ s(x), color = "firebrick", se = FALSE) +  # Smooth line in firebrick
  ggtitle("English vs Score") +
  xlab("English") +
  ylab("Score") +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Combine the plots side by side
combined_plot <- plot_income + plot_english + plot_layout(ncol = 2)

# Save the combined plot as a PDF
ggsave("income_lunch_plots.pdf", combined_plot, width = 10, height = 5)
