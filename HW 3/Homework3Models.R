set.seed(13)

library(splines)
library(mgcv)
library(MASS)
library(Metrics)


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
model_gam <- gam(Score ~ s(Lunch) + Computer + Expenditure + s(Income) + s(English) + STratio, data=train)


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

