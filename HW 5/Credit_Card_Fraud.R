### Credit Card Fraud ###
setwd("C:/Users/patri/Documents/Statistics/Stats 536/Unit 5/Homework 5")

# Load the smotefamily package
library(smotefamily)



# Read in the Credit Card Fraud data
fraud <- read.csv("CCFraud.csv")
summary(fraud)
head(fraud)

# Removing the first column
fraud <- fraud[-1]

# Changing the variable types
fraud$Class <- as.factor(fraud$Class)

# Doing some intial EDA
plot(fraud$Amount, fraud$Class)
unique(fraud$Class)
cor(fraud[-length(fraud)]) # No correlation because of Principal components

# Very few cases of Fraud in the data set only 491(2) from what I found
# We could use SMOTE to deal with the few cases

smote_fraud <- SMOTE(X = fraud, target = fraud$Class, K = 50, dup_size = 50)

# Not going to do logistic regression as one of our methods because we did it
# in the last homework. Potential methods to try are bagging, knn, Support vector
# machines, or LDA/QDA. No, assumptions for KNN, bagging, or support vector 
# machines. 

## Chosen methods: BART, Boosting

## Testing KNN

# Generating row indices for training set
training_indices <- sample(1:nrow(fraud), size = 0.8 * nrow(fraud), replace = FALSE)

# Splitting the data into training and test sets
train <- fraud[training_indices, ]
test <- fraud[-training_indices, ]

library(class)
model1 <-  knn(train = train, test = test, cl = train$Class, k = 3)
plot(x.test,y.test,col=as.numeric(model1),pch=19)


# De-correlate
XX <- X.scaled%*%solve(chol(cov(X.scaled)))

model2 = knn(train = XX[train,], test=XX[test,], cl = fraud$Class, k = 5)
plot(x.test,y.test,col=as.numeric(model2),pch=19)


# larger k

model3 = knn(train = XX[train,], test=XX[test,], cl = fraud$Class, k = 200)
plot(x.test,y.test,col=as.numeric(model3),pch=19)
