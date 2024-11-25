rm(list=ls())

# Set working directory
setwd("C:/Users/patri/Documents/Statistics/Stats 536/STATS-536/HW 6")

library(tokenizers)
library(torch)


dat = read.csv('CompanyComplaints.csv')
dat$Department <- as.factor(dat$Department)
ss = dat[1:100,]
tokens = sapply(ss$Complaint,tokenize_words)
tally.word = function(X,target.word){
    sum(stringr::str_count(X,pattern = target.word))
}

x.debt <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'debt'))
x.collect <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'collect'))
x.credit <-  as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'credit'))
x.loan <-  as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'loan'))
x.length <- as.vector(sapply(X = tokens, FUN = length))
x.fraud <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'fraud'))
x.authority <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'authority'))
x.contract <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'contract'))
x.accounts <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'accounts'))
x.income <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'income'))
x.inaccuracies <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'inaccuracies'))
x.denied <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'denied'))
x.transactions <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'transactions'))
x.home <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'home'))
x.mortgage <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'mortgage'))
x.purchase <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'purchase'))
x.student <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'student'))
x.checking <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'checking'))
x.statement <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'statement'))
x.collection <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'collection'))
x.bill <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'bill'))
x.interest <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'interest'))
x.agency <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'agency'))
x.scam <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'scam'))
x.contact <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'contact'))
x.money<- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'money'))
x.complaint <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'complaint'))
x.balance <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'balance'))
x.identity <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'identity'))
x.theft <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'theft'))
x.insurance <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'insurance'))
x.payments <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'payments'))
x.reports <- as.vector(sapply(X = tokens, FUN = tally.word, target.word = 'reports'))

# Combine all vectors into a matrix
features_matrix <- cbind(
  x.debt, x.collect, x.credit, x.loan, x.length, x.fraud, x.authority,
  x.contract, x.accounts, x.income, x.inaccuracies, x.denied, x.transactions,
  x.home, x.mortgage, x.purchase, x.student, x.checking, x.statement, 
  x.collection, x.bill, x.interest, x.agency, x.scam, x.contact, x.money, 
  x.complaint, x.balance, x.identity, x.theft, x.insurance, x.payments, 
  x.reports
)

# Assign column names for clarity
colnames(features_matrix) <- c(
  "debt", "collect", "credit", "loan", "length", "fraud", "authority",
  "contract", "accounts", "income", "inaccuracies", "denied", "transactions",
  "home", "mortgage", "purchase", "student", "checking", "statement", 
  "collection", "bill", "interest", "agency", "scam", "contact", "money", 
  "complaint", "balance", "identity", "theft", "insurance", "payments", 
  "reports"
)

# Convert to a data frame for easier handling if needed
features_df <- as.data.frame(features_matrix)

# Add the response variable to the data frame (optional)
features_df$Department <- ss$Department

# Covariance Matrix
cor_matrix <- cor(features_df[,-ncol(features_df)])

# Convert correlation matrix to a long format for ggplot
cor_data <- as.data.frame(as.table(cor_matrix))

ggplot(cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +  # Tile plot
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Plot",
       x = NULL,
       y = NULL,
       fill = "Correlation")


library(rpart)
library(rpart.plot)
mod = rpart(Department ~ .,control = rpart.control(cp = 0.0), data = features_df)
rpart.plot(mod)



features_matrix <- cbind(
  x.collect, x.credit, x.debt, x.length, x.loan
)

colnames(features_matrix) <- c(
  "collect", "credit", "debt", "length", "loan"
)

features_df <- as.data.frame(features_matrix)
features_df$Department <- ss$Department

# Rearrange Department to be the first column
features_df <- features_df[, c("Department", setdiff(names(features_df), "Department"))]

# Split data into training and test sets
tst = sample(1:nrow(features_df), 20)
txt <- features_df
txt$Department <- as.factor(txt$Department)
Ytrain <- txt$Department[-tst]
Ytest <- txt$Department[tst]
Xtrain <- txt[-tst, 2:ncol(txt)]
Xtest <- txt[tst, 2:ncol(txt)]

# Standardize features
XtrainMean <- apply(Xtrain, 2, mean)
XtrainSd <- apply(Xtrain, 2, sd)
Xtrain_stand <- sweep(sweep(Xtrain, 2, XtrainMean, "-"), 2, XtrainSd, "/")
Xtest_stand <- sweep(sweep(Xtest, 2, XtrainMean, "-"), 2, XtrainSd, "/")

# Convert to tensors
x_train <- torch_tensor(as.matrix(Xtrain))
y_train <- torch_tensor(as.numeric(Ytrain) - 1)
x_test <- torch_tensor(as.matrix(Xtest))
y_test <- torch_tensor(as.numeric(Ytest) - 1)


net = nn_module(
  "class_net",
  initialize = function(){
    self$linear1 = nn_linear(16, 64) # 16 columns in the X matrix
    self$linear2 = nn_linear(64,64)
    self$linear3 = nn_linear(64,32)
    self$linear4 = nn_linear(32, length(unique(Ytrain))) # Update 9 if necessary
    
  },
  forward = function(x) {
    x %>%
      self$linear1() %>%
      nnf_relu() %>%
      self$linear2() %>%
      nnf_relu() %>%
      self$linear3() %>%
      nnf_relu() %>%
      self$linear4() %>%
      nnf_softmax(2)
  }
)

model2 = net()
#this will let us know how many parameters we have;
#note that there are a lot, but for a DNN, not that many
model2 

# Now, we define our loss function (cross-entropy), optimizer (adam), learning rate, and the number of epochs to consider for the SGD.

#define the cost and optimizer
criterion <- nn_cross_entropy_loss()
optimizer <- optim_adam(model2$parameters, lr = 0.01)

epochs = 150
loss_values <- numeric()
accuracy_values <- numeric()
test_loss_values <- numeric()

### Training the model
# Now, let's train the network (some of this is just the book-keeping of our loss and accuracy values).

for(i in 1: epochs){
  optimizer$zero_grad()
  y_pred = model2$forward(x_train)
  loss = criterion(y_pred, y_train)
  loss$backward()
  optimizer$step()
  
  ### administration:
  
  
  # Append loss and accuracy values to vectors
  loss_values <- c(loss_values, as.numeric(loss))
  
  
  # Calculate validation loss
  test_outputs = model2(x_test)
  test_loss = criterion(test_outputs, y_test)
  
  # Append the current validation loss to the vector
  test_loss_values <- c(test_loss_values, test_loss$item())
  
  
  #check training
  if(i %% 10 == 0){
    winners = y_pred$argmax(dim=2)
    corrects = (winners == y_train)
    accuracy = corrects$sum()$item() / y_train$size()
    
    cat("Epoch:", i, "Loss:", loss$item(),"Accuracy:", accuracy, "\n")
    accuracy_values <- c(accuracy_values, accuracy)
  }
}



# Plot the loss values
plot(1:epochs, loss_values, type="l", col="blue", xlab="Epochs", 
     ylab="Loss", main="Loss vs Epochs")
lines(1:epochs, test_loss_values, col="red")
legend("topright", legend=c("Training Loss", "Validation Loss"),
       col=c("blue", "red"), lty=1)




### Test Data Evaluation
# Let's see how well we do on the test data.  

yTST_pred = model2$forward(x_test)     #classify the new data
lossTST = criterion(yTST_pred, y_test) #get the loss for this classification
winnersTST = yTST_pred$argmax(dim=2)   #get the category with highest probability
#get accuracy
correctsTST = (winnersTST == y_test)
accuracyTST = correctsTST$sum()$item() / y_test$size()
cat("Test", "Loss:", lossTST$item(),"Accuracy:", accuracyTST, "\n")
