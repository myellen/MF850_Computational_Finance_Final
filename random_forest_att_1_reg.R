# Template for importing data into train and test sets 
# Also need to remove some variables 

# Import data
data <- read.csv("mf850-finalproject-data.csv")
y <- data$RETMONTH
# Peak at data again 
# summary(data)

# Determine test and train size 
test_size <- 300 
train_size <- 2000 

# Store  test and train response variable 

# Test set is last test_size
# Training set is next train_size after test_size 

test_high <- nrow(data)
test_low <- test_high - test_size # Lower row number for test set 
train_high <- test_low - 1 # Upper row number for training set 
train_low <- train_high - train_size # Lower row number for training set  

# Subset the Response variable into test and training sets 
y_test <- y[test_low:test_high]
y_train <- y[train_low:train_high]

# Remove Date, Industry, Returns (response variables)
data$Date <- NULL
data$Industry <- NULL 
data$RETMONTH <- NULL 

# Create training set 
data <- scale(data)
x_test <- data[test_low:test_high,]
x_train <- data[train_low:train_high, ]

# Some benchmarks for regression and categorical analysis 

# Baseline MSE from guessing the mean for in sample accuracy 
MSE_train <- mean((y_train - mean(y_train))^2)
# Baseline MSE from guessing the mean for test (out of sample) accuracy  
MSE_test <- mean((y_test - mean(y_test))^2)



############################################
## Start Random Forest Stuff
############################################
install.packages("randomForest")
library(randomForest)
# Fit random forest with 1000 trees, test on test data 
fit_rf2 <- randomForest(x_train, y_train, xtest = x_test, ytest = y_test, ntree = 1000)


names(fit_rf2)
# Calculate in sample MSE 
(MSE_train_rf <- mean((fit_rf2$predicted - y_train)^2))
# Compared to baseline in sample MSE
MSE_train
# Percentage increase 
((abs(MSE_train_rf - MSE_train))/MSE_train)*100

# Out of sample MSE 
(MSE_rf <- mean((fit_rf2$test$predicted - y_test)^2))
# Compared to baseline 
MSE_test

# Very small gain percentage gain over regular 
(abs(MSE_rf - MSE_test)/MSE_test)*100


