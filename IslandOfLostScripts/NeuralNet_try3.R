# Neural Network for regression 

# Import data
data <- read.csv("mf850-finalproject-data.csv")
y <- data$RETMONTH
# Peak at data again 

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
x_test <- data[test_low:test_high, ]
x_train <- data[train_low:train_high, ]

# Some benchmarks for regression and categorical analysis 

# Baseline MSE from guessing the mean for in sample accuracy 
MSE_train <- mean( (y_train - mean(y_train)) ^ 2)
# Baseline MSE from guessing the mean for test (out of sample) accuracy  
MSE_test <- mean( (y_test - mean(y_test)) ^ 2)


########################################
###### Neural Network 
####################################

install.packages("nnet")
library(nnet)

nn1 <- nnet(x_train, y_train, size = 15, maxit = 400)
names(nn1)

# In sample MSE 
(MSE_nn1 <- mean((nn1$fitted.values - y_train)^2))
MSE_train

# Out sample predictions 
nn1_prediction <- predict(nn1, x_test)
# Out sample MSE 
MSE_test_nn1 <- mean((nn1_prediction - y_test) ^ 2)
MSE_test_nn1
MSE_test

# Sequence to hidden layers to try 
hidden_layers = seq(from = 2, to = 74, by = 3)

# Pre allocate space 
CV_nn_hidden_layers <- data.frame(matrix(0, nrow =length(hidden_layers), 
                                         ncol = 2))
names(CV_nn_hidden_layers)[1:2] <- c("Hidden Layer", "MSE")

