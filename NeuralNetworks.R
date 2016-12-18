# Neural Network for regression 

# Import data
data <- read.csv("mf850-finalproject-data.csv")
# Not equal number of observations per month 

# Create test set - the last month of observations 
test_set <- data[data$Date == "2015-12-31", ]

y_test <- test_set$RETMONTH

# Create train set - previous 2 months
# First take one month 
train_set1 <- data[data$Date == "2015-11-30", ]
train_set2 <- data[data$Date == "2015-10-31", ]

# Combine two months for training set  
train_set <- rbind(train_set1, train_set2)
y_train <- train_set$RETMONTH

# Remove company id - (there should only be one per month)
test_set$compid <- NULL
train_set$compid <- NULL
# Remove Date 
test_set$Date <- NULL
train_set$Date <- NULL

# SCALE DATA
# Need to separate categorical variables 
Industry_test <- (test_set$Industry)
Industry_train <- (train_set$Industry)

# Remove Industry from sets before we scale 
pre_scale_test <- within(test_set, rm("Industry"))
pre_scale_train <- within(train_set, rm("Industry"))

# Scale the continous variable
test_set <- scale(pre_scale_test)
train_set <- scale(pre_scale_train)

#Create data frames
x_train <- as.data.frame(train_set)
x_test <- as.data.frame(test_set)

# Add industry to sets 
x_train$Industry <- Industry_train
x_test$Industry <- Industry_test

# Train set for categorical analysis 
y_train_cat <- ifelse(y_train > 0, 1, 0)
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio_train <- sum(y_train_cat == 1)  / length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- ifelse(y_test > 0, 1, 0)
# Ratio of high vs low returns for TEST set 
(high_low_ratio_test <- sum(y_test_cat == 1) / length(y_test_cat))
# A priori number to beat 

# Test and training sets to csv 
write.csv(test_set, file = "Test_set.csv")
write.csv(train_set, file = "Train_set.csv")

# Remove variables we will not need 
rm(data, train_set1, train_set2, pre_scale_test, 
   pre_scale_train, Industry_train, Industry_test,
   train_set, test_set)


# Some benchmarks for regression and categorical analysis 

# Baseline MSE from guessing the mean for in sample accuracy 
MSE_train <- mean( (y_train_cat - mean(y_train_cat)) ^ 2)
# Baseline MSE from guessing the mean for test (out of sample) accuracy  
MSE_test <- mean((y_test_cat - mean(y_train_cat)) ^ 2)


########################
### Neural Network part 
#######################

# Install packages 
#install.packages("neuralnet")
library(neuralnet)

# Format data for neuralnet command 
train_data <- cbind(x_train, y_train)
test_data <- cbind(x_test, y_test)

# Create formula manually (bug in neural net package )
f <- as.formula(c("y_train ~", (paste(colnames(x_train), collapse = " + "))))


# Train neural net
nnet1 <- neuralnet(f, data = train_data, hidden = 3, threshold = 0.01, stepmax = 1e5)

# Options for neural net 
names(nnet1)

# In sample accuracy 

# Compute responses from test data 
nnet_train_response <- compute(nnet1, x_train)$net.result
# In sample MSE 
(MSE_train_nnet <- mean((y_train - nnet_train_response)^2))
# Baseline MSE 
MSE_train
# Percent improvement 
(abs(MSE_train - MSE_train_nnet)/MSE_train)

# Out sample predictions
nnet_test_response <- compute(nnet1, x_test)$net.result
# Out sample MSE 
(MSE_test_nnet <- mean((y_test - nnet_test_response) ^ 2))
# Out sample baseline 
MSE_test


# parameter tuning - number of hidden layers for neural network 

# hidden layers to try from 3 - 51 by 3  
hidden_layers <- seq(from = 3, to = 51, by= 3)
# Pre allocate data frame space
nnet_hidden_layer_df <- data.frame(matrix(0, nrow = 4, ncol = 3))
# Rename columns 
names(nnet_hidden_layer_df)[1:3] <- c("Number of Hidden Layers", "MSE_in", "MSE_out")

for (hl in hidden_layers){
  # Index for storing things in data frame 
  df_index <- hl/3
  
  # Train neural net with varying hidden layer parameter 
  nnet1 <- neuralnet(f, data = train_data, hidden = hl, threshold = 0.01, stepmax = 1e5, rep = 8)

  # In sample accuracy 
  
  # Compute responses from test data 
  nnet_train_response_temp <- compute(nnet1, x_train)$net.result
  # In sample MSE 
  nnet_hidden_layer_df$MSE_in[df_index] <- mean((y_train - nnet_train_response_temp) ^ 2)
  
  # Out sample predictions
  nnet_test_response_temp <- compute(nnet1, x_test)$net.result
  # Out sample MSE 
  nnet_hidden_layer_df$MSE_out[df_index]<- mean((y_test - nnet_test_response_temp) ^ 2)
}

