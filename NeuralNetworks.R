# Neural Network for regression 

# Import data
data <- read.csv("mf850-finalproject-data.csv")
#Scaling the Data
# Need to separate categorical variables 
data_industry <- (data$Industry)
data_date <- (data$Date)

# Remove Industry from sets before we scale 
pre_scale_data <- within(data, rm("Industry", "Date"))

# Scale the continous variable
data <- scale(pre_scale_data)

#Create data frames
x <- as.data.frame(data)

# Add industry to sets 
x$Industry <- Industry_frame
# Peak at data again 

# Determine test and train size 
test_size <- 3430
train_size <- 6772

# Store  test and train response variable 
# Test set is last test_size
# Training set is next train_size after test_size 

# Observatoins per month 
summary(x$Date)
# Not equal number of observations per month 

# Create test set - the last month of observations 
test_set <- x[x$Date == "2015-12-31", ]

# Create train set - previous 2 months
# First take one month 
train_set1 <- x[x$Date == "2015-11-30", ]
train_set2 <- x[x$Date == "2015-10-31", ]

# Combine two month 
train_set <- rbind(train_set1, train_set2)

# Test and training sets to csv 
write.csv(test_set, file = "Test_set.csv")
write.csv(train_set, file = "Train_set.csv")

train_data <- read.csv("Train_set.csv")
test_data <- read.csv("Test_set.csv")

# Remove Date, Company ID, Returns (response variables)
train_data[,1] <- NULL
train_data$RETMONTH <- NULL
train_data$compid <- NULL

test_data[,1] <- NULL
test_data$RETMONTH <- NULL
test_data$compid <- NULL

# Some benchmarks for regression and categorical analysis 

# Baseline MSE from guessing the mean for in sample accuracy 
MSE_train <- mean( (y_train - mean(y_train)) ^ 2)
# Baseline MSE from guessing the mean for test (out of sample) accuracy  
MSE_test <- mean((y_test - mean(y_train)) ^ 2)


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

