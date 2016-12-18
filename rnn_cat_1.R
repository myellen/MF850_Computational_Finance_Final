# Try using recurrent nueral networks

# Import data
data <- read.csv("mf850-finalproject-data.csv")
y <- data$RETMONTH
# Peak at data again 
# summary(data)

# Determine test and train size 
test_size <- 300 
train_size <- 10000 

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
Date <- data$Date[test_low:test_high]
data$Date <- NULL
data$Industry <- NULL 
data$RETMONTH <- NULL 

# Create training set 
data <- scale(data)
x_test <- data[test_low:test_high,]
x_train <- data[train_low:train_high, ]

# Train set for categorical analysis 
y_train_cat <- ifelse(y_train >0, 1, 0)
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio <- sum(y_train_cat == 1)/length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- ifelse(y_test >0, 1, 0)
# Ratio of high vs low returns for TEST set 
(high_low_ratio <- sum(y_test_cat == 1)/length(y_test_cat))
# A priori number to beat 


##########################
## RNN Practice 
##########################

install.packages("rnn")
library(rnn)

max((x_train))

x_train_arr <- array(x_train, dim = c(dim(x_train),ncol(x_train) ))
y_train_arr <- array(y_train, dim = c(length(y_train),ncol(x_train),1))

# Train RNN 
rnn1 <- trainr(Y = y_train_arr[,dim(y_train_arr)[2]:1,,drop = F], 
               X = x_train_arr[,dim(x_train_arr)[2]:1,,drop = F], 
               learningrate = 0.1, 
               hidden_dim = 20, 
               batch_size = 100, 
               numepochs = 7)

# In sample accuracy 
# Use RNN to predict against the training data 
rnn_predictions <- predictr(rnn1, 
                            x_train_arr[,dim(x_train_arr)[2]:1,,drop = F])
# Reverse transform 
rnn_predictions1 <- rnn_predictions[,dim(rnn_predictions)[2]:1]

dim(rnn_predictions1) 

# Test RNN 
