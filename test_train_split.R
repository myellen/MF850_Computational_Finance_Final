# Template for importing data into train and test sets 
# Also need to remove some variables 

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
MSE_test1 <- mean((y_test - mean(y_train)) ^ 2)


# Train set for categorical analysis 
y_train_cat <- ifelse(y_train > 0, 1, 0)
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio <- sum(y_train_cat == 1)  / length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- ifelse(y_test > 0, 1, 0)
# Ratio of high vs low returns for TEST set 
(high_low_ratio <- sum(y_test_cat == 1) / length(y_test_cat))
# A priori number to beat 



# Fun Comparision/ Sampling stuff - want to make sure we have a representative sample
# Plot histogram of full, test and training for response
par(mfrow = c(1, 3))
hist(y, breaks = 40)
hist(y_train, breaks = 40)
hist(y_test, breaks = 40)
# Notice that full has more extreme values and slightly more extreme

# Look at means for the samples 
c(mean(y), mean(y_train), mean(y_test))

par(mfrow = c(1, 1))
# 
qqplot(y, y_train)
qqline(y_train)
ks.test(y_train, y)
#
qqplot(y, y_test)
qqline(y_test)
ks.test(y_test, y)
# 
qqplot(y_test, y_train)
qqline(y_test)
ks.test(y_test, y_train)