# Test train split 

source("MF850Utilities.R")

# Read data from source
data <- loadLaggedDataSet()
# Re move columns with NA 
data <- na.omit(data)

# Observatoins per month 
summary(data$Date)
# Not equal number of observations per month 

# Create test set - the last month of observations 
test_set <- data[data$Date == "2015-11-30", ]

y_test <- test_set$futurereturns

# Create train set - previous 2 months
# First take one month 
train_set1 <- data[data$Date == "2015-10-31", ]
train_set2 <- data[data$Date == "2015-09-30", ]

# Combine two months for training set  
train_set <- rbind(train_set1, train_set2)
y_train <- train_set$futurereturns

# Remove company id - (there should only be one per month)
test_set$compid <- NULL
train_set$compid <- NULL
# Remove Date 
test_set$Date <- NULL
train_set$Date <- NULL
# Remove RETMONTH
test_set$RETMONTH <- NULL
train_set$RETMONTH <- NULL
# Remove SPX - unique per month 
test_set$RETMONTH_SPX <- NULL
train_set$RETMONTH_SPX <- NULL
# Remove future returns (saved in previous variable)
test_set$futurereturns <- NULL
train_set$futurereturns <- NULL

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
high_low_ratio_train <- sum(y_train_cat == 1)  / length(y_train_cat)

# Test set for categorical analysis 
y_test_cat <- ifelse(y_test > 0, 1, 0)
# Ratio of high vs low returns for TEST set 
high_low_ratio_test <- sum(y_test_cat == 1) / length(y_test_cat)
# A priori number to beat 

# Baseline MSE for training set- pick the mean 
MSE_train <- mean((y_train - mean(y_train)) ^ 2)
MSE_test <- mean((y_test - mean(y_train)) ^ 2)

# Test and training sets to csv 
# write.csv(test_set, file = "Test_set.csv")
# write.csv(train_set, file = "Train_set.csv")

# Remove variables we will not need 
# rm(data, train_set1, train_set2, pre_scale_test, 
#    pre_scale_train, Industry_train, Industry_test,
#    train_set, test_set)

## Remove functions from Utilities 
#rm(lagStockData, loadLaggedDataSet, loadpackages,
#   requiredpackages, laggedDataFile, mf850_finalproject_data)


# Practice set (without returns )
train_set3 <- data[data$Date == "2015-02-28", ]
train_set4 <- data[data$Date == "2015-01-31", ]
train_set5 <- rbind(train_set3, train_set4)
train_set_results <- train_set5$futurereturns
# Remove variables 
train_set5$RETMONTH <- NULL
train_set5$futurereturns <- NULL 
# Write results to file 
#write.csv(train_set5, file = "TestDF_no_returns.csv")
#write.csv(train_set_results, file = "TestDF_returns.csv")

