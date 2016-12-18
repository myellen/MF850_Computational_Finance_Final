# Test train split 

# Read data 
data <- read.csv("mf850-finalproject-data.csv")

# Observatoins per month 
summary(data$Date)
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

# Naming work around to keep variable names consistent 
Industry_train <- Industry_test
x_test <- cbind(test_set, Industry_train)

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
# write.csv(test_set, file = "Test_set.csv")
# write.csv(train_set, file = "Train_set.csv")

# Remove variables we will not need 
rm(data, train_set1, train_set2, pre_scale_test, 
   pre_scale_train, Industry_train, Industry_test,
   train_set, test_set)




