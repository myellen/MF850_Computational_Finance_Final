# Test train split 

# Read data 
data <- read.csv("mf850-finalproject-data.csv")

# Observatoins per month 
summary(data$Date)
# Not equal number of observations per month 

# Create test set - the last month of observations 
test_set <- data[data$Date == "2015-12-31", ]

# Create train set - previous 2 months
# First take one month 
train_set1 <- data[data$Date == "2015-11-30", ]
train_set2 <- data[data$Date == "2015-10-31", ]

# Combine two month 
train_set <- rbind(train_set1, train_set2)

# Remove company id - (there should only be one per month)
test_set$compid <- NULL
train_set$compid <- NULL
# Remove Date 
test_set$Date <- NULL
train_set$Date <- NULL


# SCALE DATA
# Need to separate categorical variables 
Industry_test <- test_set$Industry
Industry_train <- train_set$Industry

# Remove Industry from sets before we scale 
pre_scale_test <- within(test_set, rm("Industry"))
pre_scale_train <- within(train_set, rm("Industry"))

# Scale the continous variable 
test_set <- scale(pre_scale_test)
train_set <- scale(pre_scale_train)

# Add industry to sets 
test_set <- cbind(test_set, Industry_test)
train_set <- cbind(train_set, Industry_train) 


# Test and training sets to csv 
write.csv(test_set, file = "Test_set.csv")
write.csv(train_set, file = "Train_set.csv")

# Remove variables we will not need 
rm(data, train_set1, train_set2, pre_scale_test, 
   pre_scale_train, Industry_train, Industry_test)




