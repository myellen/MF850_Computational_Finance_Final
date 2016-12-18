# Import data
data <- read.csv("mf850-finalproject-data.csv")
#Scaling the Data
# Need to separate categorical variables 
data_industry <- (data$Industry)

# Remove Industry from sets before we scale 
pre_scale_data <- within(data_industry, rm("Industry"))

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
train_data$Date <- NULL
train_data$RETMONTH <- NULL
train_data$compid <- NULL

test_data[,1] <- NULL
test_data$Date <- NULL
test_data$RETMONTH <- NULL
test_data$compid <- NULL

# Some benchmarks for regression and categorical analysis 

# Baseline MSE from guessing the mean for in sample accuracy 
MSE_train <- mean( (y_train - mean(y_train)) ^ 2)
# Baseline MSE from guessing the mean for test (out of sample) accuracy  
MSE_test <- mean((y_test - mean(y_train)) ^ 2)

#Heirarchical Clustering

clusters <- hclust(dist(train_data[,#columns of training data]))