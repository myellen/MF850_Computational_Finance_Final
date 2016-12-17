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

# Test and training sets to csv 
# write.csv(test_set, file = "Test_set.csv")
# write.csv(train_set, file = "Train_set.csv")

# Remove variables we will not need 
rm(data, train_set1, train_set2)
