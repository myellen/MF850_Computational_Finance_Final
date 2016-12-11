# Random forest for classification

# Template for importing data into train and test sets
# Also need to remove some variables

# Import data
data <- read.csv("mf850-finalproject-data.csv")
y <- data$RETMONTH
# Peak at data again
# summary(data)

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
x_test <- data[test_low : test_high, ]
x_train <- data[train_low : train_high, ]

# Train set for categorical analysis
y_train_cat <- as.factor(ifelse(y_train > 0, 1, 0))
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio_train <- sum(y_train_cat == 1) / length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- as.factor(ifelse(y_test > 0, 1, 0))
# Ratio of high vs low returns for TEST set 
(high_low_ratio_test <- sum(y_test_cat == 1) / length(y_test_cat))
# A priori number to beat 


############################################
## Start Random Forest Stuff
############################################
install.packages("randomForest")
library(randomForest)
# Fit random forest with 1000 trees, test on test data 
fit_rf1 <- randomForest(x_train, y_train_cat, xtest = x_test, ytest = y_test_cat, ntree = 1000)

# In sample confusion matrix 
(rf_confusion_train <- fit_rf1$confusion)
# In sample accuracy - sum the 1st and 4th element of matrix (6 elements total) 
# Divide by the total number of element (sum 1st through 4th elements)
sum(rf_confusion_train[c(1, 4)]) / sum(rf_confusion_train[1:4])
# Compare with in sample a priori estimate
high_low_ratio_train

# Out of sample accuracy 
(rf_confusion_test <- fit_rf1$test$confusion)
# Out of sample accuracy 
sum(rf_confusion_test[c(1, 4)]) / sum(rf_confusion_test[1:4])
# Compare to a priori guess 
high_low_ratio_test





