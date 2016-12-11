# Template for importing data into train and test sets 
# Also need to remove some variables 

# Import data
data <- read.csv("mf850-finalproject-data.csv")
y <- data$RETMONTH

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
x_test <- data[test_low:test_high,]
x_train <- data[train_low:train_high, ]



# Train set for categorical analysis 
y_train_cat <- as.factor(ifelse(y_train > 0, 1, 0))
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio <- sum(y_train_cat == 1)/length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- as.factor(ifelse(y_test > 0, 1, 0))
# Ratio of high vs low returns for TEST set 
(high_low_ratio <- sum(y_test_cat == 1)/length(y_test_cat))
# A priori number to beat 

#######################
### LDA 
########################

install.packages("caret")
install.packages("MASS")
library(MASS)
library(caret)

# Remove columns with near 0 variance 
x_train_edit <- x_train[, -c(23, 39)]
# Fit LDA 
fit_lda <- lda(x_train_edit, y_train_cat )
# Get sample accuracy - predict based on training data
pred_train <- predict(fit_lda, x_train_edit)
# in sample confusion matrix - without cross validation ( not as accurate without cv)
(confusion_train <- table(y_train_cat, pred_train$class))
# Accuracy percentage 
sum(diag(prop.table(confusion_train)))


# Out of sample prediction and accuracy 
# Change x_test data 
x_test_edit <- x_test[, -c(23, 39)]
# Predict outcomes based on training data  
pred_test <- predict(fit_lda, x_test_edit)
# Confusion matrix for test set predictions 
(confusion_test <- table(y_test_cat, pred_test$class))
# Out of sample accuracy 
sum(diag(prop.table(confusion_test)))
