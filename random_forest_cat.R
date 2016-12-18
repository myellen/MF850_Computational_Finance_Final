

source("test_train_split_by_date.R")


############################################
## Start Random Forest Stuff
############################################
install.packages("randomForest")
library(randomForest)
# Fit random forest with 1000 trees, test on test data 
fit_rf1 <- randomForest(x_train, y_train_cat, xtest = x_test, ytest = y_test_cat, ntree = 2000)

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





