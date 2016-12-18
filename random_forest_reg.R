source("test_train_split_by_date.R")
############################################
## Start Random Forest Stuff
############################################
install.packages("randomForest")
library(randomForest)

x_test$Industry <- NULL
x_train$Industry <- NULL
# Fit random forest with 1000 trees, test on test data 
fit_rf2 <- randomForest(x_train, y_train, xtest = x_test, ytest = y_test, ntree = 1000)


names(fit_rf2)
# Calculate in sample MSE
(MSE_train_rf <- mean((fit_rf2$predicted - y_train) ^ 2))
# Compared to baseline in sample MSE
MSE_train
# Percentage increase
((abs(MSE_train_rf - MSE_train)) / MSE_train) * 100

# Out of sample MSE
(MSE_rf <- mean((fit_rf2$test$predicted - y_test) ^ 2))
# Compared to baseline
MSE_test

# Very small gain percentage gain over regular 
(abs(MSE_rf - MSE_test) / MSE_test) * 100

