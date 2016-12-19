

source("test_train_split_by_date.R")


############################################
## Start Random Forest Stuff
############################################
#install.packages("randomForest")
library(randomForest)

# Data prep for random forest categorization - need factors 
y_test_cat <- as.factor(y_test_cat)
y_train_cat <- as.factor(y_train_cat)
# Random forest cannot handle categorical variables with more than 53 categories... 
x_train$Industry <- NULL
x_test$Industry <- NULL

# Number of of trees 
numb_tree <- 10
# Pre allocate space for random forest 
rf_tree_err <- data.frame(matrix(0, ncol = 3, nrow = numb_tree))
# Rename columns -- really should not have the out error rate 
names(rf_tree_err)[1:3] <- c("In_Error_Rate", "Numb Trees", "Out_Error_Rate")

# Look for best number of tree parameter for random forest 
for (tree_numb in 1:numb_tree){
  # Number of trees to use in algorithm 
  numb_trees_rf <- 200*tree_numb
  # Fit random forest 
  fit_temp <- randomForest(x_train, y_train_cat, xtest = x_test, ytest = y_test_cat, ntree = numb_trees_rf)
  # Temporary confusion matrix for in sample 
  rf_conf_train <- fit_temp$confusion
  # # In sample accuracy - sum the 1st and 4th element of matrix (6 elements total) 
  # Divide by the total number of element (sum 1st through 4th elements)
  train_accuracy_temp <- sum(rf_conf_train[c(1, 4)]) / sum(rf_conf_train[1:4])
  # Look at the out sample error rate (bad ML form)
  rf_conf_test <- fit_temp$test$confusion
  # Out of sample accuracy 
  test_accuracy_temp <- sum(rf_conf_test[c(1, 4)]) / sum(rf_conf_test[1:4])
  # Store in data frame 
  rf_tree_err[tree_numb, 1:3] <- c(train_accuracy_temp, numb_trees_rf, test_accuracy_temp) 
  
}

# Look at error rate vs. Number of tree 
plot(rf_tree_err$`Numb Trees`, rf_tree_err$In_Error_Rate)
rf_err_fit <- lm( In_Error_Rate~`Numb Trees`, data = rf_tree_err)
abline(rf_err_fit, col = "blue", lwd= 4)
abline(h = 1-high_low_ratio_train, col = "red", lwd = 4) # at 0.65
legend("topright",  c("Regression error fit", "Error Rate Training set"), col = c("blue", "red"), lwd = 4)

# Find min error rate
min_err_rate <- min(rf_tree_err$In_Error_Rate)
best_rf_row <- rf_tree_err[which(rf_tree_err$In_Error_Rate == min_err_rate), ]

# Bad form 
min_out_err <- min(rf_tree_err$Out_Error_Rate)
# rf_tree_err[which(rf_tree_err$Out_Error_Rate == min_out_err), ]

fit_cat <- randomForest(x_train, as.factor(y_train_cat), xtest = x_test, ytest = as.factor(y_test_cat), keep.forest = T, ntree = 1400)

saveRDS(fit_cat, file = RandomForestCatModelFile)
# saveRDS(best_row$Lambda, file = LinearRegrssionModelLambdaFile)
