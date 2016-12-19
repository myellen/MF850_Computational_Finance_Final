
source("MF850Utilities.R")
source("test_train_split_by_date.R")
############################################
## Start Random Forest Stuff
############################################
#install.packages("randomForest")
library(randomForest)

# Data prep for random forest - cannot handle large categorical variables 
x_test$Industry <- NULL
x_train$Industry <- NULL

# Number of number of trees 
numb_tree <- 10
# Pre allocate space for random forest 
rf_tree_mse <- data.frame(matrix(0, ncol = 3, nrow = numb_tree))
# Rename columns -- really should not have the out error rate 
names(rf_tree_mse)[1:3] <- c("MSE_in_sample", "Numb Trees", "MSE_out_sample") 

# Look for best number of tree parameter for random forest 
for (tree_numb in 1:numb_tree){
  # Number of trees to use in algorithm 
  numb_trees_rf <- 200*tree_numb
  # Fit random forest 
  fit_temp <- randomForest(x_train, y_train, xtest = x_test, ytest = y_test, ntree = numb_trees_rf)
  
  # Calculate in sample MSE
  MSE_in_sample <- mean((fit_temp$predicted - y_train) ^ 2)
  # Calculate out sample MSE 
  MSE_out_sample <- mean((fit_temp$test$predicted - y_test) ^ 2)
  
  # Store in data frame 
  rf_tree_mse[tree_numb, 1:3] <- c(MSE_in_sample, numb_trees_rf, MSE_out_sample)
  
}

# Look at error rate vs. Number of tree 
plot(rf_tree_mse$`Numb Trees`, rf_tree_mse$MSE_in_sample)
rf_err_fit <- lm( MSE_in_sample~`Numb Trees`, data = rf_tree_mse)
abline(rf_err_fit, col = "blue", lwd= 4)
abline(h = MSE_train, col = "red", lwd = 4)
legend("topright",  c("Regression error fit", "MSE Training set"), col = c("blue", "red"), lwd = 4)

# Find min error rate
min_err_rate <- min(rf_tree_mse$MSE_in_sample)
best_rf_row <- rf_tree_mse[which(rf_tree_mse$MSE_in_sample == min_err_rate), ]
best_rf_row

# Bad form 
min_out_err <- min(rf_tree_mse$MSE_out_sample)
# rf_tree_mse[which(rf_tree_mse$MSE_out_sample == min_out_err), ]


# Use regression results for categorical analysis 
fit_temp <- randomForest(x_train, y_train, xtest = x_test, ytest = y_test, ntree = 700)
# Save predictions from best random forest 
predictions <- fit_temp$test$predicted
# Turn regression results into categorical - high/low predictions 
pred_high_low <- ifelse(predictions < 0, 0, 1)
# Compare with y train cat - categorical variables 
error_rate <- mean(pred_high_low != y_test_cat)
error_rate


# Fit the best tree keeping the forest for prediction 
fit_best_rf <- randomForest(x_train, y_train, xtest = x_test, ytest = y_test, ntree = 700, keep.forest = TRUE)
# Save RDS file 
saveRDS(fit_best_rf, RandomForestRegressionModelFile)
