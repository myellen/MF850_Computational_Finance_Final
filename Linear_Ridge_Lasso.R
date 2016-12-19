

# Import the splits done previously 
source("test_train_split_by_date.R")
source("MF850Utilities.R")


###########################
#### LASSO ANALYSIS 
###########################

requiredpackages <- "glmnet"
loadpackages(requiredpackages)

# Prepare data for glm 
data_train = data.frame(cbind(y_train, x_train))
# Lasso for all variable lasso  
fit_all <- glm(y_train_cat~., data = data_train)
summary(fit_all)

# Find MSE 
(MSE_all_train <- mean((fit_all$fitted.values - y_train) ^ 2))
MSE_train
# Compare- percentage drop in accuracy 
((MSE_train - MSE_all_train)/MSE_train)*100
 
# Need out of sample measure 


# Try using lasso and ridge  

# Prepare data for glmnet package 
x = model.matrix(y_train ~., data = data_train)
# Specify lambdas we would like look through during ridge and lasso regressions 
lambdas <- 10^seq(-5, 3, length = 100)
# Lasso Regression 
cv.lasso = cv.glmnet(x, y_train, type.measure = "mse", 
                     lambda = lambdas, alpha = 1, standardize = FALSE)
# Ridge regression- very similar to lasso- alpha parameter is = 0 
cv.ridge = cv.glmnet(x, y_train, type.measure = "mse",
                     lambda = lambdas, alpha = 0, standardize = FALSE)

# Plot lasso cross validation error means vs. log lambda 
lasso_lambdas <- cv.lasso$lambda
lasso_cv_means <- cv.lasso$cvm
plot(lasso_lambdas, lasso_cv_means, main = "Lasso Lambda vs Cross Validation MSE", log = "x")

# Plot ridge cross validation error means vs. log lambda
ridge_lambdas <- cv.ridge$lambda
ridge_cv_means <- cv.ridge$cvm
plot(ridge_lambdas, ridge_cv_means, main = "Ridge Lambda vs Cross Validation MSE", log = "x")


# Experimental mix of lasso and ridge 
# Number of alphas to try 
numb_alphas <- 20
# Pre allocate space 
lambda_cv_min <- data.frame(matrix(0, nrow = numb_alphas, ncol = 3))
# Re name columns 
names(lambda_cv_min)[1:3] <- c("Lambda", "Min CV", "Alpha")

for (alpha_n in 1:numb_alphas){
  # New alpha
  alpha <- alpha_n * (1 / numb_alphas)
  # Run Ridge/Lasso Mix 
  cv.temp = cv.glmnet(x, y_train, type.measure = "mse", 
                      lambda = lambdas, alpha = alpha, standardize = FALSE)
  # Store min cvm 
  min_cv_temp <- min(cv.temp$cvm)
  # Store lambda for min cvm 
  lambda_temp <- cv.temp$lambda.min
  # Store in alpha, lambda, min cv in data frame 
  lambda_cv_min[alpha_n, c(1:3)] <- c(lambda_temp, min_cv_temp, alpha) 
}

# Find min cv and then find the row with that information 
min_cv_best <- min(lambda_cv_min$`Min CV`) 
(best_row <- lambda_cv_min[which(lambda_cv_min$`Min CV` == min_cv_best), ])
# Look at alpha vs. cross validation error mean 
plot(lambda_cv_min$Alpha, lambda_cv_min$`Min CV`)
# Look at regression 
CV_alpha_fit <- lm(`Min CV` ~ Alpha, data = lambda_cv_min)
abline(CV_alpha_fit, col = "blue", lwd = 3)

# See how much better the training set is 
((best_row$`Min CV`-MSE_train)/MSE_train)*100

# Predict using best lasso ridge mix 
glm_best <- glmnet(x, y_train, alpha = best_row$Alpha, 
                   standardize = FALSE, lambda = lambdas)
# Get the coefficients of best alpha mix 
glm_best_coef <- predict(glm_best, type = "coefficients", 
                         s = best_row$Lambda)[1:(ncol(x)), ]

# Coefficients 
sort(glm_best_coef[abs(glm_best_coef) > 0.1])
# How many is that? 
length(glm_best_coef[abs(glm_best_coef) > 0.1])

# Now we will use this new model on the test set

# Combine x and y for test set 
x2 <- data.frame(cbind(y_test, x_test))
# Prepare test set data frame 
x_train_edit <- model.matrix(y_test ~., data = x2)
# Predict outcomes on test set 
predict_glm <- predict(glm_best, newx = x_train_edit, 
                       type = "response", s = best_row$Lambda)

# Calculate test MSE 
(MSE_glm_test <- mean((predict_glm - y_test)^2))
MSE_train
# Compare with original - Percentage decrease  
((MSE_train - MSE_glm_test)/MSE_train)*100
# Decrease b

## Save Trained Model
# saveRDS(glm_best, file = LinearRegrssionModelFile)
# saveRDS(best_row$Lambda, file = LinearRegrssionModelLambdaFile)
