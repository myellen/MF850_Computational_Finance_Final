

# Import the splits done previously 
source("test_train_split_by_date.R")

# Subset the Response variable into test and training sets
y_test <- test_set$RETMONTH
y_train <- train_set$RETMONTH

# SCALE DATA


# Create training set 
x_test <- data[test_low:test_high, ]
x_train <- data[train_low:train_high, ]


# Train set for categorical analysis 
y_train_cat <- ifelse(y_train > 0, 1, 0)
# Ratio of high vs low returns for TRAIN set 
(high_low_ratio <- sum(y_train_cat == 1)  / length(y_train_cat))

# Test set for categorical analysis 
y_test_cat <- ifelse(y_test > 0, 1, 0)
# Ratio of high vs low returns for TEST set 
(high_low_ratio <- sum(y_test_cat == 1) / length(y_test_cat))
# A priori number to beat 

###########################
#### LASSO ANALYSIS 
###########################

# Prepare data for glm 
data_train = data.frame(cbind(y_train_cat, x_train))
# Lasso for all variable lasso  
fit_all <- glm(y_train_cat~., data = data_train, family = "binomial")
# Find accuracy of logistic regression 
# install.packages("boot")
library(boot) #cv.glm
# In sample accuracy 
cv_err <- cv.glm(data_train, fit_all, K = 10)$delta[1]
# In sample accuracy for logistic regression 
1 - cv_err


# Try using lasso and ridge  
# install.packages("glmnet")
library(glmnet)
# Prepare data for glmnet package 
x = model.matrix(y_train_cat~., data = data_train)
# Specify lambdas we would like look through during ridge and lasso regressions 
lambdas <- 10^seq(-6, 4, length = 200)
# Lasso Regression 
cv.lasso = cv.glmnet(x, y_train_cat, family = "binomial", type.measure = "class", 
                     lambda = lambdas, alpha = 1, standardize = FALSE)
# Ridge regression- very similar to lasso- alpha parameter is = 0 
cv.ridge = cv.glmnet(x, y_train_cat, family = "binomial", type.measure = "class",
                     lambda = lambdas, alpha = 0, standardize = FALSE)

# Plot lasso cross validation error means vs. log lambda 
lasso_lambdas <- cv.lasso$lambda
lasso_cv_means <- cv.lasso$cvm
plot(lasso_lambdas, lasso_cv_means, main = "Lasso Lambda vs Cross Validation Error mean", log = "x")

# Plot ridge cross validation error means vs. log lambda
ridge_lambdas <- cv.ridge$lambda
ridge_cv_means <- cv.ridge$cvm
plot(ridge_lambdas, ridge_cv_means, main = "Ridge Lambda vs Cross Validation Error mean", log = "x")


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
  cv.temp = cv.glmnet(x, y_train_cat, family = "binomial", type.measure = "class", 
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

# Predict using best lasso ridge mix 
glm_best <- glmnet(x, y_train_cat, family = "binomial", alpha = best_row$Alpha, 
                   standardize = FALSE, lambda = lambdas)
# Get the coefficients of best alpha mix 
glm_best_coef <- predict(glm_best, type = "coefficients", 
                         s = best_row$Lambda)[1:(ncol(x)), ]

# Coefficients 
sort(glm_best_coef[abs(glm_best_coef) > 0.3])
# How many is that? 
length(glm_best_coef[abs(glm_best_coef) > 0.3])

# Now we will use this new model on the test set

# Combine x and y for test set 
x2 <- data.frame(cbind(y_test_cat, x_test))
# Prepare test set data frame 
x_train_edit <- model.matrix(y_test_cat ~., data = x2)
# Predict outcomes on test set 
predict_glm <- predict(glm_best, newx = x_train_edit, 
                       type = "response", s = best_row$Lambda)
# Convert logistic probabilities into yes / no 
predict_01 <- ifelse(predict_glm > 0.5, 1, 0)
# accuracy is where predictions != test set
(accuracy <-  mean(predict_01 == y_test_cat))