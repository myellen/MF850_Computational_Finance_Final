install.packages("boot")

# Input the data
data <- read.csv("mf850-finalproject-data.csv")
summary(data)
# Seperate the response variable 
RETMONTH <- data$RETMONTH
# Histogram for response 
hist(RETMONTH, breaks =60)
# Count how many returns are higher and lower 
(re_up <- length(RETMONTH[RETMONTH<0]))
(re_down <- length(RETMONTH[RETMONTH>0])) 
# Percentage of increases - Baseline   
re_up/(re_up+re_down)
# Stop up down as new column vector 
up_down <- ifelse(RETMONTH >0, 1, 0)

# Determine how many unique predictors there are  
# Function to measure number of unique elements in column 
numb_unique <- function(x) {
  uniques <- length(unique(x))
  return (uniques)
}

# Apply number of unique function to all columns of the data set 
uniqueness <- apply(data,2,numb_unique)
# Count how many columns have less than 50 unique values (indicating they are categorical)
length(uniqueness[uniqueness<50])
# Determine which variables should be categorical variables 
which(uniqueness <50)
# Already saw Industry has levels 

# Establish plotting points - Last 1000 data points 
n <- length(RETMONTH)
n_low <- n-1000 

# Plot last __ data points 
plot(RETMONTH[n_low:n], type = 'l', col= 'red', lwd = 2)
abline(h= mean(RETMONTH[n_low:n]), col = "blue", lwd = 2)
# A priori MSE - squared error from the mean 
MSE_i <- (RETMONTH[n_low:n]-mean(RETMONTH[n_low:n]))^2
# MSE
(MSE <- mean(MSE_i))

# Take out industry, date, retmonth variables 
industry <- data$Industry
date <- data$Date
data_no_ind <- data
data_no_ind$Industry <- NULL
data_no_ind$Date <- NULL
data_no_ind$RETMONTH <- NULL
# Scale data 
data_no_ind <- scale(data_no_ind)

# PCA 
pca <- prcomp(data_no_ind, center = TRUE, scale = FALSE)
summary(pca)
plot(pca)
# Maybe take the first 31 variables- they explain 94% of the variance and the increase becomes less than 1% after that 
# Will loose a lot of interpretability 

# Save first 9 principal components, last __ data points  ~ 58% variance, decrease <2% 
xs <- pca$x[n:n_low,1:9]
xs_df <- cbind(RETMONTH[n_low:n], as.data.frame(xs))
# pairs(xs_df)
# not much there 

# Get the data from the new data frame for the specified length 
data2 <- data_no_ind[n_low:n,]
# Combine categorical output with predictors 
data3 <- cbind(up_down[n_low:n], data2)
# Change data type to data frame -- preparations for logistic regression 
data3 <- data.frame(data3)
# Rename repsonse column in data frame  
names(data3)[1] <- "updown"
# Fit logistic regression 
fit <- glm(updown~., data= data3, family = "binomial")
# Find accuracy of logistic regression 
library(boot) #cv.glm
# In sample accuracy 
cv_est <- cv.glm(data3, fit, K= 10)$delta[1]
# In sample accuracy for logistic regression 
1-cv_est

