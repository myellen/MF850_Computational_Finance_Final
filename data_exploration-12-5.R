
# Input the data
data <- read.csv("mf850-finalproject-data.csv")
summary(data)
# Seperate the response variable 
REMONTH <- data$RETMONTH_SPX
# Histogram for response 
hist(REMONTH, breaks =60)
# How many unique response variables 
length(unique(REMONTH))
# Count how many returns are higher and lower 
(re_up <- length(REMONTH[REMONTH<0]))
(re_down <- length(REMONTH[REMONTH>0])) 
# Up to down ratio 
re_up/re_down
# Stop up down as new column vector 
up_down <- ifelse(REMONTH >0, 1, 0)

# Put plots above and below 
par(mfrow = 2:1)
# Plot the last 100 points 
plot(REMONTH[(length(REMONTH)-109):length(REMONTH)], type = 'l')
# Plot the first 100 points 
plot(REMONTH[0:100], type = 'l')

# Ontop of each other 
par(mfrow = c(1,1))
plot(REMONTH[(length(REMONTH)-109):length(REMONTH)], type = 'l', col= 'red', lwd = 3)
lines(REMONTH[0:109], type = 'l', col= 'blue', lwd = 2)

length(unique(REMONTH[(length(REMONTH)-100):length(REMONTH)]))
acf(REMONTH)

# Look at first 50 points of high low response 
plot(up_down[1:50], type = 'l')

# Take out industry, date, retmonth variables 
industry <- data$Industry
date <- data$Date
data_no_ind <- data
data_no_ind$Industry <- NULL
data_no_ind$Date <- NULL
# Scale data 
data_no_ind <- scale(data_no_ind)

# PCA 
pca <- prcomp(data_no_ind, center = TRUE, scale = FALSE)
summary(pca)
plot(pca)
# Maybe take the first 32 variables- they explain 94% of the variance and the increase becomes less than 1% after that 
# Will loose a lot of interpretability 




