
# Input the data
data <- read.csv("mf850-finalproject-data.csv")
summary(data)
# Seperate the response variable 
REMONTH <- data$RETMONTH_SPX
# Look at quantiles for response
summary(REMONTH)
# Histogram for response 
hist(REMONTH, breaks =60)
# Count how many returns are higher and lower 
(re_up <- length(REMONTH[REMONTH<0]))
(re_down <- length(REMONTH[REMONTH>0])) 
# How many unique response varible 
length(unique(REMONTH))

up_down <- ifelse(REMONTH >0, 1, 0)



# Attempt blind logistic regression 
data1 <- data
data1$Date <- NULL
data1$Industry <-NULL
x <- data.frame(model.matrix(RETMONTH_SPX~., data= data1))
data2 <-data.frame(data1)
data2$RETMONTH_SPX <- NULL

data2 <- c(up_down,data2)
fit2 <- glm(up_down ~., data = data2, family = "binomial")



y <- as.factor(data1$RETMONTH_SPX)
library(nnet)  # multinom 
# Multinomial regression 
fit1 <- multinom(RETMONTH_SPX ~. , data= data1)

