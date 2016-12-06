## Load the data
mydata<-read.csv(file="/Users/leighm888/Desktop/mf850-finalproject-data.csv", header=TRUE, sep=",")

#turns categorical variables in factors
require(rpart)	
mydata$Industry=as.factor(mydata$Industry)	
contrasts(mydata$Industry) = contr.treatment(213)
