## Load the data
mydata<-read.csv(file="mf850-finalproject-data.csv", header=TRUE, sep=",")

#turns categorical variables in factors
require(rpart)	
mydata$Industry=as.factor(mydata$Industry)	
contrasts(mydata$Industry) = contr.treatment(213)
