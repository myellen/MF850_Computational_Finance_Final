#https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/
#http://www.kdnuggets.com/2016/08/begineers-guide-neural-networks-r.html
#http://www.parallelr.com/r-deep-neural-network-from-scratch/
#http://gekkoquant.com/2012/05/26/neural-networks-with-r-simple-example/

#Install package 'neuralnet'

library("neuralnet")

#Going to create a neural network to perform sqare rooting
#Type ?neuralnet for more information on the neuralnet library
stockdata = read.csv("mf850-finalproject-data.csv")
stockdata$Date = NULL
stockdata$Industry = NULL
scaled_Stock = scale(stockdata)
y = stockdata$RETMONTH_SPX
x = stockdata
x$RETMONTH_SPX = NULL

traininginput <- x
trainingoutput <- y

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

#Train the neural network
#Going to have 100 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=100, threshold=0.01)
print(net.sqrt)

#Plot the neural network
plot(net.sqrt)

#Test the neural network on some training data
testdata <- as.data.frame((1:10)^2) #Generate some squared numbers
net.results <- compute(net.sqrt, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)