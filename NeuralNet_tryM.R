source("MF850Utilities.R")
source("test_train_split_by_date.R")
requiredpackages <- "h2o"
loadpackages(requiredpackages)

h2o.init(nthreads = -1)

explanatorycolumns <- colnames(x_test)

x_train$futurereturns <- y_train
x_test$futurereturns <- y_test

x_trainh2o.hex <- as.h2o(x_train)
x_testh20.hex <- as.h2o(x_test)

h2o.deeplearning( x = explanatorycolumns, y = "futurereturns", training_frame =  x_trainh2o.hex,
                  validation_frame = x_testh20.hex, activation =  "Rectifier", hidden = rep(100, 100),
                  #momentum_start = 0.5, momentum_stable = 0.99, 
                  loss = "Automatic",
                  distribution = "AUTO", nfolds = 4)

colnames(x_test)

h2o.shutdown()
y
