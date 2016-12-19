source("MF850Utilities.R")
source("test_train_split_by_date.R")
requiredpackages <- "h2o"
loadpackages(requiredpackages)

h2o.init(nthreads = -1)

explanatorycolumns <- colnames(x_test)

x_train$futurereturns <- y_train
x_test$futurereturns <- y_test

x_trainh2o.hex <- as.h2o(x_train)
x_testh2o.hex <- as.h2o(x_test)


h2o.deeplearning( x = explanatorycolumns, y = "futurereturns", training_frame =  x_trainh2o.hex,
                  validation_frame = x_testh2o.hex, activation =  "Tanh", hidden = rep(100, 100),
                  #momentum_start = 0.5, momentum_stable = 0.99, 
                  loss = "Automatic",
                  distribution = "AUTO", nfolds = 2,
                  model_id = model_id,
                  initial_weight_distribution = "Uniform")

m <- h2o.getModel(model_id)
summary(m)
h2o.performance(m)
h2o.saveModel(m,model_id)
plot(m)

## Categorical

y_train <- ifelse(y_train > 0, 1, 0)
y_test <- ifelse(y_test > 0, 1, 0)
# Ensure data types are the same int -> factors 
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

x_train$futurereturns <- y_train
x_test$futurereturns <- y_test

x_trainh2o.hex <- as.h2o(x_train)
x_testh2o.hex <- as.h2o(x_test)

h2o.deeplearning( x = explanatorycolumns, y = "futurereturns", training_frame =  x_trainh2o.hex,
                  validation_frame = x_testh2o.hex, activation =  "Tanh", hidden = rep(100, 100),
                  #momentum_start = 0.5, momentum_stable = 0.99, 
                  loss = "Automatic",
                  distribution = "AUTO", 
                  nfolds = 2,
                  model_id = model_id_cat
                  #, initial_weight_distribution = "Uniform"
                  )

m2 <- h2o.getModel(model_id_cat)
summary(m2)
h2o.performance(m2)
h2o.saveModel(m2,model_id_cat)
plot(m2)

h2o.shutdown(prompt = FALSE)

#https://github.com/h2oai/h2o-3/blob/master/h2o-docs/src/product/tutorials/dl/dlperf.Rmd
# https://www.analyticsvidhya.com/blog/2016/05/h2o-data-table-build-models-large-data-sets/

