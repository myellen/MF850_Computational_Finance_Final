removeLastMonth <- function(stockdata) {
  companies <- split(stockdata, factor(stockdata$compid))
  #  companylist <- vector("list", NROW(companies))
  newdataset <- data.frame()
  i <- 0
  for(company in companies)
  {
    company = company[-1,]
    
    newdataset = rbind(newdataset, company)
    i <- i + 1
    #    companylist[[i]] <- company
    if(i %% 500 == 0) print(i)
  }
  return(newdataset)
}

regressionStockPredictions <- function(df) {
  
  # remove columns we don't use
  df$Date <- NULL
  df$compid <- NULL
  df$RETMONTH_SPX <- NULL
  
  # Scale data - remove categorical variable and add it back 
  Industry <- df$Industry
  df <- within(df, rm("Industry"))
  df <- scale(df)
  df <- as.data.frame(df)
  df$Industry <- as.factor(Industry)
  
  ###################
  ### GLMNET 
  #################
  
  # Import glmnet model and parameter from file 
  glm_best <- readRDS(LinearRegrssionModelFile)
  bestLambda <- readRDS(LinearRegrssionModelLambdaFile)
  
  # Reformat data for glmnet 
  x_data_lin <- model.matrix(~., data = df)
  # Predict using optimal Linear Regression Coefficients 
  predict_glm <- predict.glmnet(glm_best, newx = x_data_lin, type = "response", s = bestLambda)
  
  # Calculate Glm Model MSE 
  (MSE_glm <- mean((predict_glm - y_train)^2))
  print(MSE_glm)
  ##################
  ## Random Forest 
  ##################
  
  # Import random forest model from file - has 1400 trees 
  rf_reg_model <- readRDS(RandomForestRegressionModelFile)
  
  # Reformat data for random forest
  x_data_rf <- within(df, rm("Industry"))
  
  # Predict using Random Forest 
  predict_rf <- predict(rf_reg_model, newdata = x_data_rf)
  
  # Calculate Random Forest MSE 
  (MSE_rf <- mean((predict_rf - y_train)^2))
  print(MSE_rf)
  ##################
  ## Deep Learning
  ##################
  
  # This requires having Java installed 
  # Download it here 
  # http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
  
  # Look for JDK version 
  # Accept the conditions via radio button
  # Look for the version of your computer and click the download link , ~200 Mb for windows x64 
  h2o.init(nthreads = -1)
  
  x_data_h2o.hex <- as.h2o(x_data_lin)
  dnnModel <- h2o.loadModel("DNNmodel/DNNmodel")
  predict.reg <- as.data.frame(h2o.predict(dnnModel, x_data_h2o.hex))
  
  (MSE_dnn <- mean((predict.reg$predict - y_train)^2))
  print(MSE_dnn)
  
  NROW(predict.reg)
  NROW(y_test)
  
  h2o.shutdown(prompt = FALSE)
  weights <- cbind(1,1,1)
  predictions <- (weights[1]*predict.reg$predict + weights[2]*predict_rf + weights[3]*predict_glm) / sum(weights)
  return(predictions)
}

# deliverables go here
source("MF850Utilities.R") # use this for now
requiredpackages <- "h2o, glmnet, randomForest"
loadpackages(requiredpackages)

# assume csv like mf850-finalproject-data.csv with RETMONTH removed
df <- read.csv("TestDF_no_returns.csv")
y_test <- read.csv("TestDF_returns.csv")
y_train <- y_test

stockPredictions <- regressionStockPredictions(df)
print(stockPredictions)
