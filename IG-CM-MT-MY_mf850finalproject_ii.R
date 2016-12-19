######### MF850 Final Project Question ii ###########
####
#### 12/20/2016
####
#### Izzat Ghuneim, Cassandra McKay, Malcolm Taylor, Max Yellen
####
####
# This requires having Java installed 
# Download it here 
# http://www.oracle.com/technetwork/java/javase/downloads/jdk7-downloads-1880260.html
# Look for JDK version 
# Accept the conditions via radio button
# Look for the version of your computer and click the download link , ~200 Mb for windows x64 

# Add variables for filenames of saved models
laggedDataFile <- "laggeddata.rds"
model_id <- "DNNmodel"

LinearRegrssionFile <- "Linear_Ridge_Lasso.R"
RandomForestRegressionFile <- "random_forest_reg.R"
LogisticRegressionFile <- "Logistic_Ridge_Lasso.R"
RandomForestCatFile <- "random_forest_cat.R"  # Random forest high/low
LogisticRegressionModelLambdaFile <- "Logistic_Ridge_Lasso_model_lambda.rds"

LinearRegrssionModelFile <- "Linear_Ridge_Lasso_model.rds"
LinearRegrssionModelLambdaFile <- "Linear_Ridge_Lasso_model_lambda.rds"
RandomForestRegressionModelFile <- "random_forest_model.rds"
LogisticRegressionModelFile <- "Logistic_Ridge_Lasso_model.rds"
RandomForestCatModelFile <- "random_forest_cat_model.rds"

par(ask=FALSE)

# Add Convenience Functions
# installs packages if not already installed
loadpackages <- function(packagestoinstall) {
  packagestoinstall_split <- strsplit(packagestoinstall,", ")[[1]]
  for (packagename in packagestoinstall_split) {
    if(!packagename %in% rownames(installed.packages())){
      install.packages(packagename, dependencies = TRUE)
    }
    else {
      print(paste(packagename,"already installed"))
    }
    library(packagename, character.only=TRUE)
  }
}

# functions that provides predictions
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
  
  ##################
  ## Random Forest 
  ##################
  
  # Import random forest model from file - has 1400 trees 
  rf_reg_model <- readRDS(RandomForestRegressionModelFile)
  
  # Reformat data for random forest
  x_data_rf <- within(df, rm("Industry"))
  
  # Predict using Random Forest 
  predict_rf <- predict(rf_reg_model, newdata = x_data_rf)
  
  ##################
  ## Deep Learning
  ##################
  
  h2o.init(nthreads = -1)
  
  x_data_h2o.hex <- as.h2o(x_data_lin)
  dnnModel <- h2o.loadModel("DNNmodel/DNNmodel")
  predict.reg <- as.data.frame(h2o.predict(dnnModel, x_data_h2o.hex))
  
  h2o.shutdown(prompt = FALSE)
  
  ########################
  ## Combine Predictions
  ########################
  
  weights <- cbind(0.28, 0.31, 0.41)
  predictions <- (weights[1]*predict.reg$predict + weights[2]*predict_rf + weights[3]*predict_glm) 
  return(predictions)
}

# load packages
requiredpackages <- "h2o, glmnet, randomForest"
loadpackages(requiredpackages)

# import stockdata
# assume csv like mf850-finalproject-data.csv with RETMONTH removed
df <- read.csv("TestDF_no_returns.csv")

stockPredictions <- regressionStockPredictions(df)
print(stockPredictions)
