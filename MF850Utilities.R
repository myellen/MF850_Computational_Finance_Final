laggedDataFile <- "laggeddata.rds"
model_id <- "DNNmodel"
model_id_cat <- "DNNmodel_cat"

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

mf850_finalproject_data <- function() {
  df <- read.csv("mf850-finalproject-data.csv")
  return(df)
}

loadLaggedDataSet <- function() {
  mynewdataframe <- readRDS(file = laggedDataFile)
  return(mynewdataframe)
}

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

lagStockData <- function(stockdata) {
  companies <- split(stockdata, factor(stockdata$compid))
  newdataset <- data.frame()
  i <- 0
  for(company in companies)
  {
    returns <- company$RETMONTH
    futurereturns <- c(returns[2:length(returns)],NA)
    if (1 == NROW(returns)) {futurereturns <- c(NA)}
    
    company$futurereturns <- futurereturns
    newdataset = rbind(newdataset, company)
    i <- i + 1
    if(i %% 500 == 0) print(i)
  }
  saveRDS(newdataset, file = laggedDataFile)
  return(newdataset)
}

removeLastMonth <- function(stockdata) {
  companies <- split(stockdata, factor(stockdata$compid))
  newdataset <- data.frame()
  i <- 0
  for(company in companies)
  {
    company = company[-1,]
    
    newdataset = rbind(newdataset, company)
    i <- i + 1
    if(i %% 500 == 0) print(i)
  }
  return(newdataset)
}

