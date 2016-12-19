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

# deliverables go here
source("MF850Utilities.R") # use this for now

# assume csv like mf850-finalproject-data.csv with RETMONTH removed
df <- read.csv("mf850-finalproject-data-NORETMONTH.csv")
df <- removeLastMonth(df)
df <- na.omit(df)

# Scale data - remove categorical variable and add it back 
Industry <- df$Industry
df <- within(df, rm("Industry"))
df <- scale(df)
df$Industry <- Industry

# remove columns we don't use
df$Date <- NULL
df$compid <- NULL
df$RETMONTH_SPX <- NULL


originalLag <- loadLaggedDataSet()
y_test <- originalLag$futurereturns

###################
### GLMNET - LOGISITIC 
#################

# Import glmnet model and parameter from file 
glm_best <- readRDS(LogisticRegressionModelFile)
bestLambda <- readRDS(LogisticRegressionModelLambdaFile)

# Reformat data for glmnet 
x_data_lin <- model.matrix(~., data = df)
# Predict using optimal Linear Regression Coefficients 
predict_glm <- predict.glmnet(glm_best, newx = x_data, type = "response", s = bestLambda)

# Calculate Glm Model MSE 
(MSE_glm <- mean((predict_glm - y_train)^2))


##################
## Random Forest 
##################

# Import random forest model from file - has 1400 trees 
rf_cat_model <- readRDS(RandomForestCatFile)

# Reformat data for random forest
x_data_rf <- within(df, rm("Industry"))

# Predict using Random Forest 
predict_rf <- predict(rf_cat_mode, newdata = x_data_rf)

# Calculate Random Forest MSE 
(MSE_rf <- mean((predict_rf - y_train)^2))

