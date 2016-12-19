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
df <- read.csv("TestDF_no_returns.csv")
y_test <- read.csv("TestDF_returns.csv")
# Remove column with numbers 
df <- within(df, rm("X"))
y_train <- within(y_test, rm("X"))
# Switch y_train to be categorical
y_train <- as.factor(ifelse(y_train > 0, 1, 0))

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
### GLMNET - LOGISITIC 
#################

# Import glmnet model and parameter from file 
glm_best <- readRDS(LogisticRegressionModelFile)
bestLambda <- readRDS(LogisticRegressionModelLambdaFile)

# Reformat data for glmnet 
x_data_lin <- model.matrix(~., data = df)
# Predict using optimal Linear Regression Coefficients 
predict_glm <- predict.glmnet(glm_best, newx = x_data_lin, type = "response", s = bestLambda)
# Change log odds to categorical variables 
predict_glm <- ifelse(predict_glm > 0, 1, 0)
# Ensure data types are the same int -> factors 
predict_glm <- as.factor(predict_glm)
# Calculate Glm Model error rate 
(GLM_error_rate <- mean((predict_glm != y_train)))


##################
## Random Forest 
##################

# Import random forest model from file - has 1400 trees 
rf_cat_model <- readRDS(RandomForestCatFile)

# Reformat data for random forest
x_data_rf <- within(df, rm("Industry"))

# Predict using Random Forest 
predict_rf <- predict(rf_cat_model, newdata = x_data_rf)

# Calculate Random Forest Merror rate 
(RF_error_rate <- mean((predict_rf == y_train)))

