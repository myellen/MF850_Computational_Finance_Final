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

# remove columns we don't use
df$Date <- NULL
df$compid <- NULL
df$RETMONTH_SPX <- NULL

x_data <- model.matrix(~., data = df)

glm_best <- readRDS(LinearRegrssionModelFile)
bestLambda <- readRDS(LinearRegrssionModelLambdaFile)

predict_glm <- predict(glm_best, newx = x_data, 
                       type = "response", s = bestLambda)

predict_glm

originalData <- loadLaggedDataSet()
#originalLag <- lagStockData(originalData)
#originalLag <- na.omit(originalLag)
y_test <- originalLag$futurereturns
# Calculate test MSE 
(MSE_glm_test <- mean((predict_glm - y_test)^2))
MSE_train
# Compare with original - Percentage decrease  
((MSE_train - MSE_glm_test)/MSE_train)*100


