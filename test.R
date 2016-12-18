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

#update.packages(ask = FALSE)

requiredpackages <- "devtools"
loadpackages(requiredpackages)


lagStockData <- function(stockdata) {
  companies <- split(stockdata, factor(stockdata$compid))
#  companylist <- vector("list", NROW(companies))
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
#    companylist[[i]] <- company
    if(i %% 1000 == 0) print(i)
  }
  return(newdataset)
#  companymatrix <- rbind(companylist)
#  return(companymatrix)
#  newdataset <- as.data.frame(as.table(companymatrix))
}

stockdata <- read.csv("mf850-finalproject-data.csv")

laggedDataSet <- lagStockData(stockdata)
