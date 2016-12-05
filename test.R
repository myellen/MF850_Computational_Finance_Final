#install.packages("devtools")
#library(devtools)
#devtools::use_travis()

loadpackages <- function(packagestoinstall) {
  packagestoinstall_split <- strsplit(packagestoinstall,", ")[[1]]
  for (packagename in packagestoinstall_split) {
    if(!"devtools" %in% rownames(installed.packages())){
      install.packages("devtools", dependencies = TRUE)
    }
    else {
      print(paste(packagename,"already installed"))
    }
    library(packagename, character.only=TRUE)
  }
}

requiredpackages <- "devtools, glmnet"
loadpackages(requiredpackages)


f1 <- function(x, y) {
  x+y
}
f1(1,2)

data <- read.csv("mf850-finalproject-data.csv")
