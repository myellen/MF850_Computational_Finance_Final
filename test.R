#install.packages("devtools")
if(!"devtools" %in% rownames(installed.packages())) { install.packages("devtools", dependencies = TRUE) }
install.packages("glmnet")
library(glmnet)
#library(devtools)
#devtools::use_travis()

f1 <- function(x, y) {
  x+y
}
f1(1,2)

data <- read.csv("mf850-finalproject-data.csv")
