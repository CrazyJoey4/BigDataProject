library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(data.table)
library(tidyverse)
library(readr)
library(magrittr)
library(microbenchmark)
library(parallel)
library(lme4)
library(cluster)
library(factoextra)
library(useful)

gc()

install.packages("microbenchmark")
install.packages("factoextra")
install.packages("useful")

#change directory
setwd("~/BigData/BigDataProject")
#import csv files
#Grocery
year_osward_grocery<-read.csv("./Area-level grocery purchases/year_osward_grocery.csv")
#Health Data
diabetes_estimates_osward_2016<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")

#Import Grocery Purchases
setwd("~/BigData/BigDataProject/Area-level grocery purchases")
ListFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE)
GPData = do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))




#Microbenchmark (Sequential)
mbm <- microbenchmark("Time Taken by Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
mbm
autoplot(mbm)

#Microbenchmark (Parallel)

cl <- makeCluster(getOption("cl.cores", 3))
clusterEvalQ(cl, c(library(data.table)))
clusterExport(cl, c("bindToEnv", "ccc"), 
              envir=environment())
f <- function(x) {
  bindToEnv(objNames='ccc')
  return(x+x)  
}

mbm2 <- microbenchmark("Time Taken by Parallel Process" = {do.call(rbind, parLapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
mbm2
autoplot(mbm2)


#Cluster Analysis
GP_weight <- GPData[2:9]

clustering <- kmeans(x = GP_weight, centers = 3)
clustering

plot(clustering, data=GP_weight)

