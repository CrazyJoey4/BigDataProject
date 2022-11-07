install.packages("microbenchmark")
install.packages("factoextra")
install.packages("useful")
install.packages("manipulate")

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
library(ff)
library(manipulate)

gc()


#change directory
setwd("~/BigData")
#import csv files
#Grocery
year_osward_grocery<-read.csv("./Area-level grocery purchases/year_osward_grocery.csv")
#Health Data
diabetes_estimates_osward_2016<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")

#Import Grocery Purchases
setwd("~/BigData/Area-level grocery purchases")
ListFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE)
GPData = do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))


#Microbenchmark (Sequential)
mbm <- microbenchmark("Time Taken by Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
mbm
autoplot(mbm)

#Microbenchmark (Parallel)


mbm2 <- microbenchmark("Time Taken by Parallel Process" = {do.call(rbind, parLapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
mbm2
autoplot(mbm2)


#Cluster Analysis
GP_data <- (GPData[, -which(names(GPData) == "area_id")])
GP_cleansed <- scale(GP_data)

clustering <- kmeans(x = GP_data, centers = 4, nstart = 25)
clustering

plot(clustering)
fviz_cluster(clustering, data = GP_data)

cl <- makeCluster(GPData[, -which(names(GPData) == "area_id")])

mbm2 <- microbenchmark("Time Taken by Parallel Process" = {parLapply(clustering)})

