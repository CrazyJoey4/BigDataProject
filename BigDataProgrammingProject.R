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


#Sequential Process
mbm <- microbenchmark("Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
mbm
autoplot(mbm)

Seqfunction <- function(x)
{
  do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))
}

Seq_TT <- system.time(Seqfunction())


#Parallel Process
Parfunction <- function(y)
{
  cl_area <- detectCores(ListFile)
  cl <- makeCluster(cl_area)        #takes in as an argument the number of cores
  clusterEvalQ(cl, 2 + 2)           #takes a cluster and any expression, and executes the expression on each process
  
  clusterEvalQ(cl, {do.call(rbind, lapply(list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE), function(x) read.csv(x, stringsAsFactors = FALSE)))})
}

Par_TT <- system.time(parLapply(cl, 1:100, Parfunction()))




