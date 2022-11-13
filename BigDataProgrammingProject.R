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

stopCluster(cl)

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

system.time(Seq_TT <- Seqfunction)


#Parallel Process
Parfunction <- function(y)
{
  YearFile <- 
    list.files(path="~/BigData/Area-level grocery purchases/Yearly", pattern="*.csv", all.files=TRUE, full.names=FALSE)
  
  cl_area <- detectCores(YearFile)
  cl <- makeCluster(cl_area)        #takes in as an argument the number of cores
  clusterEvalQ(cl, 
               {library(ggplot2)
                library(stringr)
                 })       #takes a cluster and any expression, and executes the expression on each process
  
}

system.time(Par_TT <- parLapply(1:100, Parfunction))
  
Par_TT
  
  
  clusterEvalQ(cl, {list.files(path="~/BigData/Area-level grocery purchases/Yearly", pattern="*.csv", all.files=TRUE, full.names=FALSE), function(x) read.csv(x, stringsAsFactors = FALSE)})
  
  parLapply(cl, 1:100, list.files(path="~/BigData/Area-level grocery purchases/Yearly", pattern="*.csv", all.files=TRUE, full.names=FALSE), function(x) read.csv(x, stringsAsFactors = FALSE))
}

Par_TT <- system.time(parLapply(cl, 1:100, Parfunction()))


