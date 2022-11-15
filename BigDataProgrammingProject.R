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


#change directory
setwd("~/BigData")
#import csv files
#Grocery
year_osward_grocery<-read.csv("./Area-level grocery purchases/year_osward_grocery.csv")
#Health Data
diabetes_estimates_osward_2016<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")

#Import Grocery Purchases
setwd("~/BigData/Area-level grocery purchases/Yearly")
ListFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE)

GPData = do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))


#Sequential Process
Seq <- microbenchmark("Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
Seq
autoplot(Seq)

Seqfunction <- function(x)
{
  do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))
}

system.time(Seq_TT <- Seqfunction)


#Parallel Process
setwd("~/BigData/Area-level grocery purchases/Yearly")
YearFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE) %>%
  map_df(~read_csv(.))

YearFile

numCores <- detectCores()
cl <- parallel::makeCluster(numCores)       #takes in as an argument the number of cores
clusterEvalQ(cl, {
  library(ggplot2)
  library(stringr)
  })
  
Parfunction <- function(y)
{
  parLapply(cl, 1:100, YearFile)
  stopCluster(cl)
}

Par_TT <- system.time(parLapply(cl, 1:100, Parfunction()))

Par <- microbenchmark("Parallel " = {parLapply(cl, 1:100, AL_Grocery2)})


