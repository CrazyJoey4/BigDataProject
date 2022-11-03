library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(data.table)
library(tidyverse)
library(readr)
library(magrittr)

#change directory
setwd("~/BigData/BigDataProject")

#import csv files

#Grocery
year_osward_grocery<-read.csv("./BigDataProject/Area-level grocery purchases/year_osward_grocery.csv")

#Health Data
diabetes_estimates_osward_2016<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")


# Using read.csv()
list_csv_files <- list.files(path = "./Validation data (obesity, diabetes)/")
df2 = do.call(rbind, lapply(list_csv_files, function(x) read.csv(x, stringsAsFactors = FALSE)))
df2

df <-
  list.files(path = "Validation data (obesity, diabetes)/", pattern = "*.csv") %>% 
  map_df(~read_csv(.))
df



