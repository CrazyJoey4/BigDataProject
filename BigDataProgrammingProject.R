library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

#import csv files

#Nutrition
tesco_msoa<-read.csv("./Area-level grocery purchases/year_msoa_grocery.csv")
tesco_msoa<-read.csv("./Area-level grocery purchases/year_osward_grocery.csv")
tesco_msoa<-read.csv("./Area-level grocery purchases/year_borough_grocery.csv")
#Health Data
tesco_msoa<-read.csv("./Validation data (obesity, diabetes)/child_obesity_london_ward_2013-2014.csv")
tesco_msoa<-read.csv("./Validation data (obesity, diabetes)/child_obesity_london_oslaua_2015-2016.csv")
tesco_msoa<-read.csv("./Validation data (obesity, diabetes)/london_obesity_oslaua_2012.csv")
tesco_msoa<-read.csv("./Validation data (obesity, diabetes)/obesity_hospitalization_oslaua_2016.csv")
tesco_msoa<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")


