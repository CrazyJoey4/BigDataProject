library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyr)

#import csv files

#Nutrition
tesco_msoa<-read.csv("./Area-level grocery purchases/year_msoa_grocery.csv")
tesco_ward<-read.csv("./Area-level grocery purchases/year_osward_grocery.csv")
tesco_oslaua<-read.csv("./Area-level grocery purchases/year_borough_grocery.csv")
#Health Data
child_obesity_ward<-read.csv("./Validation data (obesity, diabetes)/child_obesity_london_ward_2013-2014.csv")
child_obesity_oslaua<-read.csv("./Validation data (obesity, diabetes)/child_obesity_london_oslaua_2015-2016.csv")
adult_obesity_oslaua<-read.csv("./Validation data (obesity, diabetes)/london_obesity_oslaua_2012.csv")
adult_obesity_hospital_oslaua<-read.csv("./Validation data (obesity, diabetes)/obesity_hospitalization_oslaua_2016.csv")
diabetes_ward<-read.csv("./Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")


