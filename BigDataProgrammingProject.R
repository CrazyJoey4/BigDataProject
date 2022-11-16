install.packages("microbenchmark")
install.packages("factoextra")
install.packages("useful")
install.packages("manipulate")
install.packages("ggplot2")
install.packages("corrgram")
install.packages("psych")

library(dplyr)
library(ggplot)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(data.table)
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
library(corrplot)
library(GGally)
library(corrgram)
library(psych)


#change directory
setwd("~/BigData")

#Import Data
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

system.time(Seq_TT <- Seqfunction())


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

Par_TT <- system.time(parLapply(cl, 1:100, YearFile))

Par <- microbenchmark("Parallel " = {parLapply(cl, 1:100, YearFile)})



#Import Dataset
#Grocery
year_osward_grocery<-read.csv("~/BigData/Area-level grocery purchases/year_osward_grocery.csv")
#Health Data
diabetes_estimates_osward_2016<-read.csv("~/BigData/Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")

#Cleanse (Select required variables)
year_osward_grocery
diabetes_estimates_osward_2016

view(year_osward_grocery)
view(diabetes_estimates_osward_2016)

Diabetes_Food <- merge(diabetes_estimates_osward_2016, year_osward_grocery, 
                       by.x = "area_id")

Diabetes_Food <- Diabetes_Food %>% select(area_id, estimated_diabetes_prevalence, 
                                        carb, sugar, fat, saturate, protein, fibre)


view(Diabetes_Food)
head(Diabetes_Food)

#Calculate Correlation
corNutrient <-c( 
  carbohydrates <- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$carb),
  Sugar <- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$sugar), 
  Fat <- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$fat),
  SaturateFat<- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$saturate),
  Protein <- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$protein),
  Fibre <- cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$fibre)
            )

max(corNutrient)  #Carbs has the highest Correlation

#Plotting Correlation
corrplot(corr = cor(Diabetes_Food[2:8]),
         title = "Correlation Between Nutrients and Diabetes Prevalence",
         addCoef.col = "white",
         number.cex = 0.8,
         number.digits = 1,
         diag = FALSE,
         bg = "black",
         outline = "grey",
         method = "pie",
         type = "upper",
         tl.pos = "td",
         order = "original",
         mar=c(0,0,2,0)
         )

ggpairs(Diabetes_Food[2:8])

pairs.panels(Diabetes_Food[2:8], main = "Pairs Panels")

#Calculate Regression
model <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$carb)
print(model)

plot(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$carb, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model",
     
     )


abline(lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$carb + ))
