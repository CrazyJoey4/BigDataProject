install.packages("microbenchmark")
install.packages("factoextra")
install.packages("useful")
install.packages("manipulate")
install.packages("ggplot2")
install.packages("corrgram")
install.packages("psych")
install.packages("partykit")
install.packages("ggplot")
install.packages("dplyr")
install.packages("tidyverse")

library(rlang)
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
library(partykit)
library(caTools)


#change directory
setwd("~/BigData")

#Import Data
setwd("~/BigData/Area-level grocery purchases/Yearly")
ListFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE)

GPData = do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))


#Sequential Process
Seq <- microbenchmark("Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
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

YearFile <- select_if(YearFile, is.numeric)



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

#Parallel does not work because of cluster




# To identify which food category has the highest effect in diabetes level

#Import Dataset
#Grocery
year_osward_grocery<-read.csv("~/BigData/Area-level grocery purchases/year_osward_grocery.csv")
#Health Data
diabetes_estimates_osward_2016<-read.csv("~/BigData/Validation data (obesity, diabetes)/diabetes_estimates_osward_2016.csv")

#Cleanse (Select required variables)
Diabetes_Food <- merge(diabetes_estimates_osward_2016, year_osward_grocery, 
                       by.x = "area_id")

Diabetes_Food <- Diabetes_Food %>% select(area_id, estimated_diabetes_prevalence, 
                                        carb, sugar, fat, saturate, protein, fibre,
                                        f_beer, f_dairy, f_eggs, f_fats_oils, f_fish, f_fruit_veg, f_grains,
                                        f_meat_red, f_poultry, f_readymade, f_sauces, f_soft_drinks,
                                        f_spirits, f_sweets, f_tea_coffee, f_water, f_wine
)

#To view
view(Diabetes_Food)

#Correlation Calculation
#Calculate Correlation between Nutrients and Diabetes Prevalence
Ncorrelation <-c( 
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$carb),
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$sugar), 
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$fat),
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$saturate),
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$protein),
  cor(Diabetes_Food$estimated_diabetes_prevalence, Diabetes_Food$fibre)
)

Nutrients <- c("Carbohydrates", "Sugar", "Fats", "Saturated Fat", "Protein", "Fibre")
corNutrient <- cbind(Ncorrelation, Nutrients)

#Top 3 in nutrient
head(corNutrient[order(Ncorrelation, decreasing = TRUE), ], n=3)
# Carbohydrates | Sugar | Saturated Fat


#Plotting Correlation
corrplot(corr = cor(Diabetes_Food[2:8]),
         title = "Correlation Between Nutrients and Diabetes Patient Count",
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


#Calculate Correlation between Carbohydrates and Food Category
Fcorrelation <- c(
  cor(Diabetes_Food$carb, Diabetes_Food$f_beer),
  cor(Diabetes_Food$carb, Diabetes_Food$f_dairy), 
  cor(Diabetes_Food$carb, Diabetes_Food$f_fats_oils),
  cor(Diabetes_Food$carb, Diabetes_Food$f_fish),
  cor(Diabetes_Food$carb, Diabetes_Food$f_fruit_veg),
  cor(Diabetes_Food$carb, Diabetes_Food$f_grains),
  cor(Diabetes_Food$carb, Diabetes_Food$f_meat_red),
  cor(Diabetes_Food$carb, Diabetes_Food$f_poultry), 
  cor(Diabetes_Food$carb, Diabetes_Food$f_readymade),
  cor(Diabetes_Food$carb, Diabetes_Food$f_sauces),
  cor(Diabetes_Food$carb, Diabetes_Food$f_soft_drinks),
  cor(Diabetes_Food$carb, Diabetes_Food$f_spirits),
  cor(Diabetes_Food$carb, Diabetes_Food$f_sweets),
  cor(Diabetes_Food$carb, Diabetes_Food$f_tea_coffee),
  cor(Diabetes_Food$carb, Diabetes_Food$f_water),
  cor(Diabetes_Food$carb, Diabetes_Food$f_wine)
)

Category <- c("Beer", "Dairy", "Fats Oils", "Fish", "Fruit and Vegetables", "Grains", 
         "Meat in Red", "Poultry", "Readymade", "Sauces", "Soft Drinks", "Spirits", "Sweets", "Tea and Coffee", "Water", "Wine")

corFood <- cbind(Fcorrelation, Category)


#Top 3 in Food Category that related to Carbohydrates
head(corFood[order(Fcorrelation, decreasing = TRUE), ], n=3)
# Grains | Sweets | Soft Drinks


food_data <- Diabetes_Food %>% select(f_beer, f_dairy, f_eggs, f_fats_oils, f_fish, f_fruit_veg, f_grains,
                                          f_meat_red, f_poultry, f_readymade, f_sauces, f_soft_drinks,
                                          f_spirits, f_sweets, f_tea_coffee, f_water, f_wine)



       

# Histogram of products purchased
ggplot(food_data, mapping = aes(x = f_grains)) + geom_histogram(na.rm = TRUE, bins = 50, colour='black', fill='grey') + ggtitle("Grains weights in average products")
ggplot(food_data, mapping = aes(x = f_soft_drinks)) + geom_histogram(na.rm = TRUE, bins = 50, colour='black', fill='grey') + ggtitle("Soft Drinks weights in average products")
ggplot(food_data, mapping = aes(x = f_sweets)) + geom_histogram(na.rm = TRUE, bins = 50, colour='black', fill='grey') + ggtitle("Sweet weights in average products")


# Plots of diabetes prevalence by nutrients
ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, carb)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Carbohydrates")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, sugar)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Carbohydrates Sugar")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, fat)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Fat")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, saturate)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Saturated Fat")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, protein)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Protein")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, fibre)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Fibre")


# Scatter plots of Carbohydrates in Food Categories
ggplot(Diabetes_Food, aes(carb, f_grains)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Beer Nutrient by Carbohydrates")  

ggplot(Diabetes_Food, aes(carb, f_sweets)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Beer Nutrient by Carbohydrates")  

ggplot(Diabetes_Food, aes(carb, f_soft_drinks)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Beer Nutrient by Carbohydrates") 


#Linear Regression of Nutrients
#Carbohydrates
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$carb, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
     )
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$carb))

#Sugar
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$sugar, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$sugar))

#Fat
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$fat, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$fat))

#Saturated Fat
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$saturate, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$saturate))

#Protein
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$protein, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$protein))

#Fibre
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$fibre, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$fibre))


#Pre-code
is.na(Diabetes_Food)
sample_data = sample.split(Diabetes_Food, SplitRatio = 0.8)
train_data <- subset(Diabetes_Food, sample_data == TRUE)
test_data <- subset(Diabetes_Food, sample_data == FALSE)


#Generate Linear Models
model1 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$carb)
model2 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$sugar)
model3 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$fat)
model4 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$saturate)
model5 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$protein)
model6 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$fibre)

M_carb <- ctree(estimated_diabetes_prevalence ~ carb, train_data)
M_sugar <- ctree(estimated_diabetes_prevalence ~ sugar, train_data)
M_fat <- ctree(estimated_diabetes_prevalence ~ fat, train_data)
M_saturate <- ctree(estimated_diabetes_prevalence ~ saturate, train_data)
M_protein <- ctree(estimated_diabetes_prevalence ~ protein, train_data)
M_fibre <- ctree(estimated_diabetes_prevalence ~ fibre, train_data)

plot(model1)
plot(model2)
plot(model3)
plot(model4)
plot(model5)
plot(model6)

print(model)
print(summary(model))


