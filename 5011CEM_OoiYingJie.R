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


#Import Data
setwd("~/BigData/Area-level grocery purchases/Yearly")
ListFile <- 
  list.files(path=".", pattern="*.csv", all.files=TRUE, full.names=FALSE)


#Sequential Process 
Seq <- microbenchmark("Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
autoplot(Seq)

Seqfunction <- function(x)
{
  do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))
}

system.time(Seq_TT <- Seqfunction())

#Parallel Process (mclapply)
Par <- microbenchmark("Parallel Process" = {do.call(rbind, mclapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))})
autoplot(Par)

Parfunction <- function(y)
{
  do.call(rbind, mclapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))
}

system.time(Par_TT <- Parfunction())


compare <- microbenchmark("Sequential Process" = {do.call(rbind, lapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))},
               "Parallel Process" = {do.call(rbind, mclapply(ListFile, function(x) read.csv(x, stringsAsFactors = FALSE)))}
               )
autoplot(compare)
##### 
#Parallel Process (parLapply)
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

#####



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
         title = "Correlation Between Nutrients and Estimate Diabetes Prevalence",
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
         "Meat in Red", "Poultry", "Readymade", "Sauces", "Soft Drinks", "Spirits", "Sweets", 
         "Tea and Coffee", "Water", "Wine")

corFood <- cbind(Fcorrelation, Category)


#Top 3 in Food Category that related to Carbohydrates
head(corFood[order(Fcorrelation, decreasing = TRUE), ], n=3)
# Grains | Sweets | Soft Drinks


#Calculate Correlation between Sugar and Food Category
Fcorrelation2 <- c(
  cor(Diabetes_Food$sugar, Diabetes_Food$f_beer),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_dairy), 
  cor(Diabetes_Food$sugar, Diabetes_Food$f_fats_oils),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_fish),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_fruit_veg),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_grains),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_meat_red),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_poultry), 
  cor(Diabetes_Food$sugar, Diabetes_Food$f_readymade),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_sauces),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_soft_drinks),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_spirits),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_sweets),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_tea_coffee),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_water),
  cor(Diabetes_Food$sugar, Diabetes_Food$f_wine)
)

Category <- c("Beer", "Dairy", "Fats Oils", "Fish", "Fruit and Vegetables", "Grains", 
              "Meat in Red", "Poultry", "Readymade", "Sauces", "Soft Drinks", "Spirits", "Sweets", 
              "Tea and Coffee", "Water", "Wine")
corFood2 <- cbind(Fcorrelation2, Category)


#Top 3 in Food Category that related to Sugar
head(corFood2[order(Fcorrelation2, decreasing = TRUE), ], n=3)
# Sweets | Grains | Readymade


#Calculate Correlation between Saturated Fat and Food Category
Fcorrelation3 <- c(
  cor(Diabetes_Food$saturate, Diabetes_Food$f_beer),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_dairy), 
  cor(Diabetes_Food$saturate, Diabetes_Food$f_fats_oils),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_fish),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_fruit_veg),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_grains),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_meat_red),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_poultry), 
  cor(Diabetes_Food$saturate, Diabetes_Food$f_readymade),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_sauces),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_soft_drinks),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_spirits),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_sweets),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_tea_coffee),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_water),
  cor(Diabetes_Food$saturate, Diabetes_Food$f_wine)
)

Category <- c("Beer", "Dairy", "Fats Oils", "Fish", "Fruit and Vegetables", "Grains", 
              "Meat in Red", "Poultry", "Readymade", "Sauces", "Soft Drinks", "Spirits", "Sweets", 
              "Tea and Coffee", "Water", "Wine")
corFood3 <- cbind(Fcorrelation3, Category)


#Top 3 in Food Category that related to Saturated Fat
head(corFood3[order(Fcorrelation3, decreasing = TRUE), ], n=3)
# Sweets | Grains | Soft Drinks




#Food Purchased by consumer
food_data <- Diabetes_Food %>% select(f_beer, f_dairy, f_eggs, f_fats_oils, f_fish, f_fruit_veg, f_grains,
                                          f_meat_red, f_poultry, f_readymade, f_sauces, f_soft_drinks,
                                          f_spirits, f_sweets, f_tea_coffee, f_water, f_wine)

food_data <- pivot_longer(food_data, f_beer:f_wine, names_to = "Category", values_to = "fraction")

# Strip chart of products purchased
stripchart(fraction~Category,
           data=food_data,
           main="Food Categories Purchased",
           xlab="Food Category",
           ylab="Fraction Purchased",
           col=rainbow(17),
           group.names=c("Beer","Dairy","Eggs","FatsOils","Fish", "Fruit&Vege", "Grains", "MeatRed", "Poultry",
                         "Readymade", "Sauces", "Soft Drinks", "Spirits", "Sweets", "Tea&Coffee", "Water", "Wine"),
           vertical=TRUE,
           pch=16,
           mar=c(0,0,2,0)
)



# Plots of diabetes prevalence by nutrients
ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, carb)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Carbohydrates")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, sugar)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Sugar")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, fat)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Fat")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, saturate)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Saturated Fat")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, protein)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Protein")

ggplot(Diabetes_Food, aes(estimated_diabetes_prevalence, fibre)) +
  geom_point(size = 1.5, shape = 19) + ggtitle("Diabetes Prevalence by Fibre")


# Scatter plots of Carbohydrates in Top 3 Food Categories
ggplot(Diabetes_Food, aes(carb, f_grains)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Carbohydrates by Grains")  

ggplot(Diabetes_Food, aes(carb, f_sweets)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Carbohydrates by Sweets")  

ggplot(Diabetes_Food, aes(carb, f_soft_drinks)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Carbohydrates by Soft Drinks") 



# Scatter plots of Sugar in Top 3 Food Categories
ggplot(Diabetes_Food, aes(sugar, f_sweets)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Sugar by Sweets")  

ggplot(Diabetes_Food, aes(sugar, f_grains)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Sugar by Grains")  

ggplot(Diabetes_Food, aes(sugar, f_soft_drinks)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Sugar by Readymade") 



# Scatter plots of Saturated Fat in Top 3 Food Categories
ggplot(Diabetes_Food, aes(saturate, f_sweets)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Saturated Fat by Sweets")

ggplot(Diabetes_Food, aes(saturate, f_grains)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Saturated Fat by Grains")  

ggplot(Diabetes_Food, aes(saturate, f_soft_drinks)) +
  geom_point(size = 1.5, shape = 9) + ggtitle("Saturated Fat by Soft Drinks") 


#Linear Regression of Nutrients
#Carbohydrates
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$carb, 
     main = "Diabetes Prevalence vs Carbohydrates Regression model",
     col="blue1", pch = 19,
     xlab="Carbohydrates",
     ylab="Estimated Diabetes Prevalence"
     )
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$carb))


#Sugar
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$sugar, 
     main = "Diabetes Prevalence vs Sugar Regression model",
     col="blue2", pch = 19,
     xlab="Sugar",
     ylab="Estimated Diabetes Prevalence"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$sugar))


#Saturated Fat
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$saturate, 
     main = "Diabetes Prevalence vs Saturated Fat Regression model",
     col="blue3", pch = 19,
     xlab="Saturated Fat",
     ylab="Estimated Diabetes Prevalence"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$saturate))


#Grains
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_grains, 
     main = "Diabetes Prevalence vs Grains Regression model",
     col="purple1", pch = 19,
     xlab="Grains",
     ylab="Estimated Diabetes Prevalence"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_grains))


#Sweets
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_sweets, 
     main = "Diabetes Prevalence vs Sweets Regression model",
     col="purple2", pch = 19,
     xlab="Sweets",
     ylab="Estimated Diabetes Prevalence"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_sweets))


#Soft Drinks
plot(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_soft_drinks, 
     main = "Diabetes Prevalence vs Soft Drinks Regression model",
     col="purple3", pch = 19,
     xlab="Soft Drinks",
     ylab="Estimated Diabetes Prevalence"
)
abline(lm(Diabetes_Food$estimated_diabetes_prevalence ~ Diabetes_Food$f_soft_drinks))


#Generate Linear Regression Models
model1 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$carb)
model2 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$sugar)
model3 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$saturate)
model4 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$f_grains)
model5 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$f_sweets)
model6 <- lm(Diabetes_Food$estimated_diabetes_prevalence~Diabetes_Food$f_soft_drinks)

plot(model1)
plot(model2)
plot(model3)
plot(model4)
plot(model5)
plot(model6)

print(summary(model1))
print(summary(model2))
print(summary(model3))
print(summary(model4))
print(summary(model5))
print(summary(model6))

