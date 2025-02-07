#Loading libraries
library(readr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(skimr)
library(knitr)
library(fastDummies)
library(bestglm)


#Loading & exploring data
RentalData <- read.csv("ImmoDataNRW.csv")
dim(RentalData)

# convert the data object to a tibble 
RD = tibble::as_tibble(RentalData)

RD %>% group_by(typeOfFlat) %>% summarise(No_of_apartments = n()) 


## TASKS 1: Data preparation

RD_Dortmund =  RD[RD$regio2 == "Dortmund",]

head(RD_Dortmund)
class(RD_Dortmund)
str(RD_Dortmund)
dim(RD_Dortmund)


#View(RD_Dortmund)

#Check for missing data
sum(is.na(RD_Dortmund))

DataExplorer::plot_missing(RD_Dortmund, ggtheme = ggpubr::theme_pubr())


#Drop variable that has the highest number of missing observations
RD_Dortmund = subset(RD_Dortmund, select = -c(noParkSpaces))
head(RD_Dortmund)
dim(RD_Dortmund)

#Check for rows with missing data
RD_Dortmund[rowSums(is.na(RD_Dortmund)) > 0, ] 

#Remove rows with missing data
RD_Dortmund <- RD_Dortmund[complete.cases(RD_Dortmund), ]
head(RD_Dortmund)
dim(RD_Dortmund)
sum(is.na(RD_Dortmund)) 


#Drop redundant variable after subsetting
RD_Dortmund = subset(RD_Dortmund, select = -c(ID, regio2))
head(RD_Dortmund)
dim(RD_Dortmund)

#Create sqmPrice response variable
RD_Dortmund$sqmPrice <- (RD_Dortmund$totalRent / RD_Dortmund$livingSpace)
colnames(RD_Dortmund)
head(RD_Dortmund)
dim(RD_Dortmund)

#Recategorize typeOfFlat variable
RD_Dortmund <- mutate(RD_Dortmund,typeOfFlat = case_when(typeOfFlat == "apartment" ~ "apartment",
typeOfFlat == "loft" | typeOfFlat == "maisonette" | typeOfFlat == "penthouse" | typeOfFlat == "terraced_flat" | typeOfFlat == "other" ~ "luxurious_artistic_other",
typeOfFlat == "ground_floor" | typeOfFlat == "raised_ground_floor" ~ "r_ground_floor",
typeOfFlat == "roof_storey" | typeOfFlat == "half_basement" ~ "roof_half_basement"))

#View(RD_Dortmund)

RD_Dortmund %>% group_by(typeOfFlat) %>% summarise(No_of_properties = n(), Mean_sqmPrice = mean(sqmPrice)) 


#Setting factor variables
FactorCols <- c("newlyConst", "balcony", "hasKitchen", "lift", "typeOfFlat", "garden", "condition", "lastRefurbish", "energyEfficiencyClass")

RD_Dortmund[FactorCols] <- lapply(RD_Dortmund[FactorCols], factor) 

sapply(RD_Dortmund, class)

head(RD_Dortmund)


#Exploratory Data Analysis#

#Histogram / Density Plot of Price per Square Meter
hist(RD_Dortmund$sqmPrice, main ="Histogram / Density Plot of Price per Square Meter", 
     xlab = "Price per Square Meter", freq = FALSE)
lines(density(RD_Dortmund$sqmPrice,na.rm=T),col="red",lwd=1)

#Histogram / Density Plot of Year Constructed
hist(RD_Dortmund$yearConstructed, main ="Histogram / Density Plot of Year Constructed", 
     xlab = "Year Constructed", freq = FALSE)
lines(density(RD_Dortmund$yearConstructed,na.rm=T),col="red",lwd=1)

#Histogram of Price per Square Meter for various types of flats
ggplot(RD_Dortmund, aes(x = sqmPrice)) +
  geom_histogram(aes(y =..density..),fill = "white", colour = "black",alpha=0.6, binwidth = 2) +
  facet_wrap(~typeOfFlat) +
  geom_density(alpha=0.2, fill="lightblue")

##Graphical analysis: correlation of continuous predictor variables with target variable 

#Price per Square Meter vs Total Rent
plot(RD_Dortmund$totalRent, RD_Dortmund$sqmPrice, main="Price per Square Meter vs Total Rent",
     xlab="Total Rent", ylab="Price per Square Meter", pch=21)
#Correlation between Price per Square Meter vs Total Rent with regression line
abline(lm(RD_Dortmund$sqmPrice~RD_Dortmund$totalRent), col="green") 

#Price per Square Meter vs Living Space
plot(RD_Dortmund$livingSpace, RD_Dortmund$sqmPrice, main="Price per Square Meter vs Living Space",
     xlab="Living Space (square meters)", ylab="Price per Square Meter", pch=21)
#Correlation between Price per Square Meter vs Living Space with regression line
abline(lm(RD_Dortmund$sqmPrice~RD_Dortmund$livingSpace), col="green") 

#Boxplot of Price per Square Meter for flats vs hasKitchen
plot(RD_Dortmund$hasKitchen, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs hasKitchen", 
     xlab = "hasKitchen", ylab = "sqmPrice")

#Boxplot of Price per Square Meter vs typeOfFlat
plot(RD_Dortmund$typeOfFlat, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs Type Of Flat", 
     xlab = "typeOfFlat", ylab = "sqmPrice")

#Boxplot of Price per Square Meter vs condition
plot(RD_Dortmund$condition, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs Condition", 
     xlab = "condition", ylab = "sqmPrice")

#Boxplot of Price per Square Meter vs condition
plot(RD_Dortmund$newlyConst, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs newlyConst", 
     xlab = "newlyConst", ylab = "sqmPrice")


#Boxplot of Price per Square Meter vs balcony
plot(RD_Dortmund$balcony, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs Balcony", 
     xlab = "balcony", ylab = "sqmPrice")

#Boxplot of Price per Square Meter vs lastRefurbish
plot(RD_Dortmund$lastRefurbish, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs lastRefurbish", 
     xlab = "lastRefurbish", ylab = "sqmPrice")

#Boxplot of Price per Square Meter vs typeOfFlat
plot(RD_Dortmund$typeOfFlat, RD_Dortmund$sqmPrice, main ="Boxplot of Price per Square Meter vs typeOfFlat", 
     xlab = "typeOfFlat", ylab = "sqmPrice")


#Summary of Factor variables
skimr::partition(skim(RD_Dortmund)) %>% .$factor %>% View()


#Summary of Numeric variables
skimr::partition(skim(RD_Dortmund)) %>% .$numeric %>% dplyr::select(skim_variable, n_missing, mean, sd) #%>% View()



## TASKS 2: Linear regression
RD_Dortmund = subset(RD_Dortmund, select = -c(totalRent))

#Create dummy variables for categorical variables (dummy variables encoding)
#Drop first column of dummy variables as reference variable to avoid dummy variable trap.
#Remove original variables converted to dummy variables.
RD_Dortmund <- dummy_cols(RD_Dortmund, remove_first_dummy = TRUE,  remove_selected_columns = TRUE)

dim(RD_Dortmund)
data.frame(colnames(RD_Dortmund))

#View(RD_Dortmund)

##Best subsets regression -computing best regression subsets with bestglm library

# the Xy matrix needs y as the right-most variable:
RD_Xy = subset(RD_Dortmund, select = -c(sqmPrice))
dim(RD_Xy)
data.frame(colnames(RD_Xy))

RD_Xy = cbind(RD_Xy, sqmPrice = RD_Dortmund$sqmPrice)
dim(RD_Xy)
data.frame(colnames(RD_Xy))

#Best 5 AIC Models
bglm.AIC = bestglm(Xy = RD_Xy, family = gaussian, IC = "AIC", TopModels = 5, method = "exhaustive", nvmax = "default")

bglm.AIC$BestModels

#Best of AIC Models
AICModel1 <- lm(sqmPrice ~  yearConstructed + livingSpace + floor + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + typeOfFlat_r_ground_floor + typeOfFlat_roof_half_basement + condition_good 
                + `energyEfficiencyClass_D/E/F/G/H` + energyEfficiencyClass_NO_INFORMATION, data = RD_Dortmund)

AICModel2 <- lm(sqmPrice ~  yearConstructed + livingSpace + floor + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + typeOfFlat_r_ground_floor +condition_good + `energyEfficiencyClass_D/E/F/G/H` + 
                  energyEfficiencyClass_NO_INFORMATION, data = RD_Dortmund)

AICModel3 <- lm(sqmPrice ~  yearConstructed + livingSpace + floor + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + typeOfFlat_r_ground_floor + condition_good + lastRefurbish_NO_INFORMATION 
                + `energyEfficiencyClass_D/E/F/G/H` + energyEfficiencyClass_NO_INFORMATION, data = RD_Dortmund)

AICModel4 <- lm(sqmPrice ~  yearConstructed + livingSpace + floor + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + typeOfFlat_r_ground_floor + typeOfFlat_roof_half_basement 
                + condition_good + lastRefurbish_NO_INFORMATION + `energyEfficiencyClass_D/E/F/G/H` + energyEfficiencyClass_NO_INFORMATION, data = RD_Dortmund)

AICModel5 <- lm(sqmPrice ~  yearConstructed + livingSpace + floor + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + typeOfFlat_r_ground_floor + typeOfFlat_roof_half_basement 
                + condition_good + condition_NO_INFORMATION + lastRefurbish_NO_INFORMATION + `energyEfficiencyClass_D/E/F/G/H` + energyEfficiencyClass_NO_INFORMATION, data = RD_Dortmund)

summary(bglm.AIC$BestModel)


#Best 5 BIC Models
bglm.BIC = bestglm(Xy = RD_Xy, family = gaussian, IC = "BIC", TopModels = 5, method = "exhaustive", nvmax = "default")

bglm.BIC$BestModels

#Best of BIC Models
BICModel1 <- lm(sqmPrice ~  livingSpace + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                + typeOfFlat_luxurious_artistic_other + condition_good, data = RD_Dortmund)

BICModel2 <- lm(sqmPrice ~ yearConstructed + livingSpace + newlyConst_TRUE + hasKitchen_TRUE 
                + lift_TRUE + condition_good + condition_NO_INFORMATION, data = RD_Dortmund)

BICModel3 <- lm(sqmPrice ~  livingSpace + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE 
                 + condition_good + typeOfFlat_luxurious_artistic_other, data = RD_Dortmund)

BICModel4 <- lm(sqmPrice ~  yearConstructed + livingSpace + newlyConst_TRUE + hasKitchen_TRUE 
                + lift_TRUE + typeOfFlat_luxurious_artistic_other + condition_good, data = RD_Dortmund)

BICModel5 <- lm(sqmPrice ~  livingSpace + newlyConst_TRUE + hasKitchen_TRUE + lift_TRUE + 
                  typeOfFlat_luxurious_artistic_other + condition_good + lastRefurbish_NO_INFORMATION, data = RD_Dortmund)

summary(bglm.BIC$BestModel)

#AIC Scores of Best AIC Models
A <- as.data.frame(AIC(AICModel1,AICModel2,AICModel3,AICModel4,AICModel5)) 
A

#BIC Scores of Best BIC Models
B <-  as.data.frame(BIC(BICModel1,BICModel2,BICModel3,BICModel4,BICModel5))
B


##Summary of best model by AIC
#AICModel1 returned the lowest AIC
summary(AICModel1)

##Summary of best model by BIC
#BICModel1 returned the lowest BIC
summary(BICModel1)

#comparing the two models
anova(BICModel1, AICModel1)


#Extracting summary details from best AIC model

as.data.frame(coefficients(AICModel1))

summary(AICModel1)$r.squared

summary(AICModel1)$adj.r.squared




#Extracting 95% confidence intervals for regression parameters from best AIC model

confint(AICModel1, level = 0.95)












