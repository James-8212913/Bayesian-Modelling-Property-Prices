library(tidyverse)
library(caret)
library(MASS)
library(psych)
library(corrplot)
# Set seed
set.seed(2)
rm(list = ls())
dev.off()

# RMSE & MAE function

RMSE = function(x){
  
  RMSE = sqrt(colMeans((x[,"price"] - x[,"prediction"])^2))
  print(sprintf("RMSE: %.2f", RMSE))
  mae = colMeans(abs(x[,"price"] - x[,"prediction"]))
  print(sprintf("MAE: %.2f", mae))
}

# Merge Read Data
property_data <- read.csv("cleaned_merged_property_data_nom.csv", stringsAsFactors=FALSE) %>% filter(!is.na(lga))
Nom <- read.csv("NOM_Ratio_2019.csv", stringsAsFactors=FALSE)

#Prepare Prop with Nom data
property_data <- left_join(property_data,Nom,by="lga")
property_data <- property_data[, -c(37,39)]

#format some values
names(property_data)[38] <- "Nom.number"
names(property_data)[39] <- "Nom.ratio"
names(property_data)

#Preparing SEIF Data
seifa <- read.csv("SEIFA_2016_v2.csv", stringsAsFactors=FALSE)
str(seifa)

#Prepare Prop with Nom data
seifa$suburb <- tolower(seifa$suburb)
property_data$suburb <- str_replace_all(tolower(property_data$suburb),"-"," ")

prop_seifa <- left_join(property_data,seifa,by="suburb")
prop_seifa <- prop_seifa[,-c(40)]
names(prop_seifa)[2] <- "X"
prop_seifa$EDUOCC_decile <- as.numeric(prop_seifa$SAdvDisav_decile)
prop_seifa$EDUOCC_decile <- as.numeric(prop_seifa$ER_decile)
prop_seifa$EDUOCC_decile <- as.numeric(prop_seifa$EDUOCC_decile)

names(prop_seifa)
#Remove outlisers like the main model
predf <- prop_seifa %>%
  filter(property_id != 6758) %>%
  #removing outliers - for each LGA remove 3 highest and 3 lowest from each lga
  arrange(lga, price) %>% 
  group_by(lga) %>% 
  slice(3:(n()-2)) %>%
  ungroup() %>%
  arrange(X)

#Convert bushfire values to numerical
predf$Within_5km_2019 <- ifelse(predf$Within_5km_2019=="Y",1,0)
predf$Btwn_5_10km_2019 <- ifelse(predf$Btwn_5_10km_2019=="Y",1,0)
predf$Btwn_10_15km_2019 <- ifelse(predf$Btwn_10_15km_2019=="Y",1,0)
predf$metropolitan_area <- ifelse(predf$metropolitan_area=="TRUE",1,0)

#Normalise crime data
crime_vars <- predf %>%
  dplyr::select(Drug.offences, Non.Violent.Crime, Violent.Crime)
processed_vars <- preProcess(crime_vars, method = c("YeoJohnson"))
predf <- predict(processed_vars, predf)

names(predf)

#Identify variables for suburb level

df <- predf %>%
dplyr :: select(
        type,
        X,     
        price,
       income=median_taxable_income,
       AQI,
       DrugOff=Drug.offences,
       VioCrime=Violent.Crime,
       Nom=Nom.ratio,
       distCBD=distance_from_CBD,
       met=metropolitan_area,
       seifa_edu=EDUOCC_decile,
       1
       )
    

#Sample manual correlation 
cor(df$price,df$income)
cor(df$price,df$Nom)

#Full Correlation matrix
df1 <- df[, -c(1,2)]

str(df1)

# df1 <- df
names(df1)

corrplot(cor(df1[,-ncol(df1)]),method="number", type="upper")
#corrplot(cor(df1[,-ncol(df1)]),method="ellipse", type="upper")
# corrplot(cor(df1[,-ncol(df1)]),order="AOE",method="color",addCoef.col = "gray")

TrainIndex <- createDataPartition(df$X, p = .8,
                                  list = FALSE,
                                  times = 1)


train_data <- df[TrainIndex,]
test_data <- df[-TrainIndex,]

#Property price with no data
train_data_lm = lm(price ~ 1, train_data)

names(train_data)

# train_data_lm = lm(meanprice ~ meanincome + meancbddist + meannom + meanVC + meanDO, train_data)  

# #Regression with no variable
# summary(train_data_lm)


# Step Regression based on p value
step_lm <- step(train_data_lm,
                direction = c("both"), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console
                scope = price ~
                  income  +
                  distCBD * met +
                  AQI +
                  # DrugOff +
                  VioCrime +
                  Nom +
                  type +
                  seifa_edu
                , steps = 50) #increase steps for better prediction

# Summary of Result
summary(step_lm) # adj. r squared: 0.7245 
step_lm$anova

# Result Based on step
test_data$prediction <- predict(step_lm, newdata = test_data)
RMSE(test_data) # RMSE: 386340.74
