library(tidyverse)
library(caret)
library(MASS)
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

# Read Data
property_data <- read.csv("cleaned_merged_property_data.txt")


df <- property_data %>%
  mutate(
    #distance to categorial variable
    distance_cbd = 
       case_when(distance_from_CBD < 5 ~ "<5km", 
                 distance_from_CBD < 10 ~ "<10km",
                 distance_from_CBD < 20 ~ "<20km",
                 TRUE ~ '>=20km'),
     distance_public_school= 
       case_when(distance_from_closest_public_school < 1.5 ~ "<1.5km",
                 distance_from_closest_public_school < 3 ~ "<3km",
                 TRUE ~ '>=3km'),
    distance_private_school =
      case_when(distance_from_closest_private_school < 1.5 ~ "<1.5km",
                distance_from_closest_private_school < 3 ~ "<3km",
                TRUE ~ '>=3km'),
    distance_station = 
      case_when(distance_from_closest_station < 1 ~ "<1km",
                distance_from_closest_station < 2 ~ "<2km",
                distance_from_closest_station < 5 ~ "<5km",
                TRUE ~ '>=5km'),
    #house_size to categorial variable
      house_size_cat = cut(house_size, breaks = seq(0, 1000, by = 100)),
    
    #no of bedrooms as factor - No Linear Relationship with property price
    no_of_bed = as.factor(no_of_bed), 
    no_of_bath = as.factor(no_of_bath),
    no_of_parking = as.factor(no_of_parking),
   ) %>%
  #removing outliers - for each LGA remove 3 highest and 3 lowest from each lga
  arrange(lga, price) %>% 
  group_by(lga) %>% 
  slice(3:(n()-2)) %>%
  ungroup() %>%
  arrange(X)

#summary of df
summary(df)
str(df)

#Summary of distance variables
df %>%
  dplyr::select(address, 
         distance_cbd, 
         distance_public_school, 
         distance_station
  ) %>%
  tail()

#histogram of crime data
histogram(df$Drug.offences) 
histogram(df$Non.Violent.Crime) 
histogram(df$Violent.Crime)

#normalise data
crime_vars <- df %>%
  dplyr::select(Drug.offences, Non.Violent.Crime, Violent.Crime)
processed_vars <- preProcess(crime_vars, method = c("YeoJohnson"))
df <- predict(processed_vars, df)
#histogram of crime data
histogram(df$Drug.offences) 
histogram(df$Non.Violent.Crime) 
histogram(df$Violent.Crime)
                             
TrainIndex <- createDataPartition(df$X, p = .8,
                                  list = FALSE,
                                  times = 1)




train_data <- df[TrainIndex,]
test_data <- df[-TrainIndex,]

#Property price with no data
train_data_lm = lm(price ~ 1, train_data)

#Regression with no variable
summary(train_data_lm)

# Step Regression based on p value
step_lm <- step(train_data_lm, 
                direction = c("both"), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console 
                scope = price ~ mean_taxable_income  + 
                  metropolitan_area * (distance_cbd + 
                  distance_public_school + AQI + AQI_exclude_bushfire_month +   
                  Within_5km_2019 + Btwn_5_10km_2019	+ Btwn_10_15km_2019	+ 
                  Within_5km_2018 +	Btwn_5_10km_2018+	Btwn_10_15km_2018) +
                  Non.Violent.Crime + year + no_of_bed + metropolitan_area +
                  no_of_bath + no_of_parking + house_size_cat + type
                , steps = 50) #increase steps for better prediction

# Summary of Result
summary(step_lm)
step_lm$anova

# Result Based on step
test_data$prediction <- predict(step_lm, newdata = test_data)
RMSE(test_data) # RMSE: 392256.67

# Step Regression based on AIC value - Same Result as above
# stepAIC_lm <- stepAIC(train_data_lm,
#   direction = c("both"),
#   trace = 1,
#   scope = price ~ mean_taxable_income  +
#     metropolitan_area * (distance_cbd +
#                            distance_public_school + AQI + AQI_exclude_bushfire_month +
#                            Within_5km_2019 + Btwn_5_10km_2019	+ Btwn_10_15km_2019	+
#                            Within_5km_2018 +	Btwn_5_10km_2018+	Btwn_10_15km_2018) +
#     Non.Violent.Crime + year + no_of_bed +
#     no_of_bath + no_of_parking + house_size + type
#                 , steps = 50)

# Summary of Result
# summary(stepAIC_lm)
# stepAIC_lm$anova

# Result Based on stepAIC
# test_data$prediction <- predict(stepAIC_lm, newdata = test_data)
# RMSE(test_data) # 415,352.79


#Write file as csv
write.csv(test_data, "test_data.csv", row.names = FALSE)
