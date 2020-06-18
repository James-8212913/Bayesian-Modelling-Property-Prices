library(tidyverse)
library(caret)
library(MASS)
library(broom)
library(purrr)


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
property_data <- read.csv("cleaned_merged_property_data_nom.csv")

#Run for particular Suburb
suburb <- "Campbelltown"

property_data <- property_data %>% filter(property_data$suburb == "Campbelltown")

df <- property_data %>%
  mutate(
    #distance to categorial variable
    distance_cbd = 
      case_when(distance_from_CBD < 5 ~ "<5km", 
                distance_from_CBD < 10 ~ "<10km",
                distance_from_CBD < 20 ~ "<20km",
                distance_from_CBD < 30 ~ "<30km",
                TRUE ~ '>=30km'),
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
                distance_from_closest_station < 10 ~ "<10km",
                TRUE ~ '>=10km'),
    #house_size to categorial variable
    house_size_cat = cut(house_size, breaks = seq(0, 1000, by = 100)),
    no_of_bed_cat  = cut(no_of_bath, breaks = seq(0, 10, by = 1)),
    no_of_bath_cat = cut(no_of_bath, breaks = seq(0, 10, by = 1)),
    
    #no of bedrooms as factor - No Linear Relationship with property price
    # no_of_bed = as.factor(no_of_bed), 
    # no_of_bath = as.factor(no_of_bath),
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


TrainIndex <- createDataPartition(df$X, p = .8,
                                  list = FALSE,
                                  times = 1)




train_data <- df[TrainIndex,]
test_data <- df[-TrainIndex,]

#Property price with no data
train_data_lm = lm(price ~ 1, train_data)

#Regression with no variable
summary(train_data_lm)

str(train_data)

#Removing these variables as they all return 1 unique values as they are common for suburb
# unique(train_data$distance_private_school)
# unique(train_data$distance_public_school)
# unique(train_data$mean_taxable_income)
# unique(train_data$metropolitan_area)
# unique(train_data$distance_cbd)
# unique(train_data$AQI)
# unique(train_data$AQI_exclude_bushfire_month)
# unique(train_data$Within_5km_2019) 
# unique(train_data$Btwn_5_10km_2019)
# unique(train_data$Btwn_10_15km_2019	) 
# unique(train_data$Within_5km_2018 )	
# unique(train_data$Btwn_5_10km_2018 )	
# unique(train_data$Btwn_10_15km_2018)

# Step Regression based on p value

step_lm <- step(train_data_lm, 
                direction = c("both"), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console 
                scope = price   ~ 
                  year + 
                  no_of_bed_cat + 
                  no_of_bath_cat + 
                  no_of_parking + 
                  house_size_cat + 
                  type +
                  distance_station  
                ,steps = 50) #increase steps for better prediction

# Summary of Result
# sink()
summary(step_lm)
step_lm$anova

sink(paste("Regression/",suburb,".txt"))
print("*********Step LM Results *****************")
print( summary(step_lm))
print("*********Step LM ANOVA *****************")
print( step_lm$anova)


# Result Based on step
test_data$prediction <- predict(step_lm, newdata = test_data)
RMSE <- RMSE(test_data) # RMSE: 386340.74


print("*********RMSE Results *****************")
print(RMSE(test_data))

# tidy_lmfit <- tidy(step_lm)


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
write.csv(test_data, paste("Regression/test_data_",suburb,".csv",sep=""), row.names = FALSE)
closeAllConnections()
