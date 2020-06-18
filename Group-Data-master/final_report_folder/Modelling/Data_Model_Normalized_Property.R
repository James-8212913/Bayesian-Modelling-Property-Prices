library(caret)
library(MASS)
library(tidyverse)
# Set seed
set.seed(2)
rm(list = ls())
dev.off()

# RMSE & MAE function

RMSE = function(x){
  RMSE = sqrt(colMeans((x[,"price"] - x[,"prediction"])^2))
  # print(sprintf("RMSE: %.2f", RMSE))
  # mae = colMeans(abs(x[,"price"] - x[,"prediction"]))
  # print(sprintf("MAE: %.2f", mae))
  return(RMSE)
}

property_data <- read.csv("Data/Property/cleaned_merged_property_data_final.csv")

names(property_data)
df <- property_data %>%
  mutate(
    #distance to categorial variable
    distance_from_school = apply(cbind(distance_from_closest_public_school, distance_from_closest_private_school), 1, FUN = min),
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
    
    #no of bedrooms as factor - No Linear Relationship with property price
    no_of_bed_num = no_of_bed,
    no_of_bed = as.factor(no_of_bed),
    no_of_bath_num = no_of_bath,
    no_of_bath = as.factor(no_of_bath),
    no_of_parking_num = no_of_parking,
    no_of_parking = as.factor(no_of_parking),
   ) %>% 

  #removing outliers - for each LGA remove 3 highest and 3 lowest from each lga
  arrange(lga, price) %>% 
  group_by(lga) %>% 
  slice(3:(n()-2)) %>%
  ungroup() %>%
  arrange(property_id) %>%
  mutate_at(
    vars(Within_5km_2019, 
         Btwn_5_10km_2019, 
         Btwn_10_15km_2019, 
         Btwn_5_10km_2018,
         Btwn_5_10km_2018,
         Btwn_10_15km_2018,
         ),
    function(x) ifelse(x == "Y", 1, 0)) %>%
  mutate_at(
    vars(SAdvDisav_decile,
         ER_decile,
         EDUOCC_decile),
    as.numeric
  )

df<- df %>% filter(no_of_bed_num >= 3 & no_of_bed_num <= 4, 
                              no_of_bath_num == 2, 
                              no_of_parking_num == 2, 
                              house_size > 500 & house_size <=700
)

#summary 
names(df)
str(df)
#summary of df
summary(df)
str(df)

mean(property_data$no_of_bed) #3.540585
table(property_data$no_of_bed)

mean(property_data$no_of_bath) #1.837778
table(property_data$no_of_bed)

mean(property_data$no_of_parking) #1.903606
table(property_data$no_of_parking)

table(df$house_size_cat)

nrow(property_data %>% filter(no_of_bed >= 3 & no_of_bed <=4, 
                              no_of_bath == 2, 
                              no_of_parking == 2, 
                              house_size > 500 & house_size <=700
                              )
     )

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


df %>%
  dplyr::select(price, Drug.offences, Non.Violent.Crime,  Violent.Crime, mean_taxable_income) %>%
  cor() %>%
  corrplot(method = "number")

names(df)
df %>%
  dplyr::select(Nom.ratio, Nom.number, price, avg_temp) %>%
  cor() %>%
  corrplot(method = "number")
                             
df %>%
  filter(no_of_bath != 8)

TrainIndex <- createDataPartition(df$property_id, p = .8,
                                  list = FALSE,
                                  times = 1)

df$no_of_bath = as.integer(df$no_of_bath)
df$no_of_parking = as.integer(df$no_of_parking)

train_data <- df[TrainIndex,]
test_data <- df[-TrainIndex,]
str(train_data)
#Property price with no data


train_data_lm = lm(price ~ 1, train_data)

#Regression with no variable
summary(train_data_lm)

# Step Regression based on p value
step_lm <- step(train_data_lm, 
                direction = c("forward"), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console 
                scope = price ~ mean_taxable_income  + 
                  metropolitan_area * (distance_cbd 
                   + AQI + AQI_exclude_bushfire_month + 
                  distance_from_closest_station + distance_from_school +
                  Within_5km_2019 + Btwn_5_10km_2019	+ Btwn_10_15km_2019	+ 
                  Within_5km_2018 +	Btwn_5_10km_2018+	Btwn_10_15km_2018)
                  + year + 
                  no_of_bed + 
                  metropolitan_area +
                  # no_of_bath + 
                  # no_of_parking + 
                  house_size_cat + type  + 
                  Violent.Crime + Nom.ratio + avg_temp + EDUOCC_decile 
                , steps = 100) #increase steps for better prediction

normalized_model_coefficients <- as_tibble(tibble::rownames_to_column(as.data.frame(summary(step_lm)$coefficients)))


write_csv(normalized_model_coefficients, "model_coefficients_V4.csv")


# Summary of Result
summary(step_lm) # adj. r squared: 0.7298
step_lm$anova
test_data$prediction <- predict(step_lm, newdata = test_data)
# Result Based on step
RMSE(test_data) # RMSE: 361,712.2
for (i in 1:10){
  TrainIndex <- createDataPartition(df$property_id, p = .8,
                                    list = FALSE,
                                    times = 1)
  train_data <- df[TrainIndex,]
  test_data <- df[-TrainIndex,]
  
  #Property price with no data
  train_data_lm = lm(price ~ 1, train_data)
  test_data$prediction <- predict(step_lm, newdata = test_data)
  print(sprintf("RMSE for %i is %.2f", i, RMSE(test_data)))
}  

#test with all data
train_data_lm = lm(price ~ 1, df)

#Regression with no variable
summary(train_data_lm)
str(data)
# Step Regression based on p value
step_lm <- step(train_data_lm, 
                direction = c("forward"), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console 
                scope = price ~ mean_taxable_income  + 
                  metropolitan_area * (distance_cbd 
                                       + AQI + AQI_exclude_bushfire_month + 
                                         distance_from_closest_station + distance_from_school +
                                         Within_5km_2019 + Btwn_5_10km_2019	+ Btwn_10_15km_2019	+ 
                                         Within_5km_2018 +	Btwn_5_10km_2018+	Btwn_10_15km_2018)
                + year + no_of_bed + metropolitan_area +  + 
                  no_of_bath + no_of_parking + house_size_cat + type  + 
                  Violent.Crime + Nom.ratio + avg_temp + EDUOCC_decile + Drug.offences
                + Violent.Crime 
                , steps = 100)
summary(step_lm) # adj. r squared: 0.7328
step_lm$anova


normalized_model_coefficients <- as_tibble(tibble::rownames_to_column(as.data.frame(summary(step_lm)$coefficients)))


write_csv(normalized_model_coefficients, "model_coefficients_V4.csv")

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
