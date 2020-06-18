library(tidyverse)
library(gridExtra)
library(plotly)
library(corrplot)
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
aa<- read.csv("Data/Property/cleaned_merged_property_data_final.csv")
property_data <- read.csv("Data/Property/cleaned_merged_property_data_final.csv")
names(property_data)
df <- property_data %>%
  mutate(
    #distance to categorial variable
    distance_from_school = min(distance_from_closest_public_school, distance_from_closest_public_school),
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
    no_of_bed = as.factor(no_of_bed), 
    no_of_bath = as.factor(no_of_bath),
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
#summary 
names(df)
str(df)
#summary of df
summary(df)
str(df)
aa %>% hist(Nom.number)
ggplot(data = aa) + 
  geom_point(mapping = aes(x = postcode, y = Nom.number, size =Nom.number, colour = "blue"))
ggplot(data = aa) + 
  geom_point(mapping = aes(x = postcode, y = Nom.number, size =Nom.number))

ggplot(data = aa, mapping = aes(x = postcode, y = Nom.number)) + 
  geom_point() + 
  geom_smooth()
ggplot(data = aa, mapping = aes(x = postcode, y = distance_from_CBD)) + 
  geom_point() + 
  geom_smooth()
ggplot(data = aa) + 
  geom_bar(mapping = aes(x = postcode))
#Correlations
df %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")

#sssssssssssssssssssssssssssssssssssssss
nrow(df %>% filter(distance_from_CBD <= 5))
df %>% filter(distance_from_CBD <= 5) %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")
nrow(df %>% filter(distance_from_CBD > 5 & distance_from_CBD <= 10))
df %>% filter(distance_from_CBD > 5 & distance_from_CBD <= 10) %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")
nrow(df %>% filter(distance_from_CBD > 10 & distance_from_CBD <= 20))
df %>% filter(distance_from_CBD > 10 & distance_from_CBD <= 20) %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")
nrow(df %>% filter(distance_from_CBD > 20 & distance_from_CBD <= 30))
b <- df %>% filter(distance_from_CBD > 20 & distance_from_CBD <= 30) %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")
nrow(df %>% filter(distance_from_CBD > 30))
a <- df %>% filter(distance_from_CBD > 30) %>%
  dplyr::select(price, Nom.number) %>%
  cor() %>%
  corrplot(method = "number")




