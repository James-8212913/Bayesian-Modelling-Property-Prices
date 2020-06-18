library(MASS)
library(caret)
library(tidyverse)
library(scales)
library(corrplot)
# Read Data
set.seed(10)
property_data <- read.csv("Data/Property/cleaned_merged_property_data_final.csv")


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
    
    #no of bedrooms as factor - No Linear Relationship with property price
    no_of_bed = as.factor(no_of_bed), 
    no_of_bath = as.factor(no_of_bath),
    no_of_parking = as.factor(no_of_parking),
  ) %>%
  #removing outliers - for each LGA remove 2 highest and 2 lowest from each lga
  arrange(lga, price) %>% 
  group_by(lga) %>% 
  slice(3:(n()-2)) %>%
  ungroup() %>%
  arrange(X)


#SEIFA Analysis
df %>%
  dplyr::select(EDUOCC_Decile=EDUOCC_decile, ER_Decile=ER_decile,SAdvDisav_Decile=SAdvDisav_decile,Price=price)  %>%
  cor() %>%
  corrplot(method = "number",tl.cex = 0.7,tl.col="#8B0000")

#Nom Analysis
df %>%
  dplyr::select(Nom.Ratio=Nom.ratio, Distance_From_CBD = distance_from_CBD,Mean_Taxable_Income = mean_taxable_income,Price=price ) %>%
  cor() %>%
  corrplot(method = "number",tl.cex = 0.7,tl.col="#8B0000")


# BushFire Impact vs Property Price

Bushfire <- read.csv("Data/Bushfire_mapdata.csv") %>% arrange(desc(TotalProperties)) %>% slice(1:5)

a <-  df %>%
  filter(!is.na(lga)) %>%
  inner_join(Bushfire,by="lga")  %>%
  filter(distance_from_CBD > 40 & 
           (Within_5km_2019  == 1 | Btwn_5_10km_2019 == 1 | Btwn_10_15km_2019 == 1 )) %>%
   mutate(BushFire =
  case_when(Within_5km_2019  ==1 ~ "<5km",
            Btwn_5_10km_2019 == 1 ~ "<5-10km",
            Btwn_10_15km_2019 == 1 ~ "<10-15km",
            TRUE ~ '>=15km')
           ) %>%
  group_by(lga) %>%
  arrange(desc(price)) %>%
  slice(1:5) %>%
  dplyr :: select(Metropolitan_Area=metropolitan_area,price,BushFire,lga,suburb,Within_5km_2019,Btwn_5_10km_2019,Btwn_10_15km_2019,distance_from_CBD)

ggplot(aes(x = suburb, y = price, color=BushFire, fill=Metropolitan_Area),data=a) +
  geom_boxplot() + 
  # scale_y_continuous(labels = comma) +
  stat_summary(fun.y=mean, colour="darkred", geom="point",
               shape=18, size=3,show_guide = FALSE) +
  #scale_y_continuous(labels = comma, limits=c(0, 5000000)) +
  labs(x = "Suburb",
       y = "Property Price",
       title = "Most Impacted Suburbs by BushFire")
