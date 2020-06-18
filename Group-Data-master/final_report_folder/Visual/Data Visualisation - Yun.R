library(MASS)
library(ggplot2)
library(caret)
library(tidyverse)
library(scales)
library(corrplot)
# Read Data
set.seed(10)
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
  slice(4:(n()-3)) %>%
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

# no of bath 8
df %>%
  filter(no_of_bath == 8) %>%
  dplyr::select(url)
#Most expenseive property
df %>%
  arrange(-price) %>%
  slice(1) %>%
  dplyr::select(url)

# Property Price vs Number of Bathrooms
df %>%
  ggplot(aes(x = no_of_bath, y = price)) +
  geom_boxplot() + 
  scale_y_continuous(labels = comma) +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  labs(x = "Number of Bathrooms",
       y = "Property Price",
       title = "Property Price by Number of Bathrooms")

# Property Price vs Property Type
df %>%
  ggplot(aes(x = type, y = price)) +
  geom_boxplot() + 
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Property Type",
       y = "Property Price",
       title = "Property Price by Property Type")

# Filter Terace type only
df %>%
  filter(type == "Terrace")

# 5000 samples property price vs property size by property type
df %>%
  slice(sample(1:nrow(df), 5000, replace = FALSE)) %>%
  ggplot(aes(x = house_size, y = price, colour = type)) +
  geom_jitter() + 
  scale_y_continuous(labels = comma) +
  # stat_summary(fun.y=mean, colour="darkred", geom="point", 
  #              shape=18, size=3,show_guide = FALSE) +
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Property Property Size",
       y = "Property Price",
       subtitle = "5000 properties are randomly selected",
       title = "Property Price by Property Size",
       colour = "Property Type")

# Apartment / Unit / Flat over 500 sqm 
df %>%
  filter(type == "Apartment / Unit / Flat" & house_size > 500) %>%
  dplyr::select(url) %>%
  nrow()

#taxable income corpplot
df %>%
  dplyr::select(price, mean_taxable_income, median_taxable_income) %>%
  cor() %>%
  corrplot(method = "number")


# Property PRice distribution
df %>%
  filter(distance_from_CBD < 50 & metropolitan_area == TRUE) %>%
  mutate(distance_cbd = factor(distance_cbd, levels= c("<5km", "<10km","<20km","<30km",'>=30km') )) %>%
  ggplot(aes(x = price, colour = distance_cbd )) +
  geom_density(adjust = 3) +
  geom_vline(xintercept= mean(df$price), linetype="dotted") +
  scale_x_continuous(labels = comma, limits=c(0, 5000000)) +
  expand_limits(x=0, y=0) +
  labs(x = "Property Price",
       title = "Distribution of Property Price by distance from CBD",
       colour = "Distance from CBD",
       subtitle = "properties within 50km from CBD ") +
  theme(axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        # axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        # legend.position="none",
        # panel.background=element_blank(),
        # panel.border=element_blank(),
        # panel.grid.major=element_blank(),
        # panel.grid.minor=element_blank(),
        # plot.background=element_blank()
        ) 

#property price by distance_cbd
df %>%
  group_by(distance_cbd) %>%
  summarise(price = mean(price))
  
# property price within 10km
df %>%
  filter(distance_cbd %in% c("<5km", "<10km")) %>%
  summarise(price = mean(price))



mean(df$price)

# property data by taxable income


df %>%
  group_by(postcode) %>%
  mutate(price = mean(price))%>%
  slice(1) %>%
  ungroup() %>%
  view()


#Average Price vs Taxable Income
df %>%
  group_by(postcode) %>%
  mutate(price = mean(price))%>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(x = mean_taxable_income, y = price)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(x = "Average Taxable Income",
       y = "Average Property Price per Post Code",
       title = "Average Property Price by Average Taxable Income") 



#Filter postcode 2030 data
df %>%
  filter(postcode == 2030) %>%
  head()

#Education and Ocuupation Index and  Taxable Income
df %>%
  group_by(postcode) %>%
  slice(1) %>%
  ggplot(aes(x = as.factor(EDUOCC_decile), y = mean_taxable_income))+
  geom_boxplot() +
  stat_summary(fun.y=mean, colour="darkred", geom="point", 
               shape=18, size=3,show_guide = FALSE) +
  labs(x = "Education and Occupation Index Decile",
       y = "Average Taxable Income",
       title = "Average Property Price by Education and Occupation Index") 

