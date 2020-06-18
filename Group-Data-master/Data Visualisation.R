library(MASS)
library(caret)
library(tidyverse)
library(scales)
library(corrplot)
# Read Data
set.seed(10)
property_data <- read.csv("cleaned_merged_property_data_nom.csv")


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
    distance_school = 
      case_when(distance_from_school < 1 ~ "<1km",
                distance_from_school < 3 ~ "<3km",
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
  scale_y_continuous(labels = comma) +
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
  cor()


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
  labs(x = "Average Property Price per Post Code",
       y = "Average Taxable Income",
       title = "Average Property Price by Average Taxable Income") 

df %>%
  group_by(postcode) %>%
  mutate(price = mean(price))%>%
  slice(1) %>%
  dplyr::select(postcode, price) %>%
  view()

df %>%
  filter(postcode == 2030) %>%
  view()

                ### WEATHER (TEMPERATURE)
# Change 'date' format
new_date<-strptime(test_data$date, format="%d/%m/%Y")
test_data$date<-as.Date(new_date, format="%y/%m/%d")
str(test_data)
                
# Correlation between 'temperature' and 'property price'
# leave only year and month
test_data$year_month<-format(as.Date(test_data$date), "%y-%m")
# mean value of temperature
month.avg<-aggregate(test_data$avg_temp, by=list(test_data$year_month), mean)
head(month.avg)
# mean value of property price
price.avg<-aggregate(test_data$price, by=list(test_data$year_month), mean)
head(price.avg)
# 'square root' price twice (because it's too big)
price.avg$x<-sqrt(price.avg$x)
head(price.avg)
price.avg$x<-sqrt(price.avg$x)
head(price.avg)
# merge two data frames
avg_temp_price<-merge(month.avg, price.avg, by=c("Group.1"))
colnames(avg_temp_price)<-c("date","avg_temp","avg_price")
head(avg_temp_price)
                
# Create line plot
date1<-avg_temp_price$date
avg_temp1<-avg_temp_price$avg_temp
avg_price1<-avg_temp_price$avg_price
df1<-data.frame(x=date1, y=avg_temp1, z=avg_price1)
ggplot(df1, aes(x, group=1))+
  geom_line(size=1, aes(y=y, colour="temperature"))+
  geom_line(size=1, aes(y=z, colour="double square rooted property price"))+
  labs(x="Date",
       y="Average Value",
       title="Fluctuations of Monthly Average Temperature and Property Price by Date")
                
# Simple regression model - 1
plot(avg_price~avg_temp, data=avg_temp_price)
model1<-lm(avg_price~avg_temp, avg_temp_price)
model1
plot(avg_price~avg_temp, data=avg_temp_price, xlab="Temperature", ylab="Double SQRT Property Price",
     main="Correlation between Temperature and Property Price")
abline(coef(model1))

# Predict property price
avg_temp=15
avg_price=(-0.0208)*avg_temp+32.3744
print(avg_price)
print(avg_price*avg_price*avg_price*avg_price)
                
# Compare actual and predicted property price
avg_temp<-avg_temp_price[,2]
avg_temp
pred<-(-0.0208)*avg_temp+32.3744
pred<-(pred*pred*pred*pred)
pred
compare<-cbind(pred, avg_temp_price[,3], abs(pred-avg_temp_price[,3]))
compare
colnames(compare)<-c("pred","temp","actual")
compare


# Property Price Distribution by distance from Train Station
df %>%
  filter(distance_from_closest_station < 15 & metropolitan_area == TRUE) %>%
  mutate(distance_station = factor(distance_station, levels = c("<1km", "<2km","<5km","<10km",'>=10km') )) %>%
  ggplot(aes(x = price, colour = distance_station )) +
  geom_density(adjust = 3) +
  geom_vline(xintercept = mean(df$price), linetype = "dotted") +
  scale_x_continuous(labels = comma, limits=c(0, 5000000)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "Property Price",
       title = "Distribution of Property Price by distance from Closest Train Station",
       colour = "Distance from Closest Station",
       subtitle = "properties within 15km from Train Station ") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
  )

# Property Price Distribution by distance from School
df %>%
  filter(distance_from_school < 10 & metropolitan_area == TRUE) %>%
  mutate(distance_school = factor(distance_school, levels = c("<1km", "<3km", '>=3km') )) %>%
  ggplot(aes(x = price, colour = distance_school )) +
  geom_density(adjust = 3) +
  geom_vline(xintercept = mean(df$price), linetype = "dotted") +
  scale_x_continuous(labels = comma, limits=c(0, 5000000)) +
  expand_limits(x = 0, y = 0) +
  labs(x = "Property Price",
       title = "Distribution of Property Price by distance from Closest School",
       colour = "Distance from Closest School",
       subtitle = "properties within 10km from School") +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )


# Property Price by Violent Crime
df %>%
  group_by(postcode) %>%
  mutate(price = mean(price)) %>%
  slice(1) %>%
  ungroup() %>%
  ggplot(aes(x = Violent.Crime, y=price)) +
  geom_point(adjust = 2) +
  scale_y_continuous(labels = comma, limits=c(0, 5000000)) +
  labs(x = "Violent Crime",
       y = "Property Price",
       title = "Property Price by Violent Crime",
       subtitle = "Each data point represents one postcode"
  )