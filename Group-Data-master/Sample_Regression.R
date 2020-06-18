library(tidyverse)

unique(property_data$type)
property_data %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(count)



property_data <- property_data %>%
  filter(!(type %in% c("Development Site",
                       "Unknown", 
                       "New land",
                       "Farm",
                       "Cropping Property",
                       "Farmlet",
                       "vacant land",
                       "Car Space",
                       "Livestock Property",
                       "Horticulture Property",
                       "Retirement Living",
                       "Mixed Farming Property",
                       "New Home Designs",
                       "Specialist Farm",
                       "Rural Lifestyle Property",
                       "Penthouse",
                       "Block of Units",
                       "Acreage / Semi-Rural",
                       "Studio",
                       "Rural"))) %>%
  na.omit()

property_data %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(count)

taxable_income_data <- read.csv("Taxable Income 2016-2017.csv")
names(taxable_income_data) = c("state",
                               "postcode",
                               "no_of_individuals",
                               "median_taxable_income", 
                               "mean_taxable_income")
taxable_income_data %>%
  select(c(postcode, median_taxable_income, mean_taxable_income))

property_data <- merge(property_data, taxable_income_data, by="postcode", all.x=TRUE) %>%
  filter(price < 10000000)

str(taxable_income_data)
names(property_data)
property_data.lm <- lm(formula = price ~ factor(no_of_bed) + 
                         factor(no_of_bath) + 
                         type + 
                         mean_taxable_income +
                         sqrt(house_size) +
                         postcode
                         ,
                       data = property_data) 
summary(property_data.lm)
RMSE = function(x){
  sqrt(mean((x[,1] - x[,2])^2))
}

temp <- data.frame(actual = property_data$price,
                   estimate = predict(property_data.lm, newdata = property_data,na.action =na.pass)) %>%
  na.omit
