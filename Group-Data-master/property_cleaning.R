library(tidyverse)

merged_property_data <- read_csv("merged_property_data.csv")
merged_property_data$type = as.factor(merged_property_data$type)

# filter the property type less than 100
summary(merged_property_data$type)



clearn_merged_property_data <- merged_property_data %>% filter(type %in% c("Apartment / Unit / Flat", 
                                                                         "Duplex", 
                                                                         "House", 
                                                                         "Semi-Detached", 
                                                                         "New House & Land",
                                                                         "Terrace",
                                                                         "Townhouse",
                                                                         "Villa"),
                                                               distance_from_CBD <= 100,
                                                               price >= 10000,
                                                               no_of_bed <= 10,
                                                               no_of_bath <= 8,
                                                               no_of_parking <= 7,
                                                               house_size >= 20
                                                               )


clearn_merged_property_data <- clearn_merged_property_data %>% drop_na(median_taxable_income, mean_taxable_income)

write.csv(clearn_merged_property_data, "cleaned_merged_property_data.csv")





clearn_merged_property_data$type <- as.factor(clearn_merged_property_data$type)
clearn_merged_property_data$suburb <- as.factor(clearn_merged_property_data$suburb)

clearn_merged_property_data <- clearn_merged_property_data %>% filter(suburb %notin% c(""))

summary(clearn_merged_property_data)


nrow(clearn_merged_property_data)
nrow(clearn_merged_property_data %>% filter(distance_from_CBD <= 100))

summary(clearn_merged_property_data$price)
View(clearn_merged_property_data)

summary(clearn_merged_property_data$suburb)
