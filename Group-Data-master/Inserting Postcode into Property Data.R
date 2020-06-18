library(tidyverse)
library(dplyr) 

(df2 <- property_data_v2)
(df1 <- postcodes_geo_NSW_postcodes_geo_NSW)

colnames(df2)
colnames(df1)

#Rename  and Tidy
DF2 <- df2 %>% rename(Date = rep.date..length.property_address.., Suburb = rep.suburb..length.property_address..)
DF1 <- df1 %>% rename(Suburb = suburb, Postcode = postcode)

colnames(DF2)
colnames(DF1)

#Inser Postcodes into property data

Property_Data_Postcode <- DF2 %>% 
  full_join(DF1, by = "Suburb")