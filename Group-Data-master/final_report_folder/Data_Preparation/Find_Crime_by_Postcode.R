# The following code outpus total crime cases between Jan.2018 and Dec.2019 by postcode in NSW

library(tidyverse)
library(dplyr)

crime_data <- read.csv("[Data] Crime_Data_by_postcode.csv")
crime_type <- read.csv("[Mapping] Crime_Type.csv")

# 1. unselect columns (from column Jan.2010 to column Dec.2017) to extract dataset from 2018 to 2019
# 2. gather the non-variable columns (from column Jan.2018 to column Dec.2019) into a two-column key-value pair (month_year, number_of_case)
# 3. join with crime_type data with common variables (Offence.category, subcategory)
# 4. group data and summarise by Postcode and Crime.Type to give total number of crime cases
total_crime_18_19 <- crime_data %>%
  select(-(Jan.10:Dec.17)) %>%
  gather(month_year, number_of_case, Jan.18:Dec.19, na.rm = TRUE) %>%
  left_join(crime_type, c("Offence.category", "Subcategory")) %>%
  select(Postcode, Crime.Type, Offence.category, Subcategory, month_year, number_of_case) %>%
  group_by(Postcode, Crime.Type) %>%
  summarise(Total.Crime = sum(number_of_case)) %>%
  spread(Crime.Type, Total.Crime) %>%
  arrange(Postcode)

write.csv(total_crime_18_19, "[Output] Crime_Data_by_postcode_2018_19.csv", row.names = FALSE)