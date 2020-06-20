library(tidyverse)
library(lubridate)
library(magrittr)

view(cleaned_merged_property_data_final)

df <- cleaned_merged_property_data_final%>%
  mutate(
    date = as.Date(date, "%d/%m/%y")),
    type = as.factor(type),
    Within_5km_2019 = as.factor(Within_5km_2019),
    Btwn_5_10km_2019 = as.factor(Btwn_5_10km_2019),
    Btwn_10_15km_2019 = as.factor(Btwn_10_15km_2019),
    Within_5km_2018 = as.factor(Within_5km_2018),
    Btwn_5_10km_2018 = as.factor(Btwn_5_10km_2018),
    Btwn_10_15km_2018 = as.factor(Btwn_10_15km_2018),
)

## Remove outliers

boxplot(df$price)

priceoutliers <- boxplot(df$price)$out

df <- df[-which(df$price %in% priceoutliers),]

boxplot(df$price)

## Plot distribution of prices

ggplot(df) +
  geom_histogram(aes(y = price)) +
  coord_flip()

summary(df)
summary(cleaned_merged_property_data_final)




