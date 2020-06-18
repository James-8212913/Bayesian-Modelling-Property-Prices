library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

df <- read_csv("Cash_Rate.csv",
               col_names = c("Year", "Cash Rate"),
               skip = 11)

df_interest_data <- df %>% 
  mutate(Year = lubridate::dmy(Year)) %>% 
  drop_na()
df_interest_data


ggplot(df_interest_data, aes(x = Year, y = `Cash Rate`)) +
  geom_line() +
  labs(title = "RBA Target Cash Rate",
       x = "Year", 
       y = "Cash Rate",
       caption = "source: RBA")

xx