library(tidyverse)
aa <- read_csv("cleaned_merged_property_data.csv")
w <- aa %>%
  filter(year==2018)
t <- w %>%
  filter(lga  %in% c('Council of the City of Sydney','Shellharbour City Council',
                     'Bayside Council','City of Ryde'))
cat <- 
  cut(t$price, breaks = c(0,900000,2000000,5000000,15000000),
      labels = c("low","medium","high","very high")) 
ggplot(data = t) + 
  geom_bar(mapping = aes(x = lga, fill = cat, colour= rainbow))+ 
  ggtitle("2018")
ggplot(data = t) + 
  geom_bar(mapping = aes(x = lga, fill = cat), position = "dodge")+
  ggtitle("2018")

e<- aa %>%
  filter(year==2019)
s <- e %>%
  filter(lga  %in% c('Council of the City of Sydney','Shellharbour City Council',
                     'Bayside Council','City of Ryde'))
cat <- 
  cut(s$price, breaks = c(0,900000,2000000,5000000,15000000),
      labels = c("low","medium","high","very high")) 
ggplot(data = s) + 
  geom_bar(mapping = aes(x = lga, fill = cat))+
  ggtitle("2019")

ggplot(data = s) + 
  geom_bar(mapping = aes(x = lga, fill = cat), position = "dodge")+
  ggtitle("2019")

