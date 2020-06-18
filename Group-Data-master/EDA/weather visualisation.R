# Monthly average temperature - 10 weather stations
library(ggplot2)
library(ggmap)

weather<-read.csv("MonthlyAverageTemp.csv")
str(weather)

# visualisation 1 - temperature average by date
ggplot(weather, aes(x=date, y=temp_avg))+
  geom_boxplot(aes(color=date), width=0.8,
               outlier.size=1, outlier.shape=16, outlier.colour="red")+
  ggtitle("Plot of temperature average by date")

# visualisation 2 - temperature average by weather station
weather$code<-as.factor(weather$code)
ggplot(weather, aes(x=code, y=temp_avg))+
  geom_boxplot(aes(color=name), width=0.8,
               outlier.size=1, outlier.shape=16, outlier.colour="red")+
  ggtitle("Plot of temperature average by weather station (code)")

# visualisation 3 - weather stations location
cen<-c(mean(weather$lon), mean(weather$lat))
gc<-data.frame(lon=weather$lon, lat=weather$lat)
gc
map<-get_googlemap(center=cen, maptype="roadmap", zoom=10, marker=gc)
gmap<-ggmap(map)
gmap+geom_text(data=weather,
               aes(x=lon, y=lat),
               size=3,
               label=weather$code)+
  ggtitle("Weather stations (code) located within 50km of Sydney CBD")
