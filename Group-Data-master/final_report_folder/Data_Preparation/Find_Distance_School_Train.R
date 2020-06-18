library(geosphere)
library(tidyverse)

property_data <- read.csv("property_data_with_geocode.csv")%>%
  select(property_id,lon,lat)
station_data <- read.csv("./Distance From Train Station/[Data] NSW_Station_Entrances_Locations_2020.csv")
public_school_data <- read.csv("./Distance From Public School/[Data] NSW_public_schools_2016.csv")
private_school_data <- read.csv("./Distance From Non-Government School/[Data] NSW_non_government_school_2017.csv")

# ### Distance From CBD ###
# 
# # longtitude & latitude of CBD (2000 Sydney NSW) is 151.2093 E & 33.8688 S
# CBD_longtitude <- 151.2093
# CBD_latitude <- -33.8688
# 
# # function 'distm' returns distance matrix between two sets of points
# property_data$distance_from_CBD <- round(distm(cbind(property_data$longtitude, property_data$latitude), c(CBD_longtitude, CBD_latitude), fun = distGeo))/1000


### Distance From Closest Entrance of Train Station ###

temp <- round(distm(cbind(property_data$lon, property_data$lat), cbind(station_data$LONG, station_data$LAT), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  station_entrance_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_station[i] <- temp[i, station_entrance_number]
}

### Distance From Closest Public School ###

temp <- round(distm(cbind(property_data$lon, property_data$lat), cbind(public_school_data$Longitude, public_school_data$Latitude), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  public_school_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_public_school[i] <- temp[i, public_school_number]
}

### Distance From Closest Non-Government School ###

temp <- round(distm(cbind(property_data$lon, property_data$lat), cbind(private_school_data$longtitude, private_school_data$latitude), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  private_school_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_private_school[i] <- temp[i, private_school_number]
}

write.csv(total_crime_18_19, "[Output] Crime_Data_by_postcode_2018_19.csv", row.names = FALSE)
