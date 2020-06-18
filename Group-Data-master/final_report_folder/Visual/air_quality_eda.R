library(tidyverse)
library(geosphere)
library(parallel)
library(ggmap)


# - Data preparation ########################
AIR_QUALITY_INDEX_NORM_FILE_PATH <- "Data/Air_Quality/air_quality_cleaned.csv"
GOOGLE_API_KEY = "AIzaSyC_IsQdpmWH4aoaONr_drcVLe-6AlqiV_s"
register_google(key=GOOGLE_API_KEY, write=TRUE)

air_quality_norm_data <- read_csv(AIR_QUALITY_INDEX_NORM_FILE_PATH) %>% na.omit()
air_quality_norm_data <- air_quality_norm_data %>% mutate(month = as.integer(substr(date, 4, 5)))

air_quality_norm_data_exclude <- air_quality_norm_data %>% filter(month >=2 & month <=10)

# group by location
grouped_air_quality_norm_data <- air_quality_norm_data %>% 
  dplyr::group_by(location) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))

grouped_air_quality_norm_data_excluded_months <- air_quality_norm_data %>% filter(month >=2 & month <=10) %>%
  dplyr::group_by(location) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))

mean(air_quality_norm_data[air_quality_norm_data$month==1 | air_quality_norm_data$month==11 | air_quality_norm_data$month==12, ]$AQI)
mean(air_quality_norm_data[air_quality_norm_data$month==12, ]$AQI)
mean(air_quality_norm_data[air_quality_norm_data$month==1, ]$AQI)
mean(air_quality_norm_data_exclude$AQI)

no_of_air_quality_stations <- nrow(grouped_air_quality_norm_data)

# Filter air station which close to CBD
grouped_air_quality_norm_data$close_to_cbd = FALSE
for (air_quality_row_index in 1:no_of_air_quality_stations) {
  # air_quality_station <- grouped_air_quality_norm_data[air_quality_row_index, ]
  air_quality_station <- grouped_air_quality_norm_data[air_quality_row_index, ]
  air_quality_station_geo <- c(air_quality_station$longitude, air_quality_station$latitude)
  distance <- distm(c(151.2093, -33.8688), air_quality_station_geo, fun = distHaversine)
  # writeLines(sprintf("Station : %s, distance : %s, current distance : %s", air_quality_station$location, distance, shortest_distance))
  if (distance <= 50000) {
    grouped_air_quality_norm_data[air_quality_row_index, ]$close_to_cbd = TRUE
  }
}


grouped_air_quality_norm_data_excluded_months$close_to_cbd = FALSE
for (air_quality_row_index in 1:no_of_air_quality_stations) {
  # air_quality_station <- grouped_air_quality_norm_data[air_quality_row_index, ]
  air_quality_station <- grouped_air_quality_norm_data_excluded_months[air_quality_row_index, ]
  air_quality_station_geo <- c(air_quality_station$longitude, air_quality_station$latitude)
  distance <- distm(c(151.2093, -33.8688), air_quality_station_geo, fun = distHaversine)
  # writeLines(sprintf("Station : %s, distance : %s, current distance : %s", air_quality_station$location, distance, shortest_distance))
  if (distance <= 50000) {
    grouped_air_quality_norm_data_excluded_months[air_quality_row_index, ]$close_to_cbd = TRUE
  }
}


# - EDA graphs ########################

# Monthly state ( group by month )
grouped_air_quality_by_month <- air_quality_norm_data %>% dplyr::group_by(month) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))
grouped_air_quality_by_month <- grouped_air_quality_by_month %>% mutate(month = as.factor(month))

grouped_air_quality_by_month %>% 
  ggplot() + 
  geom_col(mapping = aes(x = month, y = AQI), fill="blue") +
  ggtitle("Average monthly Air Quantity Index in 2018-Now")

# Air Quality Index in Sydney
gmap<-ggmap(get_googlemap(center=c(151.0180, -33.8688), maptype="hybrid", zoom=9))
gmap<-ggmap(get_googlemap("Sydney", maptype="hybrid", zoom=9))
cbd_air_quality <- grouped_air_quality_norm_data %>% filter(close_to_cbd==TRUE)
gmap + 
  geom_point(data=grouped_air_quality_norm_data, aes(x=longitude, y=latitude, size=100, colour=AQI), alpha=0.7) + 
  scale_colour_gradient2(low = "#00FF00", mid="#FF0000", high="#000000", midpoint = 80, limit=c(30, 120)) +
  scale_radius(range = c(7)) +
  guides(size = FALSE) + 
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Air Quality Index in Sydney")

  # Air Quality Index in Sydney excluded some months
cbd_air_quality_some_month <- grouped_air_quality_norm_data_excluded_months %>% filter(close_to_cbd==TRUE)
gmap + 
  geom_point(data=cbd_air_quality_some_month, aes(x=longitude, y=latitude, size=100, colour=AQI), alpha=0.5) + 
  scale_colour_gradient2(low = "#00FF00", mid="#FF0000", high="#000000", midpoint = 80, limit=c(30, 120)) +
  scale_radius(range = c(7)) +
  guides(size = FALSE) + 
  ggtitle("Air Quality Index in Sydney ( excluded bush fire months )")


# Air Quantity
new_grouped_air_quality_norm_data <- left_join(grouped_air_quality_norm_data, property_data[, c("AQI", "lga")], by = c("AQI" = "AQI"))

new_grouped_air_quality_norm_data <- left_join

air_quality_norm_data %>% filter()
  ggplot() +
  geom_point(mapping = aes(x = date, y = AQI, color = location));


