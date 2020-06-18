library(tidyverse)
library(geosphere)
library(parallel)
library(ggmap)
#library(dplyr)



######## initialize varibales ########
AIR_QUALITY_INDEX_NORM_FILE_PATH <- "air_quality/AQI_site_index_20180101_normal.csv"
AIR_QUALITY_MONITORING_SITE_FILE_PATH <- "air_quality/air-quality-monitoring-sites-summary.csv"
PROPERTY_DATA_FILE_PATH <- "air_quality/property_data_with_geocode.csv"
NO_OF_CORES <- detectCores() - 1
register_google(key="AIzaSyC_IsQdpmWH4aoaONr_drcVLe-6AlqiV_s", write=TRUE)



######## preparing AQI data ########
air_quality_raw_data = read_csv("air_quality/AQI_site_index_20180101.csv")
air_quality_norm_data <- air_quality_raw_data %>% gather("location", "AQI", -1)
air_quality_norm_data$location <- str_to_title(air_quality_norm_data$location)
write_csv(air_quality_norm_data, AIR_QUALITY_INDEX_NORM_FILE_PATH)

air_quality_norm_data <- read_csv(AIR_QUALITY_INDEX_NORM_FILE_PATH)
air_quality_monitoring_site_data <-  read_csv(AIR_QUALITY_MONITORING_SITE_FILE_PATH)
geocode_data <- read_csv("geocode_data.csv")
australia_geocode_data <- geocode_data %>% filter(country == "Australia")

air_quality_location_list <- unique(air_quality_norm_data$location)
air_quality_location_list
unique(air_quality_monitoring_site_data$`NSW air quality monitoring (AQMN) site`)

# find out all the missing Air quality station location name
for (air_quality_location in air_quality_location_list) {
  has_location = FALSE
  for (local_name in unique(air_quality_monitoring_site_data$`NSW air quality monitoring (AQMN) site`)) {
    if (identical(air_quality_location, local_name)) {
      has_location = TRUE
      break
    }
  }
  
  if (!has_location) {
    writeLines(sprintf("Missing Air Quality Location : ~%s~", air_quality_location))
  }
}



# create convertion list manually
air_quality_location_name_convertion_list <- list("Sydney Central-East" = "Cook and Phillip",
                                                  "Sydney North-West" = "Richmond",
                                                  "Albion Park Sth" = "Albion Park South",
                                                  "Central Tablelands" = "Bathurst",
                                                  "Wagga Wagga Nth" = "Wagga Wagga North",
                                                  "North-West Slopes" = "Tamworth",
                                                  "Singleton Nw" = "Singleton NW",
                                                  "Muswellbrook Nw" = "Muswellbrook NW",
                                                  "Northern Tablelands" = "Armidale",
                                                  "Roadsite Monitoring" = "Bradfield Highway",
                                                  "Southern Tablelands" = "Goulburn",
                                                  "Emergency Monitoring - Lismore" = "Lismore",
                                                  "Mid-North Coast" = "Port Macquarie",
                                                  "Liverpool Swaqs" = "Liverpool SWAQS"
                                                  )



for (key in names(air_quality_location_name_convertion_list)) {
  value = air_quality_location_name_convertion_list[[key]]
  air_quality_norm_data$location[air_quality_norm_data$location == key] = value
}

air_quality_location_list <- unique(air_quality_norm_data$location)
air_quality_location_list

# save the data again
write_csv(air_quality_norm_data, AIR_QUALITY_INDEX_NORM_FILE_PATH)

# joining the data to get the postcode, lat and long
air_quality_norm_data <- left_join(air_quality_norm_data, air_quality_monitoring_site_data[, c("NSW air quality monitoring (AQMN) site", "Latitude\r\n(South)", "Longitude\r\n(East)")], by = c("location" = "NSW air quality monitoring (AQMN) site"))

names(air_quality_norm_data)[1] = "date"
names(air_quality_norm_data)[2] = "location"
names(air_quality_norm_data)[3] = "AQI"
names(air_quality_norm_data)[4] = "latitude"
names(air_quality_norm_data)[5] = "longitude"

write_csv(air_quality_norm_data, AIR_QUALITY_INDEX_NORM_FILE_PATH)








######## getting property data ########
air_quality_norm_data <- read_csv(AIR_QUALITY_INDEX_NORM_FILE_PATH) %>% na.omit()
air_quality_monitoring_site_data <-  read_csv(AIR_QUALITY_MONITORING_SITE_FILE_PATH)
property_data <- read_csv(PROPERTY_DATA_FILE_PATH)

str(air_quality_norm_data)
air_quality_norm_data <- air_quality_norm_data %>% mutate(location = as.factor(location))
air_quality_norm_data <- air_quality_norm_data %>% mutate(month = as.integer(substr(date, 4, 5)))
# group by location
grouped_air_quality_norm_data <- air_quality_norm_data %>% 
  dplyr::group_by(location) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))

grouped_air_quality_norm_data_in_some_months <- air_quality_norm_data %>% filter(month >=3 & month <=9) %>%
  dplyr::group_by(location) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))
grouped_air_quality_norm_data_in_some_months

# group by month
grouped_air_quality_by_month <- air_quality_norm_data %>% dplyr::group_by(month) %>% 
  summarise(AQI = mean(AQI, na.rm = TRUE), latitude = min(latitude), longitude = min(longitude))
grouped_air_quality_by_month <- grouped_air_quality_by_month %>% mutate(month = as.factor(month))


no_of_air_quality_stations <- nrow(grouped_air_quality_norm_data)

### calculate distance
calculate_nearest_station <- function (row_index) {
  
  property <- property_data[row_index, ]
  property_geo_data <- c(property$lon, property$lat)
  
  shortest_distance <- 999999999
  nearest_air_quality_station_location <- NA
  nearest_air_quality_index <- NA
  
  for (air_quality_row_index in 1:no_of_air_quality_stations) {
    # air_quality_station <- grouped_air_quality_norm_data[air_quality_row_index, ]
    air_quality_station <- grouped_air_quality_norm_data[air_quality_row_index, ]
    air_quality_station_geo <- c(air_quality_station$longitude, air_quality_station$latitude)
    distance <- distm(property_geo_data, air_quality_station_geo, fun = distHaversine)
    # writeLines(sprintf("Station : %s, distance : %s, current distance : %s", air_quality_station$location, distance, shortest_distance))
    if (distance < shortest_distance) {
      nearest_air_quality_station_location <- air_quality_station$location
      nearest_air_quality_index <- air_quality_station$AQI
      shortest_distance <- distance
    }
        
  }
  
  output <- data.frame("property_id" = property$property_id, 
                       "nearest_air_quality_station_location" = nearest_air_quality_station_location, 
                       "shortest_distance" = shortest_distance,
                       "nearest_air_quality_index" = nearest_air_quality_index
                       )
  
  return (output)
  
}

distm(c(lon1, lat1), c(151.2093, -33.8688), fun = distHaversine)

for (i in c(1:2)) {
  print(calculate_nearest_station(i))
}

remain_property_data <- c(1:nrow(property_data))
results <- mclapply(remain_property_data, calculate_nearest_station, mc.cores = NO_OF_CORES)
all_location_air_quality <- plyr::ldply(results)
all_location_air_quality_in_some_month <- plyr::ldply(results)

all_location_air_quality
all_location_air_quality_in_some_month

colnames(all_location_air_quality)[4] <- "AQI"
colnames(all_location_air_quality_in_some_month)[4] <- "AQI_exclude_bushfire_month"

all_location_air_quality$AQI_exclude_bushfire_month <- all_location_air_quality_in_some_month$AQI_exclude_bushfire_month

write.csv(all_location_air_quality, "air_quality/property_air_quality.csv")


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




######## EDA #####
map <- get_googlemap(center=c(151.2093, -33.8688), maptype="roadmap", zoom=5)
gmap<-ggmap(get_googlemap("Sydney", maptype="hybrid", zoom=10))
gmap<-ggmap(get_googlemap(center=c(151.0180, -33.8688), maptype="hybrid", zoom=10))

cbd_air_quality <- grouped_air_quality_norm_data %>% filter(close_to_cbd==TRUE)
cbd_air_quality_some_month <- grouped_air_quality_norm_data_in_some_months %>% filter(close_to_cbd==TRUE)

# point
gmap + 
  geom_point(data=cbd_air_quality, aes(x=longitude, y=latitude, size=100, colour=AQI), alpha=0.5) + 
  scale_colour_gradient2(low = "#00FF00", mid="#FF0000", high="#000000", midpoint = 80, limit=c(40, 120)) +
  scale_radius(range = c(20)) +
  guides(size = FALSE) + 
  ggtitle("Air Quality Index in Sydney")

gmap + 
  geom_point(data=cbd_air_quality_some_month, aes(x=longitude, y=latitude, size=100, colour=AQI), alpha=0.5) + 
  scale_colour_gradient2(low = "#00FF00", mid="#FF0000", high="#000000", midpoint = 80, limit=c(40, 120)) +
  scale_radius(range = c(20)) +
  guides(size = FALSE) + 
  ggtitle("Air Quality Index in Sydney ( excluded bush fire months )")

# heat
gmap + 
  stat_density2d(data=expanded_cbd_air_quality, aes(x=longitude, y=latitude, fill = ..level.., alpha = ..level..), geom = "polygon", size = 0.01, bins = 16) +
  scale_fill_gradient(low = "green", high = "red") +
  ggtitle("Air Quality Index in NSW")


cbd_air_quality <- grouped_air_quality_norm_data %>% filter(close_to_cbd==TRUE)
gmap + 
  geom_contour_filled(data=cbd_air_quality, aes(x=longitude, y=latitude, z=AQI), alpha = 0.6) +
  ggtitle("Air Quality Index in NSW")


gmap + 
  geom_contour(data=cbd_air_quality, aes(x=longitude, y=latitude, z=AQI), alpha = 0.6) +
  ggtitle("Air Quality Index in NSW")


# expand data
expanded_cbd_air_quality <- data.frame()
for (i in 1:nrow(cbd_air_quality)) {
  row_data <- cbd_air_quality[i, ]
  AQI <- round(row_data$AQI)
  for (j in 1:AQI) {
    expanded_cbd_air_quality <- rbind(expanded_cbd_air_quality, row_data)    
  }
}



library(plotly)
library(reshape2)
test_df <- melt(volcano)
cbd_air_quality
test_df
test_df[4:90, ]
test_df_2 <- data.frame()
test_df_2 <- rbind(test_df_2, data.frame(Var1 = 1, Var2 = 2, value = 93.2))
test_df_2 <- rbind(test_df_2, data.frame(Var1 = 2, Var2 = 3, value = 80))
test_df_2 <- rbind(test_df_2, data.frame(Var1 = 3, Var2 = 1, value = 100))
test_df_2
ggplot(test_df[4:90, ], aes(Var1, Var2, z= value)) +
  geom_contour() +
  scale_fill_distiller(palette = "Spectral", direction = -1)


# Monthly state
grouped_air_quality_by_month %>% 
  ggplot() + 
  geom_line(mapping = aes(x = month, y = AQI, group=1)) +
  ggtitle("Average Air Quantity Index per month in year 2018-2019 ( Lower is better )")

