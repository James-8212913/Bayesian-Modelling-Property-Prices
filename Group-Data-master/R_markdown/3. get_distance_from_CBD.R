library(geosphere)
library(tidyverse)
#latitude & longitude of 2000 Sydney NSW is 33.8688° S, 151.2093° E 		
distance_km <- function(lon1, lat1){
  distm(c(lon1, lat1), c(151.2093, -33.8688), fun = distGeo)/1000
}

data <- read.csv("geocode_data.csv")
#select row_index, latitude and longitude
data <- data[, c(1, (ncol(data)-1):ncol(data))]
names(data)
names(data) <- c("no", "lat1", "lon1")

#calculate distance for across all properties
distance_from_CBD_km <- apply(data, 1, function(x) distance_km(x["lon1"], x["lat1"]))
data <- data[,c(1,4)]
data <- cbind(data, distance_from_CBD_km = distance_from_CBD_km)

write.csv(data, "distance_from_CBD.csv", row.names=FALSE)
