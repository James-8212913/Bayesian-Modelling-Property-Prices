library(jsonlite)
library(parallel)
library(stringr)
library(tidyverse)
library(plyr)


property_data <- read.csv("new_property_data.csv") %>%
  na.omit()

#87,113 properties onlynrow(property_data)
nrow(property_data)
#create address for iteration
data <- data.frame(index = property_data[,1], address = paste0(property_data$address, " Australia"))
#replace space with plus sign and remove coma
data$address <- str_replace_all(data$address, c(" " = "+", "," = ""))

#api key for google API access 
api_key = ""

base_url <- "https://maps.googleapis.com/maps/api/geocode/json?key=%s&address=%s"


geocode_data <- data.frame(
  property_id = integer(),
  subpremise	= character(),
  premise = character(),
  street_number = character(),
  route	= character(),
  locality	= character(),
  administrative_area_level_2	= character(),
  administrative_area_level_1	= character(),
  country	= character(),
  postal_code = character(),
  formatted_address = character(),
  lat = numeric(),
  lon = numeric()
)  

#change to 30001:60000 for first run
#change to 60001:nrow(data) for second run
sprintf(base_url, api_key, data$address[i])
for (i in 1:1){
  tryCatch({
    #request address and geolocation in JSON format using Google Geocode API
    temp1 <- data.frame(fromJSON(sprintf(base_url, api_key, data$address[i])))
    #select subpremise, stree_number, street_name, suburb, council, state, country, postal_code
    temp2 <- temp1$results.address_components[[1]]$long_name %>%
      t() %>%
      data.frame(stringsAsFactors = FALSE)
    colnames(temp2) <- unlist(lapply(temp1$results.address_components[[1]]$types, function(x) x[1]))
    temp2 <- temp2 %>%
      select(any_of(c("subpremise",
                      "premise",
                      "street_number" ,
                      "route",
                      "locality",
                      "administrative_area_level_2",
                      "administrative_area_level_1",
                      "country",
                      "postal_code")))
    #select formatted_address, lat and lon
    temp3 <- temp1[,-grep("results.address_components", names(temp1))] %>%
      unlist() %>%
      t() %>%
      data.frame(stringsAsFactors = FALSE) %>%
      select(results.formatted_address, results.geometry.location.lat, results.geometry.location.lng) 
    colnames(temp3) <- c("formatted_address", "lat", "lon")
    #join data
    geocode_data <- rbind.fill(geocode_data, cbind(data.frame(property_id = data[i, 1]), temp2, temp3))
  },
  error = function(e) {
    writeLines(sprintf("Error at index : %d, error : %s", i, e))
  }
  )
}

write.csv(geocode_data, "geocode_data.csv", row.names = FALSE)
