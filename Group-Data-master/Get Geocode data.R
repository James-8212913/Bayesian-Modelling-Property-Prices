library(jsonlite)
library(stringr)
library(tidyverse)
library(dplyr)
library(plyr)

data <- read.csv("property.csv", stringsAsFactors = FALSE)

summary(data)


#create address for interation
address <- paste(data$property_address, data$suburb, data$post_code, sep=",+")
#replace space with plus sign
address <- str_replace_all(address, " ", "+")
head(address)

#api key for google API access
api_key = ""

geocode_data <- read.csv("geocode_data.csv", stringsAsFactors = FALSE)

base_url <- paste("https://maps.googleapis.com/maps/api/geocode/json?key=", api_key, sep="")


for (i in 1:nrow(data)){
  if (i %in% unique(geocode_data$no)){
  } else {
    temp1 <- data.frame(fromJSON(paste(base_url, "&address=", address[i], sep="")))
    temp1$results.address_components[[1]]
    temp2 <- t(data.frame(temp1$results.address_components[[1]]$long_name))
    colnames(temp2) <- unlist(lapply(temp1$results.address_components[[1]]$types, function(x) x[1]))
    geocode_data <- rbind.fill(
      geocode_data, data.frame(
        cbind(
          data.frame(no=i), 
          temp2, 
          t(
            data.frame(
              unlist(temp1[,-grep("results.address_components", names(temp1))]
              )
            )
          )
        )
      )
    )
  }
}

write.csv(geocode_data, "geocode_data.csv", row.names = FALSE)

# a1 <- t(data.frame(x$results.address_components[[1]]$long_name))
# colnames(a1) <- unlist(lapply(x$results.address_components[[1]]$types, function(x) x[1]))         
