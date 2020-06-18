library(tidyverse)
library(parallel)
library(rvest)
library(plyr)
library(ggmap)
library(jsonlite)

# This function will download the property html and save into local drive
download_html_files <- function(row_index) {
  out <- tryCatch({
    property <- property_data[row_index, ]
    property_website_url <- property$url
    
    downloaded_file_path <- file.path("property_html", sprintf("property_%s.html", row_index))
    
    download.file(url = property_website_url, destfile = downloaded_file_path)  
  },
  error = function(cond) {
    writeLines(sprintf("Error at index : %d, error : %s", row_index, cond))
    return(NA)
  })
}

no_of_cores <- detectCores() - 1

# create directory
dir.create("property_html")

raw_property_data <- read_csv(file.path("property_web_scraping", "new_property_data.csv"))
old_property_data <- read_csv(file.path("property.csv"))

head(raw_property_data, 5)

no_of_data <-nrow(raw_property_data)

remain_property_data <- c(1:no_of_data)

# download html files in parallel
results <- mclapply(remain_property_data, download_html_files, mc.cores = no_of_cores)


############## Get property data #################
scrap_data_from_html_file_threading <- function(row_index) {
  
  output_result <- NA
  
  no_of_bed <- NA
  no_of_bath <- NA
  no_of_parking <- NA
  house_size <- NA
  
  out <- tryCatch({
    property <- property_data[row_index, ]
    property_website_url <- property$property_link
    
    html_file_path <- file.path("property_html", sprintf("property_%s.html", row_index))
    html_data <- read_html(html_file_path)
    property_features <- html_data %>% html_nodes(xpath = '//div[@class="listing-details__listing-summary-features css-er59q5"]//span[@data-testid="property-features-text-container"]') %>% html_text()
    
    for (property_feature in property_features) {
      if (grepl("Bed", property_feature, fixed=TRUE)) {
        no_of_bed <- as.integer(str_replace(property_feature, " .*", ""))
      } else if (grepl("Bath", property_feature, fixed=TRUE)) {
        no_of_bath <- as.integer(str_replace(property_feature, " .*", ""))
      } else if (grepl("Parking", property_feature, fixed=TRUE)) {
        no_of_parking <- as.integer(str_replace(property_feature, " .*", ""))
      } else if (grepl("m²", property_feature, fixed=TRUE)) {
        house_size <- as.integer(str_replace(property_feature, "m² .*", ""))
      }
    }
    
    writeLines(sprintf("Index : %s, Beds: %s, Baths: %s, Parking: %s, Size: %s", row_index, no_of_bed, no_of_bath, no_of_parking, house_size))
    
  },
  error = function(cond) {
    writeLines(sprintf("Error at index : %d, error : %s", row_index, cond))
  },
  finally = {
    # writeLines(sprintf("Index : %s, Beds: %s, Baths: %s, Parking: %s, Size: %s", row_index, no_of_bed, no_of_bath, no_of_parking, house_size))
    
    output_result <- list("property_id" = row_index,
                          "no_of_bed" = no_of_bed,
                          "no_of_bath" = no_of_bath,
                          "no_of_parking" = no_of_parking,
                          "house_size" = house_size
    )
    
  }
  )
  
  return (output_result)
}

## grep bed, bath, parking, house size data from html file and store to scraped_property_data
rm(mc_testing)
rm(mc_matrix)
rm(mc_dataframe)
mc_testing <- mclapply(c(1:no_of_data), scrap_data_from_html_file_threading, mc.cores = no_of_cores)
mc_matrix <- t(matrix(unlist(mc_testing), nrow=5))
scraped_property_data <- as.data.frame(mc_matrix)
colnames(scraped_property_data) <- c("property_id", "no_of_bed", "no_of_bath", "no_of_parking", "house_size")

# Merge data 
property_data$no_of_bed <- scraped_property_data$no_of_bed
property_data$no_of_bath <- scraped_property_data$no_of_bath
property_data$no_of_parking <- scraped_property_data$no_of_parking
property_data$house_size <- scraped_property_data$house_size


#write.csv(property_data, "property_data_v2.csv", row.names=FALSE)


############ Get property additional features ################
scraped_property_additional_features <- function(row_index, scraped_additional_property_data) {
  
  additional_features <- NA
  
  out <- tryCatch({
    property <- property_data[row_index, ]
    property_website_url <- property$property_link
    
    html_file_path <- file.path("property_html", sprintf("property_%s.html", row_index))
    html_data <- read_html(html_file_path)
    property_additional_features <- html_data %>% html_nodes(xpath = '//li[@class="listing-details__additional-features-listing"]') %>% html_text()
    
    additional_features <- as.character(paste(property_additional_features, collapse = ","))
    
    
  },
  error = function(cond) {
    writeLines(sprintf("Error at index : %d, error : %s", row_index, cond))
  },
  finally = {
    
    writeLines(sprintf("Index : %s, features : %s", row_index, additional_features))
    
    scraped_additional_property_data <- append(scraped_additional_property_data, additional_features)
    
  }
  )
  
  return (scraped_additional_property_data)
}


# scraped_additional_property_data <- data.frame("property_id" = integer(),
#                                                "additional_features" = character(),
#                                                stringsAsFactors = FALSE)

# for loop the whole data set and scrap the additional features from html files
scraped_additional_vector <- c()

no_of_data
for (row_index in 1:no_of_data) {
  scraped_additional_vector <- scraped_property_additional_features(row_index, scraped_additional_vector)
}
scraped_additional_vector


# Try to combine all the additional fetures into one vector ( test_scraped_additional_vector ), then I can use this vector to find the unqiue additional features
nrow
test_scraped_additional_vector =c()
for (vector_index in 1:length(scraped_additional_vector)) {
  print(vector_index)
  test_property_features <- scraped_additional_vector[[vector_index]]
  test_features <- strsplit(test_property_features, ",")
  # writeLines(sprintf("Index : %s,  test_features : %s", vector_index, test_features))
  for (feature_index in 1:length(test_features)) {
    test_scraped_additional_vector <- append(test_scraped_additional_vector, test_features[[feature_index]])
  }
  
}


# print out the first 100 unqiue additions features
unique_features <- unique(test_scraped_additional_vector) 
unique_features %>% head(100)



# choose some features and convert them to dataframe ( additional_features_dataframe )
chosen_features <- c("Built in wardrobes", 
                     "Close to schools", 
                     "Close to shops", 
                     "Close to transport", 
                     "Secure Parking", 
                     "Balcony", 
                     "Air conditioning",
                     "Formal Lounge",
                     "Garden",
                     "North rear facing",
                     "Separate Dining",
                     "Security Alarm",
                     "Swimming Pool",
                     "Broadband",
                     "Dishwasher",
                     "Gas",
                     "Heating",
                     "Internal Laundry",
                     "Carpeted",
                     "Ocean Views",
                     "City Views",
                     "Gym"
                     )

additional_features_dataframe <- data.frame("property_id" = integer(),
                                              "built_in_wardrobes" = logical(),
                                              "close_to_schools" = logical(),
                                              "close_to_shops" = logical(),
                                              "close_to_transport" = logical(),
                                              "secure_parking" = logical(),
                                              "balcony" = logical(),
                                              "air_conditioning" = logical(),
                                              "formal_lounge" = logical(),
                                              "garden" = logical(),
                                              "north_rear_facing" = logical(),
                                              "separate_dining" = logical(),
                                              "security_alarm" = logical(),
                                              "swimming_pool" = logical(),
                                              "broadband" = logical(),
                                              "dishwasher" = logical(),
                                              "gas" = logical(),
                                              "heating" = logical(),
                                              "internal_laundry" = logical(),
                                              "carpeted" = logical(),
                                              "ocean_views" = logical(),
                                              "city_views" = logical(),
                                              "gym" = logical()
)
length(scraped_additional_vector)
for (vector_index in 1:length(scraped_additional_vector)) {
  print(vector_index)
  init_row_values <- list("property_id" = vector_index,
                          "built_in_wardrobes" = FALSE,
                          "close_to_schools" = FALSE,
                          "close_to_shops" = FALSE,
                          "close_to_transport" = FALSE,
                          "secure_parking" = FALSE,
                          "balcony" = FALSE,
                          "air_conditioning" = FALSE,
                          "formal_lounge" = FALSE,
                          "garden" = FALSE,
                          "north_rear_facing" = FALSE,
                          "separate_dining" = FALSE,
                          "security_alarm" = FALSE,
                          "swimming_pool" = FALSE,
                          "broadband" = FALSE,
                          "dishwasher" = FALSE,
                          "gas" = FALSE,
                          "heating" = FALSE,
                          "internal_laundry" = FALSE,
                          "carpeted" = FALSE,
                          "ocean_views" = FALSE,
                          "city_views" = FALSE,
                          "gym" = FALSE
                          )
  property_features <- scraped_additional_vector[[vector_index]]
  for (feature_index in 1:length(chosen_features)) {
    chosen_feature <- chosen_features[feature_index]
    if (grepl(chosen_feature, property_features, fixed=TRUE)) {
      init_row_values[[feature_index+1]] = TRUE
    }
    
    
  }
  additional_features_dataframe <- rbind(additional_features_dataframe, init_row_values)
}

# merge data to property_data price
new_property_data <- left_join(property_data, additional_features_dataframe[, c("property_id", "built_in_wardrobes","close_to_schools","close_to_shops","close_to_transport","secure_parking","balcony","air_conditioning","formal_lounge","garden","north_rear_facing","separate_dining","security_alarm","swimming_pool","broadband","dishwasher","gas","heating","internal_laundry","carpeted","ocean_views","city_views","gym")], 
                               by = c("X1" = "property_id"))

# Save dataframe to csv
write.csv(new_property_data, "property_data_v2.csv", row.names=FALSE)


########### Fix data in geo data ###########
property_geo_data = read_csv("geocode_data.csv")
GOOGLE_API_KEY <- "AIzaSyC_IsQdpmWH4aoaONr_drcVLe-6AlqiV_s"
register_google(key=GOOGLE_API_KEY, write=TRUE)
GEOCODING_API_TEMPLATE <- "https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s"
incorrect_property_geo_data <- property_geo_data[property_geo_data$country != "Australia", ] 

incorrect_property_geo_data


nrow(incorrect_property_geo_data)
new_geocode_data <- data.frame()
nrow(incorrect_property_geo_data)

for (row_index in 1:2) {
  single_property_geo_data <- incorrect_property_geo_data[row_index, ]
  print(single_property_geo_data$no)
  property <- property_data[property_data$X1 == single_property_geo_data$no, ]
  no_of_property <- nrow(property)
  writeLines(sprintf("No : %s, No. P : %s", single_property_geo_data$no, no_of_property))
  if (no_of_property == 1) {
    address <- sprintf("%s,%s,NSW,Australia", property$property_address, property$`rep.suburb..length.property_address..` )
    address <- str_replace_all(address, " ", "+")
    final_geocoding_api <- sprintf(GEOCODING_API_TEMPLATE, address, GOOGLE_API_KEY)
    writeLines(sprintf("%s : %s", property$X1, final_geocoding_api))
    geocoding_api_return_json <- fromJSON(final_geocoding_api)
    
    print(geocoding_api_return_json$results)
    geometry_data <- geocoding_api_return_json$results$geometry[1, ]  
    
    property$formatted_address <- geometry_data$formatted_address
    property$latitude <- geometry_data$location$lat
    property$longtitude <- geometry_data$location$lng
    
    new_property_data <- rbind(new_property_data, property)
    
  }
  
  
}


########### cleaning the data set ###########
property_data = read_csv("property_data_v2.csv")
property_geo_data = read_csv("geocode_data.csv")

TOTAL_NUM_OF_PROPERTY <- nrow(property_data)

unique(property_geo_data$country)
property_geo_data$no


new_property_data <- left_join(property_data, property_geo_data[, c("no", "locality", "administrative_area_level_2", "postal_code", "country", "results.geometry.location.lat", "results.geometry.location.lng")], by = c("X1" = "no"))


new_property_data$property_prices <- str_replace_all(new_property_data$property_prices, "\\$", "")
new_property_data$property_prices[new_property_data$property_prices == "Price withheld"] <- NA
new_property_data <- new_property_data[!(is.na(new_property_data$property_prices) | new_property_data$property_prices==""), ] # drop the row with NA price
new_property_data$property_prices <- str_replace_all(new_property_data$property_prices, " max bid", "")

# convert k into 000, m into 000000
new_property_data$property_prices <- ifelse(grepl("k", new_property_data$property_prices, fixed = TRUE), as.numeric(sub("k", "e3", new_property_data$property_prices, fixed = TRUE)), new_property_data$property_prices)
new_property_data$property_prices <- ifelse(grepl("m", new_property_data$property_prices, fixed = TRUE), as.numeric(sub("m", "e6", new_property_data$property_prices, fixed = TRUE)), new_property_data$property_prices)

new_property_data <- new_property_data[(new_property_data$country=="Australia" & !(is.na(new_property_data$country))), ] # drop the row with Country is not Australia
new_property_data <- new_property_data[!(is.na(new_property_data$results.geometry.location.lat)), ] # drop the row with lat value NA

colnames(new_property_data)

new_property_data_v2 <- new_property_data[, -c(3, 5, 7, 8, 36, 39)]
names(new_property_data_v2)[2] <- "date"
names(new_property_data_v2)[3] <- "suburb"
names(new_property_data_v2)[34] <- "latitude"
names(new_property_data_v2)[35] <- "longtitude"

write.csv(new_property_data_v2, "property_data_v3.csv", row.names=FALSE)


table(new_property_data_v2$postal_code)

sum(is.na(new_property_data_v2$house_size))

############ Testing code belows ##############
testing_data <- read_html("property_html/property_1.html")

testing_data

testing_data %>% html_nodes(xpath = "//html")

testing_data %>% html_nodes(xpath = '//div[@class="listing-details__listing-summary-features css-er59q5"]//span[@data-testid="property-features-text-container"]')

test_property_feature_node <- testing_data %>% html_nodes(xpath = '//div[@data-testid="property-features"]') %>% head(1)
    
test_property_features <- test_property_feature_node %>% html_nodes(xpath = '//span[@data-testid="property-features-text-container"]') %>% html_text()
test_property_features
property_features[1]
grepl("Beds", property_features[1], fixed=TRUE)
as.integer(str_replace(property_features[1], " .*", ""))

no_of_bed = NA
no_of_bath = NA
no_of_parking = NA
house_size = NA

for (property_feature in property_features) {
  if (grepl("Beds", property_feature, fixed=TRUE)) {
    no_of_bed =  as.integer(str_replace(property_feature, " .*", ""))
  } else if (grepl("Baths", property_feature, fixed=TRUE)) {
    no_of_bath =  as.integer(str_replace(property_feature, " .*", ""))
  } else if (grepl("Parking", property_feature, fixed=TRUE)) {
    no_of_parking =  as.integer(str_replace(property_feature, " .*", ""))
  } else if (grepl("m²", property_feature, fixed=TRUE)) {
    house_size = str_replace(property_feature, "m² .*", "")
  }
}

writeLines(sprintf("Beds: %s, Baths: %s, Parking: %s, Size: %s", no_of_bed, no_of_bath, no_of_parking, house_size))



test_property_features <- testing_data %>% html_nodes(xpath = '//li[@class="listing-details__additional-features-listing"]') %>% html_text()
test_property_features
test_string <- as.character(paste(test_property_features, collapse = ","))
str(test_string)
test_new_data <- list("property_id" = 1,
                 "additional_features" = test_string)
str(test_new_data)
