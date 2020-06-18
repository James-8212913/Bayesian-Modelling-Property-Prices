library(tidyverse)
library(rvest)

data <- read_csv("property.csv") %>%
  subset(property_prices != "Price withheld" )
  

get_property_value <- function(node){
  node %>%
    html_node(xpath = 'text()') %>%
    html_text()
} 

get_property_name <- function(node){
  node %>%
    html_node(xpath = 'span/text()') %>%
    html_text() %>%
    tolower()
} 

property_data <- data.frame()


# for (i in 1:20){
  temp <- read_html(data$property_link[11]) 
  
  nodes <- temp %>%
    html_nodes(xpath = '//*[@id="__domain_group/APP_ROOT"]/div/div/div[5]/div/div/div[1]/div[1]/div/div[3]/div/span/span')
  
  price = address = type = bed = bath = parking = size = NA
  
  for (node in nodes){
    if (is.na(get_property_name(node))){
      if (endsWith(get_property_value(node),"m²")) size = get_property_value(node)
    } else if (startsWith(get_property_name(node), "bed")){
      bed = get_property_value(node)
    } else if (startsWith(get_property_name(node), "bath")){
        bath = get_property_value(node)
    } else if (startsWith(get_property_name(node), "parking")){
        parking = get_property_value(node)
    } 
    price <- temp %>%
      html_node(xpath = '//*[@id="__domain_group/APP_ROOT"]/div/div/div[5]/div/div/div[1]/div[1]/div/div[1]/div[1]/div') %>%
      html_text()
    address <- temp %>%
      html_node(xpath = '//h1[@class="listing-details__listing-summary-address"]') %>%
      html_text()
    type = temp %>%
      html_node(xpath = '//div[@class="listing-details__property-type-features"]') %>%
      html_text()
   
  }
  property_data <- rbind(property_data, data.frame(address, type, price, bed, bath, parking, size))
  Sys.sleep(runif(1, 1, 2))
# }
      











