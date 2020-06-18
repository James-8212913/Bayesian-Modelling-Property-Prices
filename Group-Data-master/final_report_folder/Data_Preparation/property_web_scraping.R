library(tidyverse)
library(parallel)

PROPERTY_BASE_URL_FORMAT <- "https://www.domain.com.au/sold-listings/%s-nsw-%s/?excludepricewithheld=1&ssubs=0&page=%s"
postcode_list <- read_csv(file.path("property_web_scraping", "postcodes_geo_NSW.csv"))
no_of_cores <- detectCores() - 1

filtered_postcode_list <- postcode_list %>% filter(postcode_list$postcode >= 2000)

# substring last n character from a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# create directory
dir.create(file.path("property_web_scraping", "property_listing_html"))

################## download list html files ##################
download_property_listing_html_files <- function(row_index) {
  out <- tryCatch({
    
    page = 1
    
    while (TRUE) {
      
      postcode_data <- filtered_postcode_list[row_index, ]
      suburb <- str_replace_all(tolower(postcode_data$suburb), " ", "-")
      property_postcode_website_url <- sprintf(PROPERTY_BASE_URL_FORMAT, suburb, postcode_data$postcode, page)
      filename <- sprintf("property_link_%s_%s_%s.html", suburb, postcode_data$postcode, page)
      downloaded_file_path <- file.path("property_web_scraping", "property_listing_html", filename)
      
      download.file(url = property_postcode_website_url, destfile = downloaded_file_path)  
      
      html_data <- read_html(downloaded_file_path)
      sold_date <- html_data %>% html_nodes(xpath = '//span[@class="css-1nj9ymt"]') %>% html_text()
      sold_year <- as.integer(substrRight(sold_date[length(sold_date)], 4))
      
      # if last item is older than 2018, stop looping
      if (sold_year>=2018) {
        page = page + 1
      } else {
        break;
      }
      
    }
    
    
    
  },
  error = function(cond) {
    writeLines(sprintf("Error at index : %d, error : %s", row_index, cond))
    return(NA)
  })
}

no_of_data <-nrow(filtered_postcode_list)

remain_property_data <- c(1:no_of_data)

# download html files in parallel
results <- mclapply(remain_property_data, download_property_listing_html_files, mc.cores = no_of_cores)


################## scraping data from list html files ##################
scrape_data_from_list_file <- function(row_index) {
  
  property_data <- data.frame("date" = as.Date(character()),
                                "price" = integer(),
                                "address" = character(),
                                "suburb" = character(),
                                "postcode" = integer(),
                                "url" = character(),
                                "no_of_bed" = integer(), 
                                "no_of_bath" = integer(),
                                "no_of_parking" = integer(),
                                "house_size" = integer(),
                                "type" = character()
                              )
  
  out <- tryCatch({
    
    page = 1
    
    while (TRUE) {
      
      postcode_data <- filtered_postcode_list[row_index, ]
      suburb <- str_replace_all(tolower(postcode_data$suburb), " ", "-")
      filename <- sprintf("property_link_%s_%s_%s.html", suburb, postcode_data$postcode, page)
      list_file_path <- file.path("property_web_scraping", "property_listing_html", filename)
      html_data <- read_html(list_file_path)
      
      writeLines(sprintf("Process file : %s", list_file_path))
      
      all_sold_dates <- html_data %>% html_nodes(xpath = '//span[@class="css-1nj9ymt"]') %>% html_text()
      last_property_sold_year <- as.integer(substrRight(all_sold_dates[length(all_sold_dates)], 4))
      
      property_card_nodes <- html_data %>% html_nodes(xpath = '//div[@class="css-1kk6519"]')
      
      for (card_index in 1:length(property_card_nodes)) {
        
        # initial variables
        property_sold_date <- NA
        property_price <- NA
        property_address <- NA
        property_link <- NA
        no_of_bed <- NA
        no_of_bath <- NA
        no_of_parking <- NA
        property_type <- NA
        house_size <- NA
        
        property_card_node <- property_card_nodes[card_index]
        
        # get property sold date
        property_sold_date <- property_card_node %>% html_nodes(xpath = './/span[@class="css-1nj9ymt"]') %>% html_text()
        property_sold_date <- substrRight(property_sold_date, 11)
        property_sold_date <- as.Date(property_sold_date, format="%d %b %Y")
        property_sold_date <- as.character(property_sold_date)
        
        
        
        
        # get property sold price
        property_price <- property_card_node %>% html_nodes(xpath = './/p[@data-testid="listing-card-price"]') %>% html_text()
        property_price <- str_split_fixed(property_price, " ", n = Inf)[1]
        property_price <- str_remove_all(property_price, ",")
        property_price <- sub(".", "", property_price)
        
        # get property address
        property_address <- property_card_node %>% html_nodes(xpath = './/meta') %>% html_attr("content")
        
        # get property link
        property_link <- property_card_node %>% html_nodes(xpath = './/link') %>% html_attr("href")
        
        # get property bed, bath, parking info
        property_features <- property_card_node %>% html_nodes(xpath = './/span[@data-testid="property-features-text-container"]') %>% html_text()
        property_features
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
        
        # get property type
        property_type <- property_card_node %>% html_nodes(xpath = './/span[@class="css-693528"]') %>% html_text()
        
        
        property <- data.frame("date" = property_sold_date,
                         "price" = property_price,
                         "address" = property_address,
                         "suburb" = suburb,
                         "postcode" = postcode_data$postcode,
                         "url" = property_link,
                         "no_of_bed" = no_of_bed,
                         "no_of_bath" = no_of_bath,
                         "no_of_parking" = no_of_parking,
                         "house_size" = house_size,
                         "type" = property_type
                         
        )
        print(property)
        property_data <- rbind(property_data, property)
        
        # if last item is older than 2018, stop looping
        
      }
    
      print(last_property_sold_year)
      if (last_property_sold_year>=2018) {
        page = page + 1
      } else {
        break;
      }
      
    }
      
      
    
    
  },
  error = function(cond) {
    writeLines(sprintf("Error at index : %d, error : %s", row_index, cond))
    # return(property_data)
  },
  finally = {
    
    
    return (property_data)
    
  }
  )
  
}


no_of_data <-nrow(filtered_postcode_list)



# download html files in parallel
all_property_data <- data.frame("date" = as.Date(character()),
                                "price" = integer(),
                                "address" = character(),
                                "suburb" = character(),
                                "postcode" = integer(),
                                "url" = character(),
                                "no_of_bed" = integer(), 
                                "no_of_bath" = integer(),
                                "no_of_parking" = integer(),
                                "type" = character()
)

remain_property_data <- c(1:no_of_data)
results <- mclapply(remain_property_data, scrape_data_from_list_file, mc.cores = no_of_cores)
all_property_data <- ldply(results)

# keep the prooperty which sold data is later than 2018
filtered_all_property_data <- all_property_data %>% filter(date >= "2018-01-01")
filtered_all_property_data$suburb <- str_to_title(filtered_all_property_data$suburb)

write.csv(filtered_all_property_data, file.path("property_web_scraping", "new_property_data.csv"), row.names=TRUE)


new_property_data <- read_csv(file.path("property_web_scraping", "new_property_data.csv"))


