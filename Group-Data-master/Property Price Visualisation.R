library(scales)


### Create the Base Map to overlay the data onto

GSRBW <- get_map(location = c(left = 150.5, 
                             bottom = -34,
                             right = 151.5,
                             top = -33.9),
                zoom = 9,
                scale = "auto",
                maptype = c("terrain"),
                source = c("google"),
                messaging = FALSE,
                urlonly = FALSE,
                filename = NULL,
                crop = FALSE, 
                color = "bw")%>%
  ggmap()

### Different Maps Created

GSRBW 
GSR
GSRgooglebw
GSRgooglecolor
GSRBW 

glimpse(cleaned_merged_property_data)

### Basic Overview to see waht it looks like untouched

GSRBW + geom_point(data = cleaned_merged_property_data,
                   aes(x = lon, y = lat)) +
  labs(title = "Property Sale Locations", x = "Longitude", y = "Latitude") +
  scale_fill_discrete(name = "Title") 


### Average the Property Price by Post Code

(APPP <- cleaned_merged_property_data%>%
  select(postcode, lon, lat, price,type)%>%
  group_by(postcode) %>%
  summarise(
    n = n(),
    aveprice = mean(price, na.rm = TRUE)
  ))

view (APPP)

### Joint the postcode lat and lon to the set

APPPPC <- full_join(APPP, postcodes_geo_NSW_postcodes_geo_NSW, by = 'postcode')

glimpse(APPPPC)

### Graph for Average Price by Postcode

ggplot(APPP, aes(x = postcode, y = aveprice, size = n, color = aveprice)) +
  geom_point() +
  labs(title = "Average Property Price by Post Code", x = "Postcode", y = "Average Sale Price") +
  scale_color_continuous(name = "Average Prices") +
  scale_y_continuous(labels = dollar)

### Graph for Average Price by Postcode on a Map

GSRBW + geom_point(data = APPPPC,
                   aes(x = longitude, y = latitude,size = n, colour = aveprice, position = "jitter")) +
  scale_color_gradient(low = "light green", high = "blue",space = "blue", guide = "colourbar", aesthetics = "colour") +
  labs(title = "Average Property Price by Post Code 2018 - 2020", x = "Longitude", y = "Latitude") +
  scale_fill_continuous(name = "Average Sale Price", labels = dollar) 



  
   



