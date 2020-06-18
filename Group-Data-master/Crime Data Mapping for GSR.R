
## Mapping Porperty Data in the Greater Sydney Region
library(tidyverse)
library(tmap)
library(tmaptools)
library(ggmap)
library(ggplot2)
library(magrittr)
library(maps)
library(mapdata)
library(RColorBrewer)
library(ggthemes)
library(dplyr)
library(ozmaps)
library(viridis)
library(cartography)
devtools::install_github("dkahle/ggmap")

register_google(key = "AIzaSyBG4gBCaLSFrsJ-b5K0mrJ7GdVvOIdxorQ")

get_googlemap('Sydney', zoom = 9, maptype="hybrid") %>% 
  ggmap() 

## Generate the base MAP for Greater Sydney Region

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

## Maps Generated for the GSR

GSR
GSRgooglebw
GSRgooglecolor
GSRBW 
Sydney 

## Crime Data Plotting - locations of where crimes have occured - basically the post codes in the data set

GSRBW + geom_point(aes(x = longitude, y = latitude, colour = "red"), data = Crime_Data_V3.1,alpha=.5, size = 1) +
  theme(legend.position = "bottom")

## Summarise the Crime data in 2d

GSRgooglebw + geom_point(data = Crime_Data_V3.1,
                          aes(x = longitude, y = latitude, size = violent_crime, colour = violent_crime)) +
  scale_color_viridis(alpha = 1, option = "inferno")


## Another Option 

GSRBW + geom_point(data = Crime_Data_V3.1,
                   aes(x = longitude, y = latitude,size = violent_crime, colour = violent_crime, position = "jitter")) +
  scale_color_gradient(low = "white", high = "firebrick",space = "Lab", guide = "colourbar", aesthetics = "colour") +
  labs(title = "Violent Crime Incidents in Region 2018 - 2020", x = "Longitude", y = "Latitude") +
  scale_fill_discrete(name = "Title") 


