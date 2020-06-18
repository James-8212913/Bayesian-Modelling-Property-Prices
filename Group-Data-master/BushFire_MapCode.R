gmap<-ggmap(get_googlemap(center=c(151.0180, -33.8688), maptype="hybrid", zoom=10))
property_data <- fread("cleaned_merged_property_data.csv") 

property_data$BushFire <- 
  case_when (
    property_data$Within_5km_2019=="Y" ~ "A. < 5km",
    property_data$Within_5km_2019=="N" & property_data$Btwn_5_10km_2019=="Y" ~ "B. 5-10 km",
    property_data$Within_5km_2019=="N" & property_data$Btwn_5_10km_2019=="N" & property_data$Btwn_10_15km_2019=="Y" ~ "C. 10-15 km",
    TRUE ~ "D. >15 km"
  )

property_data$group <- 
  case_when (
    property_data$Within_5km_2019=="Y" ~ 1,
    property_data$Within_5km_2019=="N" & property_data$Btwn_5_10km_2019=="Y" ~ 2,
    property_data$Within_5km_2019=="N" & property_data$Btwn_5_10km_2019=="N" & property_data$Btwn_10_15km_2019=="Y" ~3,
    TRUE ~ 4
  )

pdata <- property_data %>% 
    filter(type=="House" & !is.na(lga) ) %>%
    group_by(lga,BushFire,group) %>%
summarise(TotalProperties=n(),
          lat=min(lat),
          lon=min(lon)
) 

pdata_rank <- pdata %>%
  group_by(lga) %>%
  mutate(my_ranks = order(group)) %>%
  filter(my_ranks==1)


g <- gmap + 
    geom_point(data=pdata_rank, aes(x=lon, y=lat, size=TotalProperties,color=BushFire)) +
    scale_color_manual(values=c("red", "orange", "yellow","green" )) +
  ggtitle("Properties near Bushfire Zone")
  
ggsave("BushFireImpact_v1.png",g)
write.csv(pdata_rank,"Bushfire_mapdata.csv")
   



