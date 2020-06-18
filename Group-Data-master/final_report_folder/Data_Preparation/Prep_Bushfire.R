#Now going to 2018-19 bushfire season:
library(tidyverse)
library(data.table)
library(geosphere)

#Read property geocode data

prop_code <- as_tibble(fread("DataSet/property_data_with_geocode.csv", header = TRUE, stringsAsFactors = FALSE))

minlat <- min(prop_code$lat)
minlon <- min(prop_code$lon)
maxlat <- max(prop_code$lat)
maxlon <- max(prop_code$lon)

#Conditions applied: 
#Select hotspot/firepixes with "high" confidence level
#Select pixel points only under NSW properties.These lats and longs are boundaries of collected property data
#Period: Ran for 2018-09-01 to 2019-03-31

sp1 <- seq(as.Date("2018-09-01"), as.Date("2019-03-01"), by="months")
ep1 <- seq(as.Date("2018-09-01"), as.Date("2019-04-01"), by="months")-1

final_res <- as_tibble()

for(month in 1:length(sp1)) {
 startdate <- sp1[month]
 enddate <- ep1[month+1]
  
 writeLines(sprintf("my start is %s",startdate))
 writeLines(sprintf("my end is %s",enddate))
 
 filteredBushFire <- as_tibble(fread("DataSet/fire_archive_V1_117459.csv", header = TRUE, stringsAsFactors = FALSE) %>%
     filter(confidence=='h') %>%
     filter(latitude > minlat  & longitude >= minlon & latitude<= maxlat & longitude < maxlon) %>%  
     filter(acq_date >= startdate & acq_date <= enddate))

 writeLines(sprintf("no of record is %s",nrow(filteredBushFire) ))
   
   if(nrow(filteredBushFire)>0) {
     for(i in 1: nrow(filteredBushFire)){
       df_row <- filteredBushFire[i,]
       p_gc <-  prop_code %>%
           mutate(
             dist_between=distHaversine(cbind(df_row$longitude,df_row$latitude),cbind(lon,lat))/1000,
             bushfire_lat=df_row$latitude,
             bushfire_lon=df_row$longitude,
             bushfire_confidence=df_row$confidence,
             bushfire_date=df_row$acq_date
           ) %>%
           filter(dist_between<=15) %>%
           dplyr::select(bushfire_lat,bushfire_lon,bushfire_confidence,bushfire_date,dist_between,property_id)
       
       final_res <- rbind(p_gc,final_res)
     }
   }
 
 }
  
write_csv(final_res,"BushFire/Results/NASA_Firms_Impact_15km_2018.csv") 

sp2 <- seq(as.Date("2019-09-01"), as.Date("2020-03-01"), by="months")
ep2 <- seq(as.Date("2019-09-01"), as.Date("2020-04-01"), by="months")-1

final_res <- as_tibble()

for(month in 1:length(sp2)) {
  startdate <- sp2[month]
  enddate <- ep2[month+1]
  
  writeLines(sprintf("my start is %s",startdate))
  writeLines(sprintf("my end is %s",enddate))
  
  filteredBushFire <- as_tibble(fread("DataSet/fire_archive_V1_117459.csv", header = TRUE, stringsAsFactors = FALSE) %>%
                                  filter(confidence=='h') %>%
                                  filter(latitude > minlat  & longitude >= minlon & latitude<= maxlat & longitude < maxlon) %>%  
                                  filter(acq_date >= startdate & acq_date <= enddate))
  
  writeLines(sprintf("no of record is %s",nrow(filteredBushFire) ))
  
  if(nrow(filteredBushFire)>0) {
    for(i in 1: nrow(filteredBushFire)){
      df_row <- filteredBushFire[i,]
      p_gc <-  prop_code %>%
        mutate(
          dist_between=distHaversine(cbind(df_row$longitude,df_row$latitude),cbind(lon,lat))/1000,
          bushfire_lat=df_row$latitude,
          bushfire_lon=df_row$longitude,
          bushfire_confidence=df_row$confidence,
          bushfire_date=df_row$acq_date
        ) %>%
        filter(dist_between<=15) %>%
        dplyr::select(bushfire_lat,bushfire_lon,bushfire_confidence,bushfire_date,dist_between,property_id)
      
      final_res <- rbind(p_gc,final_res)
    }
  }
  
}

write_csv(final_res,"BushFire/Results/NASA_Firms_Impact_15km_2019.csv") 


#Now get the distance converted to bucket per property

library(data.table)
library(tidyverse)
library(funModeling)
library(dplyr)
library(plyr)

Prop <- as_tibble(fread("DataSet/property_data_with_geocode.csv",header=TRUE,stringsAsFactors = FALSE)) 
 
BF <- as_tibble(fread("BushFire/Results/NASA_Firms_Impact_15km_2019.csv",header=TRUE,stringsAsFactors = FALSE)) %>%
  select(bushfire_lat,bushfire_lon,bushfire_confidence,bushfire_date,dist_between,property_id)

BF$dist_between <- round(as.numeric(BF$dist_between,0))
BF$dist_cat <- case_when(
  BF$dist_between <= 5 ~ "Within_5km",
  BF$dist_between > 5 & BF$dist_between <= 10 ~ "5-10km",
  BF$dist_between > 10 & BF$dist_between <= 15 ~ "10-15km",
  TRUE ~ ">15km"
)

#Select only property and distance category
BF_Cat <- BF %>% select(property_id,dist_cat)  %>%unique()

Combineddata <- left_join(Prop,BF_Cat,by=c('property_id')) %>%
  mutate(Within_5km_2019 = ifelse(dist_cat=="Within_5km" , "Y" , NA)) %>%
  mutate(Btwn_5_10km_2019 = ifelse(dist_cat=="5-10km" , "Y" , NA)) %>%
  mutate(Btwn_10_15km_2019 = ifelse(dist_cat=="10-15km" , "Y" , NA)) %>%
  select(property_id,lga,lat,lon,date,price,address,suburb,postcode,url,no_of_bed,no_of_bath,no_of_parking,house_size,type,Within_5km_2019,Btwn_5_10km_2019,Btwn_10_15km_2019) %>%
  unique()

coalesce_all_columns <- function(df) {
  return(coalesce(!!! as.list(df)))
}

final_result <- Combineddata %>% 
  group_by(property_id) %>%
  summarise_all(coalesce_all_columns)


write_csv(final_result,"BushFire/Results/Property_Bushfire_combined.csv") 


#2018 period:

Prop_BF2019 <- as_tibble(fread("BushFire/Results/Property_Bushfire_combined.csv",header=TRUE,stringsAsFactors = FALSE)) 

BF_2018 <- as_tibble(fread("BushFire/Results/NASA_Firms_Impact_15km_2018.csv",header=TRUE,stringsAsFactors = FALSE)) %>%
  select(bushfire_lat,bushfire_lon,bushfire_confidence,bushfire_date,dist_between,property_id)

BF_2018$dist_between <- round(as.numeric(BF_2018$dist_between,0))
BF_2018$dist_cat <- case_when(
  BF_2018$dist_between <= 5 ~ "Within_5km",
  BF_2018$dist_between > 5 & BF_2018$dist_between <= 10 ~ "5-10km",
  BF_2018$dist_between > 10 & BF_2018$dist_between <= 15 ~ "10-15km",
  TRUE ~ ">15km"
)

#Select only property and distance category
BF_Cat_2018 <- BF_2018 %>% select(property_id,dist_cat)  %>%unique()

Combineddata <- left_join(Prop_BF2019,BF_Cat_2018,by=c('property_id')) %>%
  mutate(Within_5km_2018 = ifelse(dist_cat=="Within_5km" , "Y" , NA)) %>%
  mutate(Btwn_5_10km_2018 = ifelse(dist_cat=="5-10km" , "Y" , NA)) %>%
  mutate(Btwn_10_15km_2018 = ifelse(dist_cat=="10-15km" , "Y" , NA)) %>%
  select(property_id,lga,lat,lon,date,price,address,suburb,postcode,url,no_of_bed,no_of_bath,no_of_parking,house_size,type,Within_5km_2019,Btwn_5_10km_2019,Btwn_10_15km_2019,Within_5km_2018,Btwn_5_10km_2018,Btwn_10_15km_2018) %>%
  unique()

coalesce_all_columns <- function(df) {
  return(coalesce(!!! as.list(df)))
}

final_result <- Combineddata %>% 
  group_by(property_id) %>%
  summarise_all(coalesce_all_columns)

write_csv(final_result,"BushFire/Results/Property_Bushfire_combined_v2.csv") 

















