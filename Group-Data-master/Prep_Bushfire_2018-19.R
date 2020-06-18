#Now going to 2018-19 bushfire season:
library(tidyverse)
library(data.table)
library(geosphere)

#Conditions applied: 
#Select hotspot/firepixes with "high" confidence level
#Select pixel points only under NSW properties.These lats and longs are boundaries of collected property data
#Period: Ran for 2018-09-01 to 2019-03-31

Df_2018 <- as_tibble(fread("DataSet/fire_archive_V1_117459.csv", header = TRUE, stringsAsFactors = FALSE) %>%
                       filter(confidence=='h') %>%
                       filter(latitude > -35  & longitude >=150 & latitude<= -33 & longitude < 152) %>%  
                       filter(acq_date >= '2018-09-01' & acq_date <= '2019-03-31'))

nrow(Df_2018)
res_nf <- as_tibble()

for(i in 1: nrow(Df_2018)){
  df2018_ind <- Df_2018[i,]
  p_gc <- as_tibble(fread("DataSet/geocode_data.csv", header = TRUE, stringsAsFactors = FALSE) %>%
                      filter(country=='Australia' & state=='New South Wales') %>%
                      mutate(
                        dist_between=distHaversine(cbind(df2018_ind$longitude,df2018_ind$latitude),cbind(lng,lat))/1000,
                        FIRMS_18_lat=df2018_ind$latitude,
                        FIRMS_18_lng=df2018_ind$longitude,
                        FIRMS_18_cf=df2018_ind$confidence,
                        FIRMS_18_aq=df2018_ind$acq_date
                      ) %>%
                      filter(dist_between<=15) %>%
                      dplyr::select(FIRMS_18_lat,FIRMS_18_lng,FIRMS_18_cf,FIRMS_18_aq,dist_between,no,postal_code))
  
  res_nf <- rbind(p_gc,res_nf)
}

write_csv(res_nf,"Results/NASA_Firms_Impact_15km_2018.csv")
