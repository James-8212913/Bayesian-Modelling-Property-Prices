#Now going to 2018-19 bushfire season:
library(tidyverse)
library(data.table)
library(geosphere)

#Conditions applied: 
#Select hotspot/firepixes with "high" confidence level
#Select pixel points only under NSW properties.These lats and longs are boundaries of collected property data
#Period: Recursively Ran for 2019-09-01 to 2020-03-31

Df_2019 <- as_tibble(fread("DataSet/fire_archive_V1_117459.csv", header = TRUE, stringsAsFactors = FALSE) %>%
                       filter(confidence=='h') %>%
                       filter(latitude > -35  & longitude >=150 & latitude<= -33 & longitude < 152) %>%  
                       filter(acq_date >= '2020-01-01' & acq_date <= '2020-03-31' ) ) #& acq_date <= '2020-03-31'))

nrow(Df_2019)
#res_nf <- as_tibble()

for(i in 1: nrow(Df_2019)){
  Df2019_ind <- Df_2019[i,]
  p_gc <- as_tibble(fread("DataSet/geocode_data.csv", header = TRUE, stringsAsFactors = FALSE) %>%
            filter(country=='Australia' & state=='New South Wales') %>%
            mutate(
              dist_between=distHaversine(cbind(Df2019_ind$longitude,Df2019_ind$latitude),cbind(lng,lat))/1000,
              FIRMS_19_lat=Df2019_ind$latitude,
              FIRMS_19_lng=Df2019_ind$longitude,
              FIRMS_19_cf=Df2019_ind$confidence,
              FIRMS_19_aq=Df2019_ind$acq_date
            ) %>%
            filter(dist_between<=15) %>%
            dplyr::select(FIRMS_19_lat,FIRMS_19_lng,FIRMS_19_cf,FIRMS_19_aq,dist_between,no,postal_code))
  
  res_nf <- rbind(p_gc,res_nf)
}


write_csv(res_nf,"Results/NASA_Firms_Impact_15km_2019.csv")
