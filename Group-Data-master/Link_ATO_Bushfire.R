library(data.table)
library(tidyverse)

#Link it with NASA Firms data
Firms_19 <- fread("Bushfire/Results/NASA_Firms_Impact_15km_2019.csv", header = TRUE, stringsAsFactors = FALSE)
Firms_18 <- fread("Bushfire/Results/NASA_Firms_Impact_15km_2018.csv", header = TRUE, stringsAsFactors = FALSE)

ATO_Def <- fread("Bushfire/Results/ATO_Deferred.csv", header = TRUE, stringsAsFactors = FALSE) %>%
           select(LGA,postal_code)

Firms_19_ato <-  left_join(Firms_19, ATO_Def, by="postal_code") %>%
  dplyr::select(FIRMS_19_lat,FIRMS_19_lng,FIRMS_19_cf,FIRMS_19_aq,dist_between,no,postal_code,LGA)

Firms_19_ato$ato_flag <- if_else(is.na(Firms_19_ato$LGA),"N","Y") 

Firms_18_ato <-  left_join(Firms_18, ATO_Def, by="postal_code") %>%
  dplyr::select(FIRMS_18_lat,FIRMS_18_lng,FIRMS_18_cf,FIRMS_18_aq,dist_between,no,postal_code,LGA)

Firms_18_ato$ato_flag <- if_else(is.na(Firms_18_ato$LGA),"N","Y") 

write.csv(Firms_19_ato,"Bushfire/Results/NASA_Firms_Impact_15km_2019_atoflag.csv")
write.csv(Firms_18_ato,"Bushfire/Results/NASA_Firms_Impact_15km_2018_atoflag.csv")

