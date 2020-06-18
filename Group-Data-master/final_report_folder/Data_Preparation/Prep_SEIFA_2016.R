#SEIFA Data;

library(readxl)
library(tidyverse)
library(stringr)

#2016 Dataset

xl_data_2016 <- "./SEIFA/2033055001 - ssc indexes.xls"
df_nsw_2016 <- read_excel(path = xl_data_2016, sheet = "Table 1")

cnt_2016 <- nrow(df_nsw_2016)

Ext_tib_2016 <- data.frame(Year = rep(2016,cnt_2016-5),
                           Suburb = df_nsw_2016[6:cnt_2016,2],
                           SAdvDisav_score = df_nsw_2016[6:cnt_2016,5], #Index of Relative Socio-economic Advantage and Disadvantage	
                           SAdvDisav_decile = df_nsw_2016[6:cnt_2016,6], #Index of Relative Socio-economic Advantage and Disadvantage	
                           ER_score=  df_nsw_2016[6:cnt_2016,7], #Index of Economic Resources	
                           ER_decile=  df_nsw_2016[6:cnt_2016,8], #Index of Economic Resources	
                           EDUOCC_score=  df_nsw_2016[6:cnt_2016,9], #Index of Education and Occupation	
                           EDUOCC_decile=  df_nsw_2016[6:cnt_2016,10] #Index of Education and Occupation	
)

colnames(Ext_tib_2016) <- c("Year","suburbraw","SAdvDisav_score","SAdvDisav_decile","ER_score","ER_decile","EDUOCC_score","EDUOCC_decile")

Ext_tib_2016$suburb <- ifelse(
  grepl("(",Ext_tib_2016$suburbraw, fixed=TRUE) & grepl("NSW",Ext_tib_2016$suburbraw, fixed=TRUE),
  trimws(substr(Ext_tib_2016$suburbraw,1,regexpr("\\(", Ext_tib_2016$suburbraw)-1)),
  ifelse(grepl("(",Ext_tib_2016$suburbraw, fixed=TRUE),NA,
  Ext_tib_2016$suburbraw)
)

Ext_tib_2016 <- Ext_tib_2016[!(is.na(Ext_tib_2016$suburb)),]

#match with property geocode

Prop_BF2018 <- as_tibble(fread("BushFire/Results/Property_Bushfire_combined_v2.csv",header=TRUE,stringsAsFactors = FALSE)) 


Combineddata <- left_join(Prop_BF2018,Ext_tib_2016,by=c('suburb')) %>%
  select(property_id,lga,lat,
         lon,date,price,address,suburb,postcode,url,no_of_bed,no_of_bath,
         no_of_parking,house_size,type,Within_5km_2019,Btwn_5_10km_2019,Btwn_10_15km_2019,
         Within_5km_2018,Btwn_5_10km_2018,Btwn_10_15km_2018,
         suburbraw,
         SAdvDisav_score,SAdvDisav_decile,
         ER_score,ER_decile,
         EDUOCC_score,EDUOCC_decile)


write.csv(Ext_tib_2016,"SEIFA/Results/SEIFA_2016_v1.csv")