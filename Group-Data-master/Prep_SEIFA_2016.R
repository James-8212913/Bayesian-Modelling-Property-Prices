library(readxl)
library(tidyverse)

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

write.csv(Ext_tib_2016,"SEIFA/Results/SEIFA_2016.csv")


