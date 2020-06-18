library(readxl)
library(tidyverse)

#2019 Dataset

setwd("./../NOM/DataSet")

xl_data_2019 <- "NOM_32180ds0002_2018-19.xls"
xl_data_2018 <- "NOM_32180ds0002_2017-18.xls"

df_nsw_2019 <- read_excel(path = xl_data_2019, sheet = "Table 1")
df_nsw_2018 <- read_excel(path = xl_data_2018, sheet = "Table 1")

cnt_2019 <- nrow(df_nsw_2019)
cnt_2018 <- nrow(df_nsw_2018)

#Remove headers and read only the data rows correspecding to NOM no's

Ext_tib_2019 <- data.frame(rep(2019,cnt_2019-8),
                      df_nsw_2019[9:cnt_2019,2],
                      df_nsw_2019[9:cnt_2019,11]) %>%
                  na.omit() 

colnames(Ext_tib_2019) <- c("Year", "Suburb", "No_ofNOM")

Ext_tib_2018 <- data.frame(rep(2018,cnt_2018-8),
                      df_nsw_2018[9:cnt_2018,2],
                      df_nsw_2018[9:cnt_2018,11]) %>%
                      na.omit() 

colnames(Ext_tib_2018) <- c("Year", "Suburb", "No_ofNOM")

colnames(Ext_tib_2019) <- c("Year", "LGA", "Nom")
colnames(Ext_tib_2018) <- c("Year", "LGA", "Nom")

write.csv(rbind(Ext_tib_2019,Ext_tib_2018),"./Results/Nom_LGA.csv")

