library(xml2)
library(rvest)
library(stringr)
library(XML)

ato_webpg <- read_html("https://www.ato.gov.au/Individuals/Dealing-with-disasters/In-detail/Specific-disasters/Bushfires-2019-20/?anchor=NotinanimpactedpostcodeorLGA#NewSouthWales")

df <- ato_webpg %>%
  html_nodes("div:nth-child(1)  > p:nth-child(n+14):nth-child(-n+49)") %>%
  xml_text() %>%
  str_split_fixed(":", 2) 

#Parsing ways to split raw data into tabular format
LGA_Names  <- df[,1]
postal_code <- strsplit(df[,2],",")

parse <-""
for (i in 1:nrow(df)) {
  parse<-rbind(as.data.frame(list("LGA"=rep(LGA_Names[i],length(postal_code[[i]])),"postal_code"=as.numeric(postal_code[[i]]))),parse)
}

df_parse <- as.data.frame(parse)

write.csv(df_parse,"Bushfire/Results/ATO_Deferred.csv")




