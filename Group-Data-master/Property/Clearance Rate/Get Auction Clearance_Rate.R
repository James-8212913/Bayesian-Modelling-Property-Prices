library(rvest)
library(tidyverse)

#url for webscraping

#last date of auction date
date <- as.Date("2020-04-04")
#base url for webscraping
base_url <- "https://www.domain.com.au/auction-results/sydney/"
data <- data.frame()
i <- 104


#repeat 104 times
while (i > 0 ){
  url <- paste(base_url, date, sep="")
  #read auction sales dta from the url
  flag <- TRUE
  tryCatch(
    expr = {
      clearance_rate <- read_html(url) %>%
        html_nodes(xpath ='//*/div/main/section/div/div[2]/div[1]/div[2]/div/div/div[1]/span/text()') %>%
        html_text()
      data <- rbind(data, data.frame(date = date, clearance_rate = as.numeric(clearance_rate)/100))},
    error = function(e) flag <<- FALSE)
  if (!flag){
    i <- i -1
    date <- date - 7
    #sleep to prevent possible data scraping interruption
    next
  }

  i <- i - 1
  date <- date - 7
  #sleep to prevent possible data scraping interruption
}
if (date == as.Date("2018-04-07"))  rbind(data, data.frame(date = date, clearance_rate = as.numeric("56")/100))
write.csv(data, "clearance_rate.csv", row.names = FALSE)
