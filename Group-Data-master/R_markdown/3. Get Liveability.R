library(tidyverse)
library(rvest)

#Sydney’s 569 suburbs ranked for liveability
url = "https://www.domain.com.au/liveable-sydney/sydneys-most-liveable-suburbs-2019/sydneys-569-suburbs-ranked-for-liveability-2019-903130/"
temp <- read_html(url)

suburbs <- temp %>%
  html_nodes(xpath = '//h3') %>%
  html_text()


info <- temp %>% 
  html_nodes(xpath = '//*[@id="post-903130"]/section/p') %>%
  html_text() 


data <- data.frame(suburbs) %>%
  separate(info, c("ranking", "suburb"), "\\.") %>%
  na.omit() %>%
  mutate(ranking = as.numeric(ranking))

data$info = info[-1]

write.csv(data, "liveability.csv", row.names = FALSE)
