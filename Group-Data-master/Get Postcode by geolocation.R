#information from https://gist.github.com/randomecho/5020859

library(readr)
library(sqldf)
library(stringr)
#read file using readr packages
file = read_file("australian-postcodes.txt")

#create blank dataframe
postcodes_geo <- data.frame(
  postcode = character(),
  suburb = character(),
  state = character(),
  latitude = double(),
  longitude = double()
)

#insert SQL style data into postcodes_geo
postcodes_geo <- sqldf(c(paste("INSERT INTO postcodes_geo (postcode, suburb, state, latitude, longitude) VALUES",
  file),
  "SELECT * from main.postcodes_geo"))

#change 3 digit post codes to 4 digit
postcodes_geo$postcode <- lapply(postcodes_geo$postcode, function(x) str_pad(x, 4, pad = "0"))

#select NSW post codes only with longigutde and latitude value
postcodes_geo_NSW <- postcodes_geo %>%
  subset(state == "NSW" & longitude != 0 & latitude != 0)

#save file
write.csv(as.data.frame(lapply(postcodes_geo_NSW, unlist)), "postcodes_geo_NSW.csv")
