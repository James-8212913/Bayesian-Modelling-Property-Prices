library(geosphere)
library(tidyverse)
library(glmnet)
library(caret)

property_data <- read.csv("property_data_v3.csv")%>%
  select(-X1, -date, -suburb, -property_type, -close_to_schools, -close_to_shops, -close_to_transport, -administrative_area_level_2)
station_data <- read.csv("./Distance From Train Station/[Data] NSW_Station_Entrances_Locations_2020.csv")
public_school_data <- read.csv("./Distance From Public School/[Data] NSW_public_schools_2016.csv")
private_school_data <- read.csv("./Distance From Non-Government School/[Data] NSW_non_government_school_2017.csv")
crime_data <- read.csv("./Crime/[Output] Crime_Data_by_postcode_2018_19.csv")
# seifa_data <- read.csv("./SEIFA/Results/SEIFA_2016.csv")

# function 'distm' returns distance matrix between two sets of points

### Distance From CBD ###

CBD_longtitude <- 151.2093
CBD_latitude <- -33.8688
temp <- round(distm(cbind(property_data$longtitude, property_data$latitude), c(CBD_longtitude, CBD_latitude), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
     property_data$distance_from_CBD[i] <- temp[i, 1]
}

### Distance From Closest Entrance of Train Station ###

temp <- round(distm(cbind(property_data$longtitude, property_data$latitude), cbind(station_data$LONG, station_data$LAT), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  station_entrance_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_station[i] <- temp[i, station_entrance_number]
}


### Distance From Closest Public School ###

temp <- round(distm(cbind(property_data$longtitude, property_data$latitude), cbind(public_school_data$Longitude, public_school_data$Latitude), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  public_school_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_public_school[i] <- temp[i, public_school_number]
}

### Distance From Closest Non-Government School ###

temp <- round(distm(cbind(property_data$longtitude, property_data$latitude), cbind(private_school_data$longtitude, private_school_data$latitude), fun = distGeo))/1000
for (i in 1:nrow(property_data)){
  private_school_number <- which(temp[i, ] == min(temp[i, ]))
  property_data$distance_from_closest_private_school[i] <- temp[i, private_school_number]
}


summary(property_data)
property_data$postal_code[9615] <- 2219
property_data <- property_data[-11358, ]

property_data <- property_data%>%
  left_join(crime_data, by = c("postal_code" = "postcode"))%>%
  select(-house_size, -north_rear_facing, -postal_code, -latitude, -longtitude)%>%
  filter(!is.na(no_of_bed))%>%
  replace_na(list(no_of_parking = 0))

### Data Partition

set.seed(42)

train_binary <- createDataPartition(y = property_data$property_prices, p = 0.7, list = F)
training_binary <- property_data[train_binary, ]
testing_binary <- property_data[-train_binary, ]

testing_predict <- data.frame(prices = testing_binary$property_prices)


### GLM

fit_glm <- glm(formula = property_prices ~ .,
               data = training_binary,
               family = "gaussian")
summary(fit_glm)
with(summary(fit_glm), 1 - deviance/null.deviance)

par(mfrow=c(2,2))
plot(fit_glm)
testing_predict$predict_glm <- predict(fit_glm, testing_binary, type = "response")


### Ridge Regression

# The glmnet package requires data be in separate x and y sets; the predictor set, x, must be a matrix
x <- model.matrix(~ ., training_binary[, -1])
y <- training_binary$property_prices

# Fit Ridge Regression Model
# alpha = 0 specifies ridge regression
fit_ridge <- cv.glmnet(x, y, alpha = 0)
summary(fit_ridge)
par(mfrow=c(1,1))
plot(fit_ridge)

# lambda.min
fit_ridge$lambda.min
coef(fit_ridge, s = fit_ridge$lambda.min)
testing_predict$predict_ridge_lambda_min <- predict(fit_ridge$glmnet.fit, newx = model.matrix(~., testing_binary[, -1]), 
                                                    type = "response", s = fit_ridge$lambda.min)
# lambda.1se
fit_ridge$lambda.1se
coef(fit_ridge, s = fit_ridge$lambda.1se)
testing_predict$predict_ridge_lambda_1se <- predict(fit_ridge$glmnet.fit, newx = model.matrix(~., testing_binary[, -1]),
                                                    type = "response", s = fit_ridge$lambda.1se)


### Lasso Regression

# Fit Lasso Regression Model
# alpha = 1 specifies Lasso regression
fit_lasso <- cv.glmnet(x, y, alpha = 1)
summary(fit_lasso)
par(mfrow=c(1,1))
plot(fit_lasso)

# lambda.min
fit_lasso$lambda.min
coef(fit_lasso, s = fit_lasso$lambda.min)
testing_predict$predict_lasso_lambda_min <- predict(fit_lasso$glmnet.fit, newx = model.matrix(~., testing_binary[, -1]), 
                                                    type = "response", s = fit_lasso$lambda.min)
# lambda.1se
fit_lasso$lambda.1se
coef(fit_lasso, s = fit_lasso$lambda.1se)
testing_predict$predict_lasso_lambda_1se <- predict(fit_lasso$glmnet.fit, newx = model.matrix(~., testing_binary[, -1]), 
                                                    type = "response", s = fit_lasso$lambda.1se)

RMSE <- function(error) { sqrt(mean(error^2)) }

metric <- c()
for (i in 2:ncol(testing_predict)){
  metric[i-1] <- RMSE(testing_predict[,i]-testing_predict[,1])
}
metric
which(metric==min(metric))