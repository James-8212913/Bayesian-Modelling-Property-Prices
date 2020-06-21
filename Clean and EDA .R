library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(car)
library(leaps)
library(AUC)
library(relaimpo)

view(cleaned_merged_property_data_final)

df <- cleaned_merged_property_data_final%>%
  mutate(
    date = as.Date(date, "%d/%m/%y"),
    type = as.factor(type),
    Within_5km_2019 = as.factor(Within_5km_2019),
    Btwn_5_10km_2019 = as.factor(Btwn_5_10km_2019),
    Btwn_10_15km_2019 = as.factor(Btwn_10_15km_2019),
    Within_5km_2018 = as.factor(Within_5km_2018),
    Btwn_5_10km_2018 = as.factor(Btwn_5_10km_2018),
    Btwn_10_15km_2018 = as.factor(Btwn_10_15km_2018),
)

glimpse(df)

df$no_of_bath = as.integer(df$no_of_bath)
df$no_of_parking = as.integer(df$no_of_parking)

## Remove outliers

boxplot(df$price)

priceoutliers <- boxplot(df$price)$out

df <- df[-which(df$price %in% priceoutliers),]

boxplot(df$price)

## Summary Stats for the Data Set

summary(df)

## Plot distribution of prices

ggplot(df) +
  geom_histogram(aes(y = price)) +
  coord_flip()

## Split Data into random train and test sets

dt = sort(sample(nrow(df), nrow(df)*.8))

train_data <-  df[dt,]
test_data <- df[-dt,]

nrow(train_data)
nrow(test_data)

## GLM with no variable - price only

train_data_lm <- lm(price ~ 1, train_data)

colnames(df)

summary(train_data_lm)

step_lm1 <- step(train_data_lm, 
                direction = c('forward'), #choose forward, backward, both
                trace = 1, #set to 0 to hide result from console 
                scope = price ~ distance_from_CBD +
                  no_of_bed +
                  no_of_bath +
                  no_of_parking +
                  house_size +
                  distance_from_closest_station +
                  distance_from_closest_public_school +
                  distance_from_closest_private_school,
                  steps = 1000) #increase steps for better prediction

# Summary of Result
summary(step_lm1) 
step_lm1$anova

train_data1 <- select(train_data, c(distance_from_CBD, no_of_bath, no_of_parking, no_of_bed, house_size, distance_from_closest_station,distance_from_closest_public_school, distance_from_closest_private_school))

price_data1 <- select(train_data, c(price,distance_from_CBD, no_of_bath, no_of_parking, no_of_bed, house_size, distance_from_closest_station,distance_from_closest_public_school, distance_from_closest_private_school))

view(train_data1)
view(price_data1)

all_vifs <- car::vif(step_lm)

print(all_vifs)

plot(step_lm1)

anova(step_lm, step_lm1)

colnames(train_data)

## All Subsets Regression with the 10 best models with each subsets size 

leaps <-  regsubsets(price ~ distance_from_CBD +
                       no_of_bed +
                       no_of_bath +
                       no_of_parking +
                       house_size +
                       distance_from_closest_station +
                       distance_from_closest_public_school +
                       distance_from_closest_private_school, 
                     data = train_data,
                     nbest = 5)
summary(leaps)

plot(leaps, scale = "r2")

## Relative Importance for each variable in the model

calc.relimp(step_lm1, type = c("lmg", "last", "first", "pratt"), rela = TRUE)

boot <- boot.relimp(step_lm1, b = 1000, type = c("lmg", "last", "first", "pratt"), ranks = TRUE, diff = TRUE, rela = TRUE)

booteval.relimp(boot)

plot(booteval.relimp(boot, sort = TRUE))


pricelm_test <- predict.lm(step_lm1, test_data, interval = 'prediction', se.fit = T)

cor(test_data$price,pricelm_test$fit[,1]) ## .442528

summary(pricelm_test$fit[,1]) 
##  Min.     1st Qu.  Median    Mean   3rd Qu.    Max. 
## -22723  774460  956384   941733   1132592     2174415

summary(test_data$price)
##Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##88500  648844  810000  935276 1135000 2225000


## create the MAE function

MAE <- function(actual, predicted) {mean(abs(actual - predicted))}

MAE(pricelm_test$fit[,1], test_data$price)
## [1] 292250.2

## Combine the actual dependant variables and the predicted dependant variables with the upper confidence value and the lower confidence value

depvar <- as.data.frame(cbind(train_data$price,pricelm_test$fit))
depvar

colnames(depvar)

varupr <- depvar$upr
varlwr <- depvar$lwr

plot_CI <- ggplot(data = depvar, aes( x= test_data$price, y = fit) +
                                        geom_point() +
                                        ggtitle("Linear Regression Model") + 
                                        labs(x = "Actual", y = "Predicted"))

plot_CI



