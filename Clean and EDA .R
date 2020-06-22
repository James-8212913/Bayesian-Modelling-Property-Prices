library(tidyverse)
library(lubridate)
library(magrittr)
library(caret)
library(car)
library(leaps)
library(AUC)
library(relaimpo)
library(psych)
library(tidymodels)
library(rsample)
library(purrr)
library(pls)
library(yardstick)
library(magrittr)


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

## Summary Stats for the Data Set in a tibble

summary(df)

d_summary <- df%>%
  psych::describe(quant = c(.25, .75))%>%
  as_tibble(rownames = "rowname")%>%
  drop_na()%>%
  print() 

view(d_summary)


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

tidy(step_lm1)

glance(step_lm1)

step_lm1$anova

varaibles <- select_(d_summary, "rowname")

varaibles


all_vifs <- car::vif(step_lm1)

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

## Data Preparation 


train_data1 <- df%>%
  select_('postcode', 
          'price', 
          'no_of_bed',
          'no_of_bath',
          'no_of_parking',
          'house_size',
          'distance_from_CBD',
          'median_taxable_income',
          'mean_taxable_income',
          'AQI',
          'Drug.offences',
          'Non.Violent.Crime',
          'Violent.Crime',
          'distance_from_closest_station',
          'distance_from_closest_public_school',
          'distance_from_closest_private_school',
          'Nom.number',
          'SAdvDisav_score',
          'ER_score',
          'EDUOCC_score',
          'avg_temp')

dt = sort(sample(nrow(train_data1), nrow(df)*.8))

train_data <-  train_data1[dt,]
test_data <- train_data1[-dt,]

dim(train_data)

nrow(train_data)
colnames(train_data)

nrow(test_data)
colnames(test_data)

## Create and run the linear model

price_lmtrain <- lm(price ~., data = train_data)

## Results for the model

tidy(summary(price_lmtrain))

## Tidy Results for the model

Train_Summary <- tidy(price_lmtrain)

Train_Glance <- glance(price_lmtrain)

## Test the model

price_lmtest <- predict.lm(price_lmtrain, test_data, interval = 'prediction', se.fit = T)

price_lmtest

## Test model results

cor(test_data$price, price_lmtest$fit[,1]) ## [1] 0.8438662

Test_Summary <- tidy(summary(price_lmtest$fit[,1])) ##

## Mean Absolute Error

### Function Creation

MAE <- function(actual, predicted){
  mean(abs(actual - predicted))
}

### Results

MAE(price_lmtest$fit[,1], test_data$price) ## [1] $159446.3 

y_out_pricelm <- as.data.frame(cbind(test_data$price, price_lmtest$fit))
upper_price <- y_out_pricelm$upr
lower_price <- y_out_pricelm$lwr

y_out_pricelm

price_lmtest$fit
test_data$price


nrow(y_out_pricelm)


Observed_Plot <- ggplot(data = y_out_pricelm, aes(x = test_data$price, y = price_lmtest$fit[,1])) +
  geom_point() +
  ggtitle("Regression Model for Prioperty Prices") +
  labs(x = "Actual Prices", y = "Predicted Prices")

Observed_Plot 
  geom_errorbar(ymin = lower_price, ymax = upper_price)

## MAPE Calculation 

head(y_out_pricelm)

as_tibble(y_out_pricelm)

Price_Pred_Truth <- select_(y_out_pricelm,'V1','fit')

head(Price_Pred_Truth) 

as_tibble(Price_Pred_Truth)

## Joining the metrics together - MAE, MAPE, R2, RMSE

Comparison_of_Model_Metrics <- as_tibble(metrics(Price_Pred_Truth, V1, fit))

mape <- mape(Price_Pred_Truth, 'V1', 'fit', na_rm = TRUE)

Comparison_metrics <- full_join(Comparison_of_Model_Metrics, mape)

Comparison_metrics

precision(Price_Pred_Truth, 'V1', 'fit')

set.seed(12345)

size <- 100
times <- 10

Priceprediction_resampled <- bind_rows(
  replicate(
    n = times,
    expr = sample_n(Price_Pred_Truth, size, replace = TRUE),
    simplify = FALSE
  ),
  .id = "resample"
)
head(Priceprediction_resampled)  

metric_results <- Priceprediction_resampled %>%
  group_by(resample) %>%
  mape('V1', 'fit')

head(metric_results)

metric_results %>%
  summarise(avg_estimate = mean(.estimate))
