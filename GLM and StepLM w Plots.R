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
library(stargazer)
library(nnet)
library(olsrr)
library(dplyr)
library(MASS)

write_csv(df, "Bayes Model Data.csv")
write_csv(train_data, "Model Train Data.csv")
write_csv(test_data,"Model Test Data.csv")

df <- "Bayes Model Data"

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

as_tibble(df)

d_summary <- df%>%
  psych::describe(quant = c(.25, .75))%>%
  as_tibble(rownames = "rowname")%>%
  drop_na()%>%
  print() 

view(d_summary)

plot(d_summary)

## Plot distribution of prices

ggplot(df) +
  geom_histogram(aes(y = price)) +
  coord_flip()

## Split Data into random train and test sets

df1 <- select_(df,'price')

colnames(df)

df <- select_(df, 'price',
              'distance_from_CBD',
              'distance_from_closest_private_school',
              'distance_from_closest_public_school',
              'distance_from_closest_station',
              'Violent.Crime',
              'Non.Violent.Crime',
              'Drug.offences')

dt <-  sort(sample(nrow(df), nrow(df)*.8)) 

glimpse(dt)

train_data <- df[dt,]
test_data <- df[-dt,]

nrow(train_data)
nrow(test_data)

## GLM with no variable - price only

colnames(train_data)

price_lmfull <- lm(price ~ distance_from_CBD +
                     distance_from_closest_private_school +
                     distance_from_closest_public_school +
                     distance_from_closest_station +
                     Violent.Crime +
                     Non.Violent.Crime +
                     Drug.offences,
                   data = train_data,
                   na.action = na.omit)

tidy(summary(price_lmfull))
glance(price_lmfull)
augment(price_lmfull)

summary(price_lmfull)
summary(price_step)

## Stepwise regression model 

price_step <- stepAIC(price_lmfull, direction = "both")

tidy(price_step)



price_step$coefficients

## Measures of the Model

s_resid <- rstandard(price_step)
hat_values <- hatvalues(price_step)
cooks_D <- cooks.distance(price_step)

## Plots of the Measures

par(mfrow = c(1,2))

plot(hat_values, s_resid, cex = 10*sqrt(cooks_D))
abline(h = c(-2.5, 2.5), lty = 2)
idx <- order(sresid)
sresid[idx]


df11 <- data_frame(
  resid = residuals(price_step),
  pred = predict(price_step)
)

ggplot(df11, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth()



## Heteroskedacity for the model

ggplot(df11, aes(resid)) +
  geom_histogram()

## ANOVA for the model

price_step$anova

## tepwise Model Path 
##Analysis of Deviance Table

##Initial Model:
  price ~ distance_from_CBD + distance_from_closest_private_school + 
  distance_from_closest_public_school + distance_from_closest_station + 
  Violent.Crime + Non.Violent.Crime + Drug.offences

##Final Model:
  price ~ distance_from_CBD + distance_from_closest_private_school + 
  distance_from_closest_public_school + distance_from_closest_station + 
  Violent.Crime + Drug.offences


##Step Df     Deviance Resid. Df   Resid. Dev      AIC
##1                                         38385 4.023198e+15 974246.9
##2 - Non.Violent.Crime  1 117639469172     38386 4.023316e+15 974246.1
  

## Plot Residuals v Fitted Results
  
  
  par(mfrow = c(2,2))
  
  plot(price_step)

## Relative importance of the variables in the model
  
calc.relimp(price_step, type = c("lmg", "last", "first", "pratt"), rela = TRUE)  
  