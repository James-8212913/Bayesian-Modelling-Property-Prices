

# Gather the Data

## build test and train data

dfmaster <- df

write.csv(dfmaster, "/Users/jamesmurray/Desktop/Data Science/36103_STDS/Property Prices/Bayesian Model/Cleaned Property Data.csv")

view(df)

propertypricesplit <- initial_split(df, prop = .8)


glimpse(training(propertypricesplit)) ## Training

train_data <- training(propertypricesplit) %>%
  select_('price',
          'distance_from_CBD',
          'distance_from_closest_station',
          'distance_from_closest_public_school',
          'distance_from_closest_private_school')

glimpse(train_data)

test_data <- testing(propertypricesplit) %>%
  select_('price',
          'distance_from_CBD',
          'distance_from_closest_station',
          'distance_from_closest_public_school',
          'distance_from_closest_private_school')

glimpse(test_data)

## grouped by post codes as required

group_by(postcode)

## Build a Linear Model

priceglm_mod <- 
linear_reg() %>%
  set_engine("lm")

## Train the Data

priceglm_fit <- 
  priceglm_mod %>%
  fit(price ~ ., 
      train_data)%>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

## Predict the Values
  
results_train <- priceglm_fit %>%
  predict(new_data = train_data) %>%
  mutate(
    truth = train_data$price,
    model = "lm"
  )

results_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)
  glimpse()

glimpse(results_train)
  
results_test <- priceglm_fit %>%
  predict(new_data = test_data) %>%
  mutate(
    truth = test_data$price,
    model = "lm"
  )

results_test %>% 
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

## R Squqred, RMSE and MAE results

priceglm_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  metrics(truth = price, estimate = .pred)
  


tidy(priceglm_fit)
  
predict(priceglm_fit, test_data)

priceglm_fit %>%
  predict(test_data) %>%
  bind_cols(test_data) %>%
  glimpse()

tidy(priceglm_fit)


## Graph the results

results_test %>%
  mutate(train = "testing") %>%
  bind_rows(results_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, colour = "gray80", size = 1.5) +
  geom_point(alpha = .1) +
  facet_wrap(~train) +
  labs(
    title = "GLM Comparison between the Training and Testing Data",
    x = "Actual Prices",
    y = "Predicted Prices",
    colour = "Type of Model")



## Predict for a classification model (Postcodes..)
priceglm_fit %>%
  Predict(test_data, type + "prob")%>%
  glimpse()




