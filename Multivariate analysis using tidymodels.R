

norm_rec <- recipe(distance_from_CBD +
                     no_of_bath +
                     house_size +
                     distance_from_closest_station +
                     no_of_bed +
                     distance_from_closest_private_school +
                     distance_from_closest_public_school +
                     no_of_parking +
                     postcode +
                     mean_taxable_income +
                     Drug.offences +
                     Violent.Crime +
                     Nom.number +
                     ER_score +
                     avg_temp +
                     median_taxable_income +
                     AQI +
                     Non.Violent.Crime +
                     SAdvDisav_score +
                     EDUOCC_score +
                     price ~ .,
                   data = train_data) %>%
  step_center(everything())%>%
  step_scale(everything())

norm_rec

set.seed(57689)

folds <- vfold_cv(train_data, repeats = 10)

folds

folds <- folds %>%
  mutate(recipes = map(splits, prepper, recipe = norm_rec))

x_mat <- juice(norm_rec, composition = "matrix", all_predictors())
y_mat <- juice(norm_rec, composition = "matrix", all_outcomes())

get_var_explained <- function(recipe, ...) {
  y_mat <- juice(norm_rec, composition = "matrix", all_outcomes())
  x_mat <- juice(norm_rec, composition = "matrix", all_predictors())
  pls_format <- data.frame(
    endpoints = I(y_mat),
    measurements = I(x_mat))
  
  mod <- plsr(enpoints ~ measurements, data = pls_format)
  
  xve <- explvar(mod/100)
  
  explained <- drop(pls::R2(mod, estimate = "train_data1", intercept = FALSE)$val)%>%
    t() %>%
    as.data.frame() %>%
    mutate(predictors = cumsum(xve) %>%
             as.vector(),
           components = seq_along(xve))%>%
    gather(source, proportion, ~components)
}

get_var_explained()

folds <- folds %>%
  mutate(var = map(recipes, get_var_explained))
