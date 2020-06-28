library(rstan)
library(rstanarm)
library(shinystan)
library(tidyverse)
library(bayesplot)
library(BayesianTools)
theme_set(bayesplot::theme_default())
library(ggplot2)
library(brm)
library(tidymodels)
library(tidybayes)


df <- Bayes_Model_Data

dt <-  sort(sample(nrow(df), nrow(df)*.8)) 

glimpse(dt)

train_data <- df[dt,]
test_data <- df[-dt,]

nrow(train_data)
nrow(test_data)

colnames(train_data)

prior1 <- normal(0,5)

## Weakly Informative Priors (the default settings in rstanarm)

glm_price_weak <- post1 <- stan_glm (price ~ distance_from_CBD + 
                                  distance_from_closest_private_school +
                                  distance_from_closest_station +
                                distance_from_closest_public_school +
                                Violent.Crime +
                                Non.Violent.Crime +
                                  Drug.offences,
                                  data = train_data, 
                                family = "gaussian")
                                
prior_summary(glm_price_weak)                               

plot(glm_price_weak, "hist")



stan_trace(glm_price_weak)

summary(glm_price_weak)

pp_check(glm_price_weak)

pp_check(glm_price, plotfun = "hist", nreps = 5)

pp_check(glm_price, plotfun = "stat", stat = "mean")

pp_check(glm_price, plotfun = "stat_2d", stat = c("mean", "sd"))

launch_shinystan(glm_price_weak)

stan_hist(glm_price_weak, pars = c"Price")

## Comparison of Posterior vs Priors for the model

posterior_vs_prior(glm_price)

## Model Validation via Simulation

pp_validate(glm_price, nreps = 20, seed = 12345)

## Pointwise log-liklihood matrix

## Flat Priors ( altered defaults in rstanarm)

glm_price_flat <- update(glm_price_weak,
                         prior = normal(0,5, autoscale = FALSE),
                         prior_intercept = student_t(4,0,10, autoscale = FALSE),
                         prior_aux = cauchy(0,3,autoscale = FALSE)
)

glm_price_flat1 <- stan_glm(price ~ distance_from_CBD + 
                              distance_from_closest_private_school +
                              distance_from_closest_station +
                              distance_from_closest_public_school +
                              Violent.Crime +
                              Non.Violent.Crime +
                              Drug.offences,
                            data = train_data, 
                            family = "gaussian",
                            prior = NULL)


## Testing// Learning

price_testing <- 
  recipe(price ~., data = train_data) %>%
  update_role()







prior1 <- normal(location = c(0,0,0,0,0,0,0), scale = c(2,2,2,2,2,2,2), autoscale = FALSE)

glm_price_inform1 <- stan_glm(price ~distance_from_CBD + 
                               distance_from_closest_private_school +
                               distance_from_closest_station +
                               distance_from_closest_public_school +
                               Violent.Crime +
                               Non.Violent.Crime +
                               Drug.offences,
                             data = train_data, 
                             family = "gaussian",
                             prior = )

prior_summary(glm_price_inform1)
prior_summary(glm_price_flat)                               
prior_summary(glm_price_flat1) 

plot
plot(glm_price_flat, "hist")
plot(glm_price_flat1, "hist")
stan_trace(glm_price_flat)
stan_trace(glm_price_flat1)
summary(glm_price_flat)

pp_check(glm_price_flat)
pp_check(glm_price_flat1)
pp_check(glm_price_flat, plotfun = "hist", nreps = 5)
pp_check(glm_price_flat1, plotfun = "hist", nreps = 5)
pp_check(glm_price_flat, plotfun = "stat", stat = "mean")
pp_check(glm_price_flat1, plotfun = "stat", stat = "mean")
pp_check(glm_price_flat, plotfun = "stat_2d", stat = c("mean", "sd"))

launch_shinystan(glm_price_flat)

plot(glm_price_weak, regex_par())


## Comparison of Posterior vs Priors for the model

posterior_vs_prior(glm_price_inform1)
posterior_vs_prior(glm_price_flat)
posterior_vs_prior(glm_price_flat1)
posterior_vs_prior(glm_price_weak)

loo1 <- loo(glm_price, save_psis = TRUE, cores = 4)
loo2 <- loo(glm_price_flat1, save_psis = TRUE, cores = 4)
loo3 <- loo(glm_price_weak, save_psis = TRUE, cores = 4)
loo4 <- loo(glm_price_inform1, save_psis = TRUE, cares = 4)

loo1

loo(glm_price)

loo_compare(loo1, loo2, loo3, loo4)

par(mfrow = c(2,2))

plot(loo(glm_price))
plot(loo(glm_price_flat1))
plot(loo(glm_price_weak))
plot(loo(glm_price_inform1))

loo_compare()

waic(glm_price, glm_price_flat1, glm_price_inform1, glm_price_weak)

stan_plot(glm_price)
stan_plot(glm_price_flat1)
stan_plot(glm_price_inform1)
stan_plot(glm_price_weak)

pp_check(glm_price_inform1)
pp_check(glm_price)
pp_check(glm_price_flat1)
pp_check(glm_price_weak)


pp_check(glm_price, plotfun = "boxplot")
pp_check(glm_price, plotfun = "hist")
pp_check(glm_price, plotfun = "scatter_avg")
pp_check(glm_price, plotfun = "scatter")
pp_check(glm_price, plotfun = "error_scatter_avg_vs_x", x = "distance_from_CBD")
pp_check(glm_price, plotfun = "dens_overlay")
pp_check(glm_price, plotfun = "stat", stat = "mean")

marginalModelPlot(glm_price, x = "distance_from_CBD", plot_pi = TRUE, jitter = TRUE, smooth_method = "loess")

## Model Validation via Simulation

posterior_predict(glm_price_weak)

pp_validate(glm_price, nreps = 20, seed = 12345)

plot(summary(train_data))

help(priors)

m <- get_variables(glm_price)

get_variables(glm_price)

head(m)
view(m)
glimpse(m)

glm_price %>%
  spread_draws('(intercept)', sigma) %>%
  head()

vars %>%
  spread_draws('(intercept)', sigma) %>%
  head()

glm_price

to_broom_names(m)

gather_draws(glm_price, variable_name[vars], regex = FALSE, sep = "[,]")


data(RankCorr, package = "ggdist")

get_variables(RankCorr)
get_variables(glm_price_flat1)
glimpse(glm_price_flat1)

glimpse(RankCorr)

RankCorr %>%
  spread_draws(b[i,j])

n %<>% recover_types(train_data)

m %<>% recover_types(train_data)
  
tidybayes::recover_types(glm_price_flat1)  

tidybayes::compose_data(train_data)

glm_price_flat1 %>%  
  spread_draws(distance_from_CBD) %>%
  to_ggmcmc_names()

glm_price_flat1 %>%
  mode_hdi(distance_from_CBD, .width = .80)

str(rstan::extract(glm_price_flat1))

tidy(cbind(Median = coef(glm_price), MAD_SD = se(glm_price_flat1)))

summary(residuals(glm_price_flat1))

tidy(cov2cor(vcov(glm_price_flat1)))

o <- posterior_predict(glm_price_flat1)  
dim(o)
