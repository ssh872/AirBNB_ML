library(tidymodels)
library(tidyverse)

load("data/reg_clean.rda")
load("data/reg_folds.rda")
load("recipes/reg_recipe.rda")

doParallel::registerDoParallel(6)

mars_mod <- mars(
  num_terms = tune(),
  prod_degree = tune()
) |> 
  set_engine("earth") |> 
  set_mode("regression")

mars_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(mars_mod)

set.seed(123)

mars_grid <- grid_regular(
  num_terms(range = c(50,200)),
  prod_degree(range = c(1,2)),
  levels = 5
)

mars_tune <- tune_grid(
  mars_wflow,
  resamples = reg_folds,
  grid = mars_grid,
  metrics = metric_set(mae)
)

save(mars_tune, file = "results/mars_tune.rda")
