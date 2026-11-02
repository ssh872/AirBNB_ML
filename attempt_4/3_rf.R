library(tidymodels)
library(tidyverse)
library(stacks)

load("data/reg_clean.rda")
load("data/reg_folds.rda")
load("recipes/reg_recipe.rda")

doParallel::registerDoParallel(6)

set.seed(203)

rf_mod <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 500
  ) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(rf_mod)

rf_grid <- grid_regular(
  finalize(mtry(), reg_clean),
  min_n(range = c(2, 20)),
  levels = 3
)

rf_tune <- tune_grid(
  rf_wflow,
  resamples = reg_folds,
  grid = rf_grid,
  metrics = metric_set(mae),
  control = control_stack_grid()
)

save(rf_tune, file = "results/rf_tune.rda")