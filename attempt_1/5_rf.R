library(tidyverse)
library(tidymodels)
library(doParallel)

load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")
load("data/reg_folds.rda")

doParallel::registerDoParallel(7)

set.seed(222)

rf_mod <- rand_forest(trees = 200,
                      mtry = tune(),
                      min_n = tune()) |> 
  set_engine("ranger", num.threads = 1) |> 
  set_mode("regression")

rf_wflow <- workflow() |> 
  add_model(rf_mod) |> 
  add_recipe(reg_recipe)

rf_params <- rf_wflow |>
  extract_parameter_set_dials() |>
  update(mtry = mtry(range = c(2, 20)))

rf_res <- tune_grid(
  rf_wflow,
  resamples = reg_folds,
  grid = 10,
  param_info = rf_params,
  metrics = metric_set(mae)
)

save(rf_res, file = "results/rf_res.rda")