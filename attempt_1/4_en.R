library(tidyverse)
library(tidymodels)
library(doParallel)

load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")
load("data/reg_folds.rda")

doParallel::registerDoParallel(6)

en_mod <- linear_reg(penalty = tune(),
                     mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

en_wflow <- workflow() |> 
  add_model(en_mod) |> 
  add_recipe(reg_recipe)

en_grid <- grid_regular(
  penalty(range = c(-4, 0)),
  mixture(),
  levels = 5
)

set.seed(222)

en_res <- tune_grid(
  en_wflow,
  resamples = reg_folds,
  grid = en_grid,
  metrics = metric_set(mae)
)



save(en_res, file = "results/en_res.rda")