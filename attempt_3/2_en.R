library(tidyverse)
library(tidymodels)
library(doParallel)
library(stacks)

load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")
load("data/reg_folds.rda")

doParallel::registerDoParallel(4)

en_3_mod <- linear_reg(penalty = tune(),
                     mixture = tune()) |> 
  set_engine("glmnet") |> 
  set_mode("regression")

en_3_wflow <- workflow() |> 
  add_model(en_3_mod) |> 
  add_recipe(reg_recipe)

en_3_grid <- grid_regular(
  penalty(range = c(-5, 1)),
  mixture(),
  levels = 10
)

set.seed(222)

en_3_res <- tune_grid(
  en_3_wflow,
  resamples = reg_folds,
  grid = en_3_grid,
  metrics = metric_set(mae),
  control = control_stack_grid()
)

save(en_3_res, file = "results/en_3_res.rda")