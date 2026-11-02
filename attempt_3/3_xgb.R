library(tidymodels)
library(tidyverse)
library(stacks)
library(embed)

load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")
load("data/reg_folds.rda")

doParallel::registerDoParallel(8)

set.seed(123)

xgb_3_model <- boost_tree(
  trees = 2000,
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  stop_iter = 50
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

xgb_3_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(xgb_3_model)

xgb_3_grid <- grid_random(
  finalize(mtry(), reg_clean),
  min_n(range = c(2, 20)),
  tree_depth(range = c(3, 10)),
  learn_rate(range = c(-2, -1)),
  size = 50
)

xgb_3_tune <- tune_grid(
  xgb_3_wflow,
  resamples = reg_folds,
  grid = xgb_3_grid,
  metrics = metric_set(mae),
  control = control_stack_grid()
)

save(xgb_3_tune, file = "results/xgb_3_tune.rda")