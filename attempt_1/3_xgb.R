library(tidymodels)
library(tidyverse)

load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")
load("data/reg_folds.rda")

doParallel::registerDoParallel(7)

set.seed(123)

xgb_model <- boost_tree(
  trees = 200,
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune()
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

xgb_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(xgb_model)

xgb_grid <- grid_regular(
  finalize(mtry(), reg_clean),
  min_n(range = c(2, 15)),
  learn_rate(range = c(-2, -1)),
  levels = 3
)

xgb_tune <- tune_grid(
  xgb_wflow,
  resamples = reg_folds,
  grid = xgb_grid,
  metrics = metric_set(mae)
)

save(xgb_tune, file = "results/xgb_tune.rda")
