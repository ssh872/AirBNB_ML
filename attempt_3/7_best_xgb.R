library(tidymodels)
library(tidyverse)
library(embed)

load("results/xgb_3_tune.rda")
load("data/clean_test.rda")
load("recipes/reg_recipe.rda")
load("data/reg_clean.rda")

doParallel::registerDoParallel(4)

best_xgb_mod <- boost_tree(
  trees = 2000,
  mtry = tune(),
  min_n = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  stop_iter = 50 
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

best_xgb_wf <- workflow() |> 
  add_model(best_xgb_mod) |> 
  add_recipe(reg_recipe)

final_xgb_wf <- best_xgb_wf |> 
  finalize_workflow(select_best(xgb_3_tune, metric = "mae"))

final_xgb_fit <- final_xgb_wf |> fit(data = reg_clean)

xgb_preds <- predict(final_xgb_fit, new_data = clean_test) |> 
  bind_cols(clean_test |> select(id)) |> 
  mutate(price = .pred) |> 
  select(id, price)

write_csv(xgb_preds, file = "results/xgb_3_preds.csv")
