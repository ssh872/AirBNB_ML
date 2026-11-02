library(stacks)
library(tidyverse)
library(tidymodels)
library(embed)

load("results/mars_4_tune.rda")
load("results/xgb_3_tune.rda")
load("results/rf_tune.rda")
load("data/reg_clean.rda")
load("data/clean_test.rda")

doParallel::registerDoParallel(8)

ensemble_2_mod <- stacks() |> 
  add_candidates(xgb_3_tune) |> 
  add_candidates(mars_4_tune) |> 
  add_candidates(rf_tune) |> 
  blend_predictions() |> 
  fit_members()

ensemble_2_preds <- predict(ensemble_2_mod, new_data = clean_test) |> 
  bind_cols(clean_test |> select(id)) |> 
  mutate(price = expm1(.pred)) |> 
  select(id, price)

write_csv(ensemble_2_preds, file = "results/ensemble_2_preds.csv")