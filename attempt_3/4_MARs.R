library(tidymodels)
library(tidyverse)
library(stacks)

load("data/reg_clean_2.rda")
load("data/reg_2_fold.rda")
load("recipes/reg_recipe_2.rda")

doParallel::registerDoParallel(4)

mars_3_mod <- mars(
  num_terms = tune()
) |> 
  set_engine("earth") |> 
  set_mode("regression")

mars_3_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(mars_3_mod)

set.seed(123)

mars_3_grid <- grid_regular(
  num_terms(range = c(50,200)),
  levels = 5
)

mars_3_tune <- tune_grid(
  mars_3_wflow,
  resamples = reg_2_fold,
  grid = mars_3_grid,
  metrics = metric_set(mae),
  control = control_stack_grid()
)

save(mars_3_tune, file = "results/mars_3_tune.rda")
