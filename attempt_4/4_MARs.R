library(tidymodels)
library(tidyverse)
library(stacks)

load("data/reg_clean.rda")
load("data/reg_folds.rda")
load("recipes/reg_recipe.rda")

doParallel::registerDoParallel(2)

mars_4_mod <- mars(
  num_terms = tune(),
  prod_degree = tune()
) |> 
  set_engine("earth") |> 
  set_mode("regression")

mars_4_wflow <- workflow() |> 
  add_recipe(reg_recipe) |> 
  add_model(mars_4_mod)

set.seed(203)

# mars_4_grid <- grid_regular(
#   num_terms(range = c(50,200)),
#   prod_degree(range = c(1,2)),
#   levels = 5
# )

mars_4_grid <- grid_regular(
  num_terms(range = c(20,200)),
  prod_degree(range = c(2,3)),
  levels = 5
)

mars_4_tune <- tune_grid(
  mars_4_wflow,
  resamples = reg_folds,
  grid = mars_4_grid,
  metrics = metric_set(mae),
  control = control_stack_grid()
)

save(mars_4_tune, file = "results/mars_4_tune.rda")
