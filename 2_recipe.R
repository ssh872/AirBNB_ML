library(tidyverse)
library(recipes)

load("data/reg_clean.rda")

reg_recipe <- recipe(price ~ ., data = reg_clean) |> 
  step_rm(maximum_nights_avg_ntm, minimum_nights_avg_ntm, maximum_maximum_nights,
          minimum_maximum_nights, amenities, host_about, description, id, listing_location,
          bathrooms_text, x_city_center, y_city_center, host_neighbourhood) |>
  step_date(host_since, first_review, last_review,
            features = c("year", "month", "dow"),
            keep_original_cols = FALSE) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |> 
  step_unknown(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors())

  #step_normalize(all_numeric_predictors())

save(reg_recipe, file = "recipes/reg_recipe.rda")
