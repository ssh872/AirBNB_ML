library(tidyverse)
library(tidymodels)

load("data/reg_clean.rda")
load("recipes/reg_recipe.rda")
load("results/en_res2.rda")
load("data/reg_clean_normal.rda")

test_data <- read_csv("data/test.csv") |> 
  mutate(
    # change numerical id var to character
    id = as.factor(id),
    
    # removing % and changing to numerics
    host_response_rate = str_remove(host_response_rate, "%") |> as.numeric(),
    host_acceptance_rate = str_remove(host_acceptance_rate, "%") |> as.numeric(),
    
    # extracting the number of bathrooms
    bathrooms_text = str_replace(bathrooms_text, "[H|h]alf-bath", '0.5'),
    num_bathrooms = str_remove_all(bathrooms_text, "[A-z]") |> as.numeric(),
    
    #number of amenities
    num_amenities = str_count(amenities, ",") + 1,
    
    # description
    has_description = as.numeric(!is.na(description)),
    len_description = str_length(description),
    
    across(where(is_logical), as.numeric),
    
    amenities = amenities |> 
      str_remove_all('[\\[\\]"]') |> 
      str_to_lower(),
    amenities = na_if(amenities, ""),
    
    x_city_center = case_when(
      listing_location == "chicago" ~ -87.627778, 
      listing_location == "jersey city" ~ -74.06,
      listing_location == "washington dc" ~ -77.016389
    ),
    y_city_center = case_when(
      listing_location == "chicago" ~ 41.881944, 
      listing_location == "jersey city" ~ 40.71,
      listing_location == "washington dc" ~ 38.904722
    ),
    dist_center = sqrt(
      ((longitude - x_city_center) * cos(y_city_center*pi/180))^2 +
        (latitude - y_city_center)^2)*69,
    
    about = as_factor(ifelse(is.na(host_about),0,1)),
    has_view        = str_detect(amenities, "view") |> as.numeric(),
    has_pool        = str_detect(amenities, "pool") |> as.numeric(),
    has_gym         = str_detect(amenities, "gym|fitness") |> as.numeric(),
    has_parking     = str_detect(amenities, "parking") |> as.numeric(),
    has_washer      = str_detect(amenities, "washer") |> as.numeric(),
    has_dishwasher  = str_detect(amenities, "dishwasher") |> as.numeric(),
    has_ev_charger  = str_detect(amenities, "ev charger") |> as.numeric(),
    mention_luxury = str_detect(amenities, "luxury") |> as.numeric(),
    has_boat_slip = str_detect(amenities, "boat slip") |> as.numeric(),
    has_rooftop = str_detect(amenities, "rooftop") |> as.numeric(),
    has_fire_pit =str_detect(amenities, "fire pit") |> as.numeric(),
    has_balcony = str_detect(amenities, "balcony") |> as.numeric(),
    
    avg_review = rowMeans(across(starts_with("review_scores_")), na.rm = TRUE),
    room_type     = as_factor(room_type),
    property_type = as_factor(property_type),
    across(where(is.character), as_factor),
    
    avg_review = rowMeans(across(starts_with("review_scores_")), na.rm = TRUE),
    
    count_verifications = str_count(host_verifications, ",") + 1,
    
    avail_rate_30 = 1 - (availability_30/30),
    avail_rate_60 = 1 - (availability_60/60),
    avail_rate_90 = 1 - (availability_90/90),
    avail_rate_365 = 1 - (availability_365/365),
    
    instant_book = as.numeric(instant_bookable),
    bath_per_bed = num_bathrooms / (bedrooms + 1),
    bed_per_accommodate = beds / (accommodates + 1)
  )

en2_mod <- linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("regression")

en2_wflow <- workflow() |>
  add_model(en2_mod) |>
  add_recipe(reg_recipe)

en2_final_wflow <- en2_wflow |>
  finalize_workflow(select_best(en_res2, metric = "mae"))

en_final_fit <- en2_final_wflow |>
  fit(data = reg_clean_normal)

en_predictions <- predict(en_final_fit, new_data = test_data) |> 
  bind_cols(test_data |> select(id)) |>
  mutate(price = exp(.pred) - 1) |> 
  select(id, price)

write_csv(en_predictions, "results/en2_submission.csv")
