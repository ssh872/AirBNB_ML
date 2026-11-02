library(tidymodels)

load("results/mars_4_tune.rda")
show_best(mars_4_tune)

load("results/rf_tune.rda")
show_best(rf_tune)

load("results/xgb_3_tune.rda")

xgb_table <- show_best(xgb_3_tune, metric = "mae") |> 
  select(mtry, min_n, tree_depth, learn_rate, mean, std_err) |> 
  knitr::kable(col.names = c("Number of Predictors", "Minimum Node Size", "Tree Depth", 
                             "Learning Rate", "Mean", "Standard Error"))

save(xgb_table, file = "memo/table/xgb_table.rda")

preds <- read_csv("results/ensemble_2_preds.csv")
summary(preds$price)

load("data/reg_clean.rda")
summary(exp(reg_clean$price) - 1)
