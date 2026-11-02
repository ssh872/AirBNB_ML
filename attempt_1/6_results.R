library(tidyverse)
library(tidymodels)

load("results/rf_res.rda")
show_best(rf_res)

load("results/en_res.rda")
show_best(en_res)

load("results/xgb_2_tune.rda")
show_best(xgb_2_tune)
autoplot(xgb_tune)
xgb_tune

load("results/en_res2.rda")
show_best(en_res2)

load("results/mars_tune.rda")
show_best(mars_tune)
