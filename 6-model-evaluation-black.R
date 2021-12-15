# load libraries
library(glmnetUtils)
library(tidyverse)

setwd("~/Desktop/STAT471/unemployment-project")
# load test data
unemploy_test = read_tsv("data/clean/econ_test.tsv")

#load least squares regression
load("results/lm_fit_black.Rda")

# load ridge fit object
load("results/ridge_fit_black.Rda")

# load lasso fit object
load("results/lasso_fit_black.Rda")

# load tree fit object
load("results/tree_fit_black.Rda")

# load pruned tree fit object
load("results/optimal_tree_black.Rda")

# load random forest
load("results/rf_fit_black.Rda")

# generate linear model predictions and test error
linear_predictions = predict(lm_fit_black, newdata = unemploy_test)
linear_RMSE = sqrt(mean((linear_predictions - unemploy_test$black_unemployment)^2))

# generate ridge regression model predictions and test error
ridge_predictions = predict(ridge_fit_black, 
                            newdata = unemploy_test, 
                            s = "lambda.1se")
ridge_RMSE = sqrt(mean((ridge_predictions - unemploy_test$black_unemployment)^2))

# generate lasso regression model predictions and test error
lasso_predictions = predict(lasso_fit_black, 
                            newdata = unemploy_test,
                            s = "lambda.1se")
lasso_RMSE = sqrt(mean((lasso_predictions -  unemploy_test$black_unemployment)^2))

# generate decision tree predictions and test error
tree_predictions = predict(tree_fit_black, 
                           newdata = unemploy_test)
tree_RMSE = sqrt(mean((tree_predictions -  unemploy_test$black_unemployment)^2))

# generate pruned decision tree predictions and test error
pruned_tree_predictions = predict(optimal_tree_black, 
                                  newdata = unemploy_test)
pruned_tree_RMSE = sqrt(mean((pruned_tree_predictions -  unemploy_test$black_unemployment)^2))

# generate random forest predictions and test error
rf_predictions = predict(rf_fit_black, 
                         newdata = unemploy_test)
rf_RMSE = sqrt(mean((rf_predictions -  unemploy_test$black_unemployment)^2))

# create table of these three model test errors
error_for_models = tribble(
  ~Model, ~RMSE, 
  #------/------- 
  "Linear", linear_RMSE,
  "Ridge", ridge_RMSE,
  "Lasso", lasso_RMSE, 
  "Random Forest", rf_RMSE
)

# print these metrics in nice table
error_for_models %>% kable(format = "latex", row.names = NA, 
                           booktabs = TRUE,
                           digits = 3,
                           col.names = c("Model type", 
                                         "Root mean squared error")) %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options = "HOLD_position")  %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/RMSE-models_black.pdf", 
             self_contained = T)
