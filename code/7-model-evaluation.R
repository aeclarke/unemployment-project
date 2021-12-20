# load libraries
library(glmnetUtils)
library(tidyverse)

setwd("~/Desktop/STAT471/unemployment-project")
# load test data
unemploy_train = read_tsv(file = "data/clean/econ_train.tsv")
unemploy_test = read_tsv("data/clean/econ_test.tsv")

#load least squares regression
load("results/lm_fit.Rda")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# load elastic net
load("results/elnet_fit.Rda")

# load tree fit object
load("results/tree_fit.Rda")

# load pruned tree fit object
load("results/optimal_tree.Rda")

# load random forest
load("results/rf_fit.Rda")

# load boosting model 
load("results/gbm_fit_optimal.Rda")


# generate intercept only predictions and test error

# intercept-only prediction error
intercept = mean(unemploy_train$UNRATE)
intercept_only_RMSE = sqrt(mean((intercept  - unemploy_test$UNRATE)^2))

# generate linear model predictions and test error
linear_predictions = predict(lm_fit, newdata = unemploy_test)
linear_RMSE = sqrt(mean((linear_predictions - unemploy_test$UNRATE)^2))

# generate ridge regression model predictions and test error
ridge_predictions = predict(ridge_fit, 
                            newdata = unemploy_test, 
                            s = "lambda.1se")
ridge_RMSE = sqrt(mean((ridge_predictions - unemploy_test$UNRATE)^2))

# generate lasso regression model predictions and test error
lasso_predictions = predict(lasso_fit, 
                            newdata = unemploy_test,
                            s = "lambda.1se")
lasso_RMSE = sqrt(mean((lasso_predictions -  unemploy_test$UNRATE)^2))

# generate lasso regression model predictions and test error
elnet_predictions = predict(elnet_fit, 
                            newdata = unemploy_test, 
                            alpha = alpha_elnet)
elnet_RMSE = sqrt(mean((elnet_predictions  -  unemploy_test$UNRATE)^2))

# generate decision tree predictions and test error
tree_predictions = predict(tree_fit, 
                           newdata = unemploy_test)
tree_RMSE = sqrt(mean((tree_predictions -  unemploy_test$UNRATE)^2))

# generate random forest predictions and test error
rf_predictions = predict(rf_fit, 
                         newdata = unemploy_test)
rf_RMSE = sqrt(mean((rf_predictions -  unemploy_test$UNRATE)^2))

# generate gbm predictions and test error
gbm_predictions = predict(gbm_fit_optimal, 
                          newdata = unemploy_test)
gbm_RMSE = sqrt(mean((gbm_predictions -  unemploy_test$UNRATE)^2))

# create table of these three model test errors
error_for_models = tribble(
  ~Model, ~RMSE, 
  #------/------- 
  "Intercept Only", intercept_only_RMSE, 
  "Least Squares", linear_RMSE,
  "Ridge", ridge_RMSE,
  "Lasso", lasso_RMSE,
  "Elastic Net", elnet_RMSE,
  "Random Forest", rf_RMSE,
  "Boosting", gbm_RMSE
)

# save table
error_for_models %>%
  write_tsv("results/error_for_models.tsv")

# print these metrics in nice table
error_for_models %>% kable(format = "latex", row.names = NA, 
                           booktabs = TRUE,
                           digits = 3,
                           col.names = c("Model type", 
                                         "Root mean squared error")) %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options = "HOLD_position")  %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/RMSE-models.pdf", 
             self_contained = T)
