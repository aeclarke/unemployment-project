# Load libraries
library(kableExtra)   # for printing tables
library(cowplot)      # for side by side plots
library(glmnetUtils)  # for running ridge and lasso
library(ISLR2)        # necessary for College data 
library(pROC)         # for ROC curves
library(tidyverse)  
library(dplyr)

# load the data
unemploy_train = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_train.tsv")
unemploy_test = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_test.tsv")
variable_names = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/var_names.tsv")

source("~/Desktop/STAT471/unemployment-project/code/functions/plot_glmnet.R")

# remove date column 
unemploy_train = unemploy_train %>% select(-date)
unemploy_test = unemploy_test %>% select(-date)

## run least-squares/ linear regression

# run linear regression
lm_fit = lm(UNRATE ~ ., data = unemploy_train)
r_squared_lm = summary(lm_fit)$r.squared
# save the linear regression fit object
save(lm_fit, file = "~/Desktop/STAT471/unemployment-project/results/lm_fit.Rda")

## run ridge regression

# run ridge fit
set.seed(3) # set seed before cross-validation for reproducibility
ridge_fit = cv.glmnet(UNRATE ~ ., alpha = 0, nfolds = 10, data = unemploy_train) 
#Save ridge fit 
save(ridge_fit, file = "~/Desktop/STAT471/unemployment-project/results/ridge_fit.Rda")

# plot MSE vy log lambda 
error_plot_ridge =plot(ridge_fit)
# save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/error_plot_ridge.png", 
       plot = error_plot_ridge, 
       device = "png", 
       width = 5, 
       height = 4)

# lambda use 1se rule 
lambda = ridge_fit$lambda.1se
sprintf("The value of lambda based on the one-standard-error rule: %f",
        lambda)

# visualize the ridge regression fitted coefficients, highlighting 6 features using the `features_to_plot` argument
ridge_coef_plot = plot_glmnet(ridge_fit, unemploy_train, features_to_plot = 6)
#save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/ridge_coef_plot.png", 
       plot = ridge_coef_plot, 
       device = "png", 
       width = 5, 
       height = 4)

## run lasso regression

# run lasso fit
set.seed(5) # set seed before cross-validation for reproducibility
lasso_fit = cv.glmnet(UNRATE ~ ., alpha = 1, nfolds = 10, data = unemploy_train) 
#Save lasso fit 
save(lasso_fit, file = "~/Desktop/STAT471/unemployment-project/results/lasso_fit.Rda")

# plot MSE vy log lambda 
plot(lasso_fit)

# note: the optimal value of lambda is quite small 
lambda_lasso = lasso_fit$lambda.1se
sprintf("The value of lambda based on the one-standard-error rule: %f",
        lambda_lasso)

# how many features are selected if features are chosen according to the 1 se rule?
num_features = lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]

# use `plot_glmnet` to visualize the lasso fitted coefficients, which by default will highlight the features selected by the lasso
lasso_coef_plot = plot_glmnet(lasso_fit, unemploy_train, features_to_plot = 8)
#save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/lasso_coef_plot.png", 
       plot = lasso_coef_plot , 
       device = "png", 
       width = 5, 
       height = 4)

# collect names of top coefficents for each model into a tibble 
lm_coefs_top10 = rownames(data.frame(coef(lm_fit)[-1]) %>% 
                            rename(coef = coef.lm_fit...1.) %>% arrange(desc(abs(coef))) %>% head(10))
ridge_coefs_top10 = rownames(data.frame(coef(ridge_fit, s = "lambda.1se")[-1,1]) %>% 
                               rename(coef = coef.ridge_fit..s....lambda.1se....1..1.) %>% 
                               arrange(desc(abs(coef))) %>% head(10))
lasso_coefs_top10 = rownames(data.frame(coef(lasso_fit, s = "lambda.1se")[-1,1]) %>% 
                               rename(coef = coef.lasso_fit..s....lambda.1se....1..1.) %>% 
                               arrange(desc(abs(coef))) %>% head(10))

# save top 10 coeficents into a chart 
top_10_by_model = tibble("least_squares" = lm_coefs_top10, 
                         "ridge" = ridge_coefs_top10, 
                         "lasso" = lasso_coefs_top10) %>%
  kable(format = "latex", row.names = NA, 
        booktabs = TRUE) %>%
  kable_styling(position = "center") %>%
  kable_styling(latex_options="scale_down",
                "striped") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/top_10_coefs.pdf", 
             self_contained = T)