# Load libraries
library(kableExtra)   # for printing tables
library(cowplot)      # for side by side plots
library(glmnetUtils)  # for running ridge and lasso
library(ISLR2)        # necessary for College data 
library(pROC)         # for ROC curves
library(tidyverse)  
library(dplyr)
library(glmnetUtils)

# load the data
unemploy_train = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_train.tsv")
unemploy_test = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_test.tsv")
variable_names = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/var_names.tsv")
source("~/Desktop/STAT471/unemployment-project/code/functions/plot_glmnet.R")

setwd("~/Desktop/STAT471/unemployment-project")
# remove date column 
unemploy_train = unemploy_train %>% dplyr::select(-date)
unemploy_test = unemploy_test %>% dplyr::select(-date)

## run least-squares/ linear regression

# run linear regression
lm_fit = lm(UNRATE ~ ., data = unemploy_train)
r_squared_lm = summary(lm_fit)$r.squared
# save the linear regression fit object
save(lm_fit, file = "~/Desktop/STAT471/unemployment-project/results/lm_fit.Rda")

#save significant coefs 
lm_sig = data.frame(summary(lm_fit)$coefficients[,4])%>% 
  rename(sig = "summary.lm_fit..coefficients...4.") %>% 
  filter(sig < .05 ) %>%
  arrange((sig))
lm_sig = cbind(rownames(lm_sig), lm_sig)
write_tsv(lm_sig, "results/lm-sig.tsv")

#check lm assumptions
par(mfrow = c(2, 2))
plot(lm_fit)

## run ridge regression

# run ridge fit
set.seed(3) # set seed before cross-validation for reproducibility
ridge_fit = cv.glmnet(UNRATE ~ ., alpha = 0, nfolds = 10, data = unemploy_train) 
#Save ridge fit 
save(ridge_fit, file = "~/Desktop/STAT471/unemployment-project/results/ridge_fit.Rda")

# create and save ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# extract features selected by ridge and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, unemploy_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/ridge-features-table.tsv")

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
lasso_fit = cv.glmnet(UNRATE ~ ., 
                      alpha = 1, 
                      nfolds = 10, 
                      data = unemploy_train) 
#Save lasso fit 
save(lasso_fit, file = "~/Desktop/STAT471/unemployment-project/results/lasso_fit.Rda")

# create and save lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# note: the optimal value of lambda is quite small 
lambda_lasso = lasso_fit$lambda.1se
sprintf("The value of lambda based on the one-standard-error rule: %f",
        lambda_lasso)

# how many features are selected if features are chosen according to the 1 se rule?
num_features = lasso_fit$nzero[lasso_fit$lambda == lasso_fit$lambda.1se]

# use `plot_glmnet` to visualize the lasso fitted coefficients, which by default will highlight the features selected by the lasso
lasso_coef_plot = plot_glmnet(lasso_fit, unemploy_train, features_to_plot = 6)
#save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/lasso_coef_plot.png", 
       plot = lasso_coef_plot , 
       device = "png", 
       width = 5, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, unemploy_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/lasso-features-table.tsv")

# run elastic net regression
set.seed(5) # set seed before cross-validation for reproducibility
elnet_fit = cva.glmnet(UNRATE ~ .,  # formula notation, as usual
                       nfolds = 10,               # number of folds
                       data = unemploy_train)   # data to run on
#plot the minimum CV error for each value of alpha
cv_elnet_plot = plot_cva_glmnet(elnet_fit)
# save plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/cv_elnet_plot.png", 
       plot = cv_elnet_plot , 
       device = "png", 
       width = 5, 
       height = 4)



# extract the `cv.glmnet` fit object based on the optimal `alpha` using `extract_best_elnet` 
elnet_fit_best = extract_best_elnet(elnet_fit)
# get lambda
lambda_elnet = elnet_fit_best$lambda.1se

# save elnet fit
save(elnet_fit_best, file = "~/Desktop/STAT471/unemployment-project/results/elnet_fit.Rda")

# extract best alpha, 0.343
alpha_elnet = elnet_fit_best$alpha

# extract features selected by elastic net and their coefficients
beta_hat_std = extract_std_coefs(elnet_fit_best, unemploy_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/elnet-features-table.tsv")

# plot coefs glmnet
elnet_coef_plot = plot_glmnet(elnet_fit_best, unemploy_train, features_to_plot = 6)
# save plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/elnet_coef_plot.png", 
       plot = elnet_coef_plot, 
       device = "png", 
       width = 5, 
       height = 4)