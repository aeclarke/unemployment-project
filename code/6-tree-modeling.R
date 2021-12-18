# load libraries
library(kableExtra)   # for printing tables
library(cowplot)      # for side by side plots
library(glmnetUtils)  # for running ridge and lasso
library(ISLR2)        # necessary for College data 
library(pROC)         # for ROC curves
library(tidyverse)  
library(dplyr)
library(randomForest)        
library(rpart)             
library(rpart.plot)    
library(grid)
library(ggplotify)
library(gbm)   

# read in the test and training data
unemploy_train = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_train.tsv")
unemploy_test = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/econ_test.tsv")
variable_names = read_tsv(file = "~/Desktop/STAT471/unemployment-project/data/clean/var_names.tsv")
# load function
source("~/Desktop/STAT471/unemployment-project/code/functions/plot_glmnet.R")

# remove date column
unemploy_train = unemploy_train %>% select(-date)
unemploy_test = unemploy_test %>% select(-date)

## Regression Tree 

# fitting and plotting regression tree
tree_fit= rpart(UNRATE ~ ., data = unemploy_train)
# save basic tree
save(tree_fit, file = "~/Desktop/STAT471/unemployment-project/results/tree_fit.Rda")
# create plot 
tree_fit_plot = rpart.plot(tree_fit)
# top 10 variable importance 
top_10_tree = rownames(data.frame(tree_fit$variable.importance)%>% arrange(desc(tree_fit.variable.importance)) %>% head(10))

# create cp table 
cp_table = printcp(tree_fit) %>% as_tibble()
cp_table %>% 
  ggplot(aes(x = nsplit+1, y = xerror, 
             ymin = xerror - xstd, ymax = xerror + xstd)) + 
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") + 
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") + 
  theme_bw()
# save optimal tree info
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>% 
  head(1)
optimal_tree_info
optimal_tree = prune(tree_fit, cp = optimal_tree_info$CP)
# save the pruned tree fit
save(optimal_tree, file = "~/Desktop/STAT471/unemployment-project/results/optimal_tree_black.Rda")
#create plot of pruned tree
pruned_tree_fit_plot = rpart.plot(optimal_tree)


## Random forest model

# Determine optimal m value using for loop
set.seed(1) # set seed  
mvalues = seq(1,28, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(UNRATE~ ., mtry = m, data = unemploy_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}
# plot OOB error by mtry
mtry_plot_rf = tibble(m = mvalues, oob_err = oob_errors) %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  theme_bw()
# Save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/mtry_plot_rf.png", 
       plot = mtry_plot_rf , 
       device = "png", 
       width = 5, 
       height = 4)

#set mtry equal to optimal value, leave ntree = 500 (default)
rf_fit = randomForest(UNRATE  ~ ., mtry = 15, importance = TRUE, data = unemploy_train)
# Check to make sure using enough trees
# OOB error by number of trees 
OBB_trees_rf_fit = plot(rf_fit)
# save the random forest
save(rf_fit, file = "~/Desktop/STAT471/unemployment-project/results/rf_fit.Rda")

# plot variable importance, onlt include top 10 variables 
rf_var_imp = varImpPlot(rf_fit, n.var = 10)


## Boosting model 

#tuning interaction depth by trying out a few different values
set.seed(1)
gbm_fit_1 = gbm(UNRATE ~ .,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = unemploy_train)
gbm_fit_2 = gbm(UNRATE   ~ .,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 2,
              shrinkage = 0.1,
              cv.folds = 5,
              data = unemploy_train)
gbm_fit_3 = gbm(UNRATE   ~ .,
              distribution = "gaussian",
              n.trees = 1000,
              interaction.depth = 3,
              shrinkage = 0.1,
              cv.folds = 5,
              data = unemploy_train)

#extract the CV errors from each of these objects by using the `cv.error` field
ntrees = 1000
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3)
)

#plot CV erors 
gmb_cv_error = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + theme_bw()
#save plot 
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/gmb_cv_error.png", 
       plot = gmb_cv_error , 
       device = "png", 
       width = 8, 
       height = 5)

# Can visualize the CV error using `gbm.perf`, which both makes a plot and outputs the optimal number of trees, but set to FALSE
gbm_fit_optimal = gbm_fit_3
# Save optimal gbm model 
save(gbm_fit_optimal, file = "~/Desktop/STAT471/unemployment-project/results/gbm_fit_optimal.Rda")
optimal_num_trees = gbm.perf(gbm_fit_3, plot.it = FALSE) 

# Interpret tuned model
summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE)

# Create ppartial dependence plots  
pdp_employees_mining_logging = as.ggplot(plot(gbm_fit_optimal, i.var = "CES1000000006", n.trees = optimal_num_trees))
pdp_real_consumption_fixed_capital = as.ggplot(plot(gbm_fit_optimal, i.var = "A262RL1A225NBEA", n.trees = optimal_num_trees))
pdp_gov_social_benefits = as.ggplot(plot(gbm_fit_optimal, i.var = "A1589C1A027NBEA", n.trees = optimal_num_trees))
pdp_farm_output = as.ggplot(plot(gbm_fit_optimal, i.var = "A365RG3A086NBEA", n.trees = optimal_num_trees))

# Save partial dependence plots 
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/pdp_employees_mining_logging.png", 
       plot = pdp_employees_mining_logging, 
       device = "png")
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/pdp_real_consumption_fixed_capital.png", 
       plot = pdp_real_consumption_fixed_capital, 
       device = "png")
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/pdp_gov_social_benefits", 
       plot = pdp_gov_social_benefits, 
       device = "png")
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/pdp_farm_output", 
       plot = pdp_farm_output, 
       device = "png")