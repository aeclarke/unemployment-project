# load libraries
library(dplyr)
library(tidyverse)
library(ggcorrplot)  # for correlation plots
library(data.table)

# read in the cleaned data
setwd("~/Desktop/STAT471/unemployment-project")
#download data sets 
econ_data_train = read_tsv(file = "data/clean/econ_train.tsv")
econ_data_test = read_tsv(file = "data/clean/econ_test.tsv")
var_info = read_tsv(file = "data/clean/var_info.tsv")

#threshold for too high of correlation is .9
#do not include date, response variables, or inflation and federal funds rate beause do not want these removed
econ_data_corr = econ_data_train %>% 
  dplyr::select(-c(date, unemployment_rate, inflation, federal_funds_rate)) 

#run loops until no variables have a corrleation with any other variable higher than 0.9
#when variables are highly correlates keeps the first variable and removes all variables that are highly correlated 
handled = c()
for(i in 1:831){
  corr = round(cor(econ_data_corr), 3)
  corr_nums = data.frame(corr)
  issues = rownames(corr_nums %>% 
                      select_if(function(x) ((sum(abs(x) >.9)) > 1)))
  if(length(issues) < 1){
    break
  }
  first_issue = issues[1]
  #first_issue is not saved as a local variable, so have to copy from global env
  remove = rownames(corr_nums %>% 
                      dplyr::select(first_issue) %>% 
                      rename(local = first_issue) %>% 
                      filter(abs(local) > .9))
  handled = c(handled, first_issue)
  econ_data_corr = econ_data_corr  %>% dplyr::select(-remove)
}

# list variables want to keep for modeling 
vars_to_keep = c("date", handled, "unemployment_rate", "inflation", 
                 "federal_funds_rate")

# pull data descriptions
fred_var_names = var_info %>% filter(id %in% handled)

## Add additional variables I pulled 
add_names = tribble(
  ~id, ~title, ~frequency, ~units,
  "FEDFUNDS", "Federal Funds Effective Rate", "Monthly", "Percent",
  "FPCPITOTLZGUSA", "Inflation, consumer prices for the United States", "Yearly", "Percent",      
  "UNRATE", "Unemployment Rate.", "Monthly", "Percent" )

#combine both 
var_names = rbind(fred_var_names, add_names)

#save id, title, frequency, units in a seperate data frame 
write_tsv(x = var_names, file = "data/clean/var_names.tsv")

var_names %>% kable(format = "latex", row.names = NA,
      booktabs = TRUE,
      digits = 2) %>%
  column_spec(1, width = "10em") %>%
  kable_styling(latex_options="scale_down",
                "striped") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/variable_description.pdf")


#change variables all to IDs and order same as var_names
econ_data_train = econ_data_train %>%  
  dplyr::select(vars_to_keep) %>%
  rename(c("FEDFUNDS" = "federal_funds_rate", "FPCPITOTLZGUSA" = "inflation", "UNRATE" =  "unemployment_rate")) 

econ_data_test = econ_data_test %>% 
  dplyr::select(vars_to_keep) %>% 
  rename(c("FEDFUNDS" = "federal_funds_rate", "FPCPITOTLZGUSA" = "inflation", "UNRATE" =  "unemployment_rate"))


# create corr plot of features 
final_econ_data_corr = econ_data_train %>% dplyr::select(-date)
corr = round(cor(final_econ_data_corr), 3)
corr_nums = data.frame(corr)

# visualize correlation matrix
corr_plot = ggcorrplot(corr,
                       type = "lower",
                       colors = c("#f04546", "white", "#3591d1")
) +
  theme(axis.text.x=element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=7),
        panel.grid.major=element_blank()) 

# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/corr-plot.png", 
       plot = corr_plot, 
       device = "png", 
       width = 10, 
       height = 8)

# too many variables, so take subsample of 30 variables to show
set.seed(1)
show = sample(1:nrow(corr), 30, replace=FALSE)
corr_sub = corr[show, show]
corr_plot_subsample = ggcorrplot(corr_sub,
                       type = "lower",
                       colors = c("#f04546", "white", "#3591d1")
) +
  theme(axis.text.x=element_text(size=7, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=7),
        panel.grid.major=element_blank()) 
# save subsample plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/corr-plot-subsample.png", 
       plot = corr_plot_subsample, 
       device = "png", 
       width = 10, 
       height = 8)

#create plot with only response variables 
corr_response = as.matrix(data.frame(corr) %>%  dplyr::select(UNRATE))

corr_response_with_names = cbind(names = rownames(corr_response), corr_response)
write_tsv(x = data.frame(corr_response_with_names) , file = "data/clean/corr_response.tsv")

response_corr_plot = ggcorrplot(corr_response,
                                colors = c("#f04546", "white", "#3591d1")) + #do not show names
  theme(axis.text.x=element_text(size=1, angle=45, vjust=1, hjust=1),
        axis.text.y=element_text(size=8),
        panel.grid.major=element_blank()) 

# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/response-corr-plot.png", 
       plot = response_corr_plot, 
       device = "png", 
       width = 6, 
       height = 3)

# resave the train and test data 
write_tsv(x = econ_data_train , file = "data/clean/econ_train.tsv")
write_tsv(x = econ_data_test, file = "data/clean/econ_test.tsv")
