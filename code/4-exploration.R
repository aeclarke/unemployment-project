# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)

# read in the cleaned data
setwd("~/Desktop/STAT471/unemployment-project")
econ_train = read_tsv(file = "data/clean/econ_train.tsv")
econ_test = read_tsv(file = "data/clean/econ_test.tsv")

# count number of test and train observations and number of filters
test_obs = nrow(econ_train)
train_obs = nrow(econ_test)
total_obs = test_obs + train_obs
num_features = ncol(econ_train)

 
# Variation in response variable 
histogram_unemploy = econ_train %>%
  ggplot(aes(x = UNRATE)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Unemployment Rate (%)", 
       y = "Count") +
  # add vertical line at the median value for unemployment rate
  geom_vline(xintercept = median(econ_train$UNRATE), color = "#f04546") + 
  theme_bw()
# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/histogram_unemploy.png", 
       plot = histogram_unemploy, 
       device = "png", 
       width = 8, 
       height = 5)

#accesss independence of time series data
pct <- function(x) {x / lag(x) - 1}
combinded = rbind(econ_train, econ_test) 
mean_change = combinded %>% 
  mutate(change = UNRATE - lead(UNRATE)) %>% 
  dplyr::select(change) %>% na.omit() %>%
  summarise(mean(abs(change)))  
sd_UNRATE = sd(combinded$UNRATE)

# which 10 years have the highest unemployment rates
top_10_unemploy = econ_train %>%
  mutate(year = year(date)) %>% #add year columns
  group_by(year) %>%  #group by year 
  summarise(UNRATE = mean(UNRATE)) %>% 
  ungroup() %>%
  arrange(desc(UNRATE)) %>%
  head(10)
top_10_unemploy %>% 
  kable(format = "latex", row.names = NA, 
        booktabs = TRUE, digits = 1) %>%
  kable_styling(position = "center") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/top_10_unemploy.pdf", 
             self_contained = T)

# Calculate mean unemployment 

# All data 
mean_all_years = econ_train %>%
  summarise("Time" = "1960-2020", "Unemployment Rate" = mean(UNRATE))
# After 2010
mean_last_10years = econ_train %>% 
  filter(date > as.Date("2010-01-01")) %>%
  summarise("Time" = "2010-2020", "Unemployment Rate" = mean(UNRATE))
# combine two metrics
mean_unemploy = rbind(mean_all_years, mean_last_10years) 

mean_unemploy  %>% 
  kable(format = "latex", row.names = NA, 
        booktabs = TRUE, digits = 2) %>%
  kable_styling(position = "center") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/mean-unemployment-chart.pdf", 
             self_contained = T)

#Plot unemployment by year 

# set colors to use for graphs 
cols <- c("Total U.S."= "#3591d1","Mean Unemployment (1972-2021)"= "#f04546")

# plot both unemployment rates for all years
all_year_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x=date,y=UNRATE, colour = "Total U.S.")) +
  geom_hline(yintercept = median(econ_train$UNRATE), 
             colour = "#f04546", linetype='dashed') +
  ggtitle("Total U.S. Unemployment (1960-2020)") + 
  theme_bw() +
  scale_colour_manual(name="Metric",values=cols)  +
  labs(
    x = "Date",
    y = "Unemployment Rate"
  )
# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/all-year-comparison-plot.png", 
       plot = all_year_plot, 
       device = "png", 
       width = 8, 
       height = 5)

# Plot phillips curve (inflation by unemployment) 
phillips_curve_US = econ_train %>% 
  summarise(year = year(date), UNRATE, FPCPITOTLZGUSA) %>% 
  group_by(year) %>% 
  summarise(unemployment_rate = mean(UNRATE), 
            inflation = mean(FPCPITOTLZGUSA)) %>% ungroup() %>%
  ggplot() + 
  geom_line(mapping = aes(x=unemployment_rate,y=inflation), color = "#3591d1") +
  ggtitle("Inflation vs. Unemployment Rate") + 
  theme_bw() +
  labs(
    x = "Unemployment Rate",
    y = "Inflation"
  )
# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/phillips-curve-us-plot.png", 
       plot = phillips_curve_US, 
       device = "png", 
       width = 8, 
       height = 5)
 
# Plot unemployment rate vs. federal funds rate
fed_funds_plot = econ_train %>% filter(date > as.Date("2000-01-01")) %>% 
  ggplot() + 
  geom_line(mapping = aes(x= FEDFUNDS,y=UNRATE), color = "#3591d1") +
  ggtitle("Unemployment Rate vs. Federal Funds Rate (2000-2020)") + 
  theme_bw() +
  labs(
    x = "Federal Funds Rate",
    y = "Unemployment Rate"
  )
# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/fed-funds-plot.png", 
       plot = fed_funds_plot, 
       device = "png", 
       width = 8, 
       height = 5)

# Examine variation in features 

set.seed(3)
#randomly select features to look at variation of
vars_for_variation = names(econ_train)[sample(1:length(econ_train), 4, replace=FALSE)]

#plot histogram of selected features distribution 
h1 = econ_train %>%
  ggplot(aes(x = CES0600000006)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Number goods producing employees (thousands)", 
       y = "Count") +
  # add vertical line at the median value for unemployment rate
  geom_vline(xintercept = median(econ_train$CES0600000006), color = "#f04546") + 
  theme_bw()
h2 = econ_train %>%
  ggplot(aes(x = A133RL1Q225SBEA)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Change Real Motor Vechicle Output (%)", 
       y = "Count") +
  # add vertical line at the median value for unemployment rate
  geom_vline(xintercept = median(econ_train$A133RL1Q225SBEA), color = "#f04546") + 
  theme_bw()
h3 = econ_train %>%
  ggplot(aes(x = A014RE1A156NBEA)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Change in private inventories (%)", 
       y = "Count") +
  # add vertical line at the median value for unemployment rate
  geom_vline(xintercept = median(econ_train$A014RE1A156NBEA), color = "#f04546") + 
  theme_bw()
h4 = econ_train %>%
  ggplot(aes(x = A001RO1Q156NBEA)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Change Real Gross National Product (%) ", 
       y = "Count") +
  # add vertical line at the median value for unemployment rate
  geom_vline(xintercept = median(econ_train$A001RO1Q156NBEA), color = "#f04546") + 
  theme_bw()
#combine all the histograms into one plot
variation_features_plot = plot_grid(h1, h2, h3, h4, nrow = 2)
# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/var-features-plot.png", 
       plot = variation_features_plot , 
       device = "png", 
       width = 8, 
       height = 5)

# Examine unemployment by thousands of employees 

# logging plot
employees_logging_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CES1011330001, y = UNRATE), color = "#3591d1") +
  ggtitle(" Logging") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Logging Employees (thousands)",
    y = "Unemployment Rate"
  )

# shipping and boating 
employees_shipping_boating_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CES3133660001, y = UNRATE), color = "#3591d1") +
  ggtitle("Shipping and Boating") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Shipping and Boating Employees (thousands)",
    y = "Unemployment Rate"
  )

# information plot
employees_information_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CEU5000000001, y = UNRATE), color = "#3591d1") +
  ggtitle("Information") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Information Employees (thousands)",
    y = "Unemployment Rate"
  )

# information plot after 2000, since likely meaningless before then
employees_information_plot_after_2000 = econ_train %>% 
  filter(date > as.Date("2000-01-01")) %>%
  ggplot() + 
  geom_line(mapping = aes(x= CEU5000000001, y = UNRATE), color = "#3591d1") +
  ggtitle("Information (2000-2020)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Information Employees (thousands)",
    y = "Unemployment Rate"
  )

# federal plot, suggests numbner might increase during high unemployment 
employees_federal_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CES9091000001, y = UNRATE), color = "#3591d1") +
  ggtitle("Federal") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Federal Employees (thousands)",
    y = "Unemployment Rate"
  )

employees_plot = plot_grid(employees_logging_plot, employees_shipping_boating_plot, employees_information_plot, employees_information_plot_after_2000, employees_federal_plot, rnow = 3)

# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/employees_plot.png", 
       plot = employees_plot, 
       device = "png", 
       width = 8, 
       height = 5)

# examine unemployment by hours of work, issue is still classifed as employed even if hours change

# hours manufacturing plot
hours_production_manufacturing_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= AWHMAN, y = UNRATE), color = "#3591d1") +
  ggtitle("Manufacturing") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Hours Production",
    y = "Unemployment Rate"
  )

# hours mining and logging plot
hours_production_mining_logging_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CES1000000007, y = UNRATE), color = "#3591d1") +
  ggtitle("Mining/Logging") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Hours Production",
    y = "Unemployment Rate"
  )

# hours construction plot
hours_production_construction_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= CEU2000000007, y = UNRATE), color = "#3591d1") +
  ggtitle("Construction") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Hours Production",
    y = "Unemployment Rate"
  )

hours_production_plot = plot_grid(hours_production_manufacturing_plot, hours_production_mining_logging_plot, hours_production_construction_plot, nrow = 1)

# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/hours_production_plot.png", 
       plot = hours_production_plot, 
       device = "png", 
       width = 12, 
       height = 4)

# government social benefits: to persons: Federal: Benefits from social insurance funds: Unemployment insurance, as increases so should 
social_benefits_plot = econ_train %>% 
  filter(A1589C1A027NBEA <mean(econ_train$A1589C1A027NBEA)*5) %>% #remove a crazy outliar 
  ggplot() + 
  geom_line(mapping = aes(x= A1589C1A027NBEA, y = UNRATE), color = "#3591d1") +
  ggtitle("Social Benefits to Persons") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Government social benefits: to person",
    y = "Unemployment Rate"
  )

# net government saving: Imputations
net_gov_savings_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= A2010C1A027NBEA, y = UNRATE), color = "#3591d1") +
  ggtitle("Net government saving") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Net government saving",
    y = "Unemployment Rate"
  )

# real Consumption of Fixed Capital: Private
consumption_fixed_capital_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= A024RL1A225NBEA, y = UNRATE), color = "#3591d1") +
  ggtitle("Real Consumption of Fixed Capital: Private") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Real Consumption of Fixed Capital: Private",
    y = "Unemployment Rate"
  )

# real Disposable Personal Income
disposable_personal_income_plot = econ_train %>% 
  ggplot() + 
  geom_line(mapping = aes(x= A067RL1A156NBEA, y = UNRATE), color = "#3591d1") +
  ggtitle("Real Disposable Personal Income") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 8)) +
  labs(
    x = "Real Disposable Personal Income",
    y = "Unemployment Rate"
  )

#combine plots
gdp_plots = plot_grid(social_benefits_plot, net_gov_savings_plot, consumption_fixed_capital_plot, disposable_personal_income_plot, nrow = 2)

# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/gdp_plots.png", 
       plot = gdp_plots, 
       device = "png", 
       width = 10, 
       height = 6)