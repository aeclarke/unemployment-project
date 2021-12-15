# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)
library(ggcorrplot)                     # for correlation plots

# read in the cleaned data
setwd("~/Desktop/STAT471/unemployment-project")
econ_data = read_tsv(file = "data/clean/econ_data_clean.tsv")
econ_data_train = read_tsv(file = "data/clean/econ_train.tsv")
econ_data_test = read_tsv(file = "data/clean/econ_test.tsv")

## Difference Between Total U.S. Unemployment and Black Unemployment

#Create plots to compare:

# set colors to use for graphs 
cols <- c("Total U.S."="#f04546","African American"="#3591d1")

# plot both unemployment rates for all years
all_year_plot = econ_data %>% 
  ggplot() + 
  geom_line(mapping = aes(x=date,y=unemployment_rate, colour = "Total U.S.")) +
  geom_line(mapping = aes(x=date,y=black_unemployment, color = "African American")) +
  ggtitle("Total U.S. vs. African American Unemployment Rate (1972-2021)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  scale_colour_manual(name="Unemployment Metric",values=cols)  +
  labs(
    x = "Date",
    y = "Unemployment Rate"
  )
# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/all-year-comparison-plot.png", 
       plot = all_year_plot, 
       device = "png", 
       width = 8, 
       height = 3)

# plot both unemployment rates after 2010
after_2010_plot= econ_data %>% filter(date > as.Date("2010-01-01")) %>%
  ggplot() + 
  geom_line(mapping = aes(x=date,y=unemployment_rate, colour = "Total U.S.")) +
  geom_line(mapping = aes(x=date,y=black_unemployment, color = "African American")) +
  ggtitle("Total U.S. vs. African American Unemployment Rate (2010-2021)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  scale_colour_manual(name="Unemployment Metric",values=cols)  +
  labs(
    x = "Date",
    y = "Unemployment Rate"
  )
# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/after-2010-comparison-plot.png", 
       plot = after_2010_plot, 
       device = "png", 
       width = 8, 
       height = 3)

# plot both unemployment rates after 2010
pct_dif_plot= econ_data %>% 
  ggplot() + 
  geom_line(mapping = aes(x=date,y=percent_dif_unemploy)) +
  ggtitle("Percent Difference Unemployment Rate Total U.S. vs. African American (1972-2021)") + 
  theme_bw() +
  theme(axis.title.x = element_text(size = 8)) +
  labs(
    x = "Date",
    y = "Percent Difference"
  )
# save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/percent-difference-plot.png", 
       plot = pct_dif_plot, 
       device = "png", 
       width = 8, 
       height = 3)

# Consider Phillips Curve - plot Unemployment vs. Inflation
# Does the Phillips Curve hold true for either metric?

# Plot unemployment vs. inflation for 1972-2021
phillips_curve_US = econ_data %>% 
  summarise(year = year(date), unemployment_rate, black_unemployment, inflation) %>% 
  group_by(year) %>% 
  summarise(unemployment_rate = mean(unemployment_rate), black_unemployment = mean(black_unemployment), 
            inflation = mean(inflation)) %>%
  ggplot() + 
  geom_line(mapping = aes(x=unemployment_rate,y=inflation, colour = "Total U.S.")) +
  ggtitle("Percent Difference Unemployment Rate Total U.S. vs. 
          African American (1972-2021)") + 
  theme_bw() +
  scale_colour_manual(name="Unemployment Metric",values=cols)  +
  theme(axis.title.x = element_text(size = 8)) +
  labs(
    x = "Unemployment Rate",
    y = "Inflation"
  )
# Save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/phillips-curve-us-plot.png", 
       plot = phillips_curve_US, 
       device = "png", 
       width = 8, 
       height = 3)

# Plot unemployment vs. inflation from 2010-2021
phillips_curve_black = econ_data %>% 
  summarise(year = year(date), unemployment_rate, black_unemployment, inflation) %>% 
  group_by(year) %>% 
  summarise(unemployment_rate = mean(unemployment_rate), 
            black_unemployment = mean(black_unemployment), 
            inflation = mean(inflation)) %>%
  ggplot() + 
  geom_line(mapping = aes(x=black_unemployment,y=inflation, colour = "African American")) +
  ggtitle("Percent Difference Unemployment Rate Total U.S. vs.
          African American (1972-2021)") + 
  theme_bw() +
  scale_colour_manual(name="Unemployment Metric",values=cols)  +
  theme(axis.title.x = element_text(size = 8)) +
  labs(
    x = "Unemployment Rate",
    y = "Inflation"
  )
# Save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/phillips-curve-black-plot.png", 
       plot = phillips_curve_black, 
       device = "png", 
       width = 8, 
       height = 3)

# Set colors to use for graphs 
cols <- c("Total U.S."="#f04546","African American"="#3591d1")

# Plot both unemployment rates after 2010
fed_funds_plot = econ_data %>% 
  ggplot() + 
  geom_line(mapping = aes(x=federal_funds_rate,y=unemployment_rate, 
                          colour = "Total U.S.")) +
  geom_line(mapping = aes(x=federal_funds_rate,y=black_unemployment, 
                          colour = "African American")) +
  ggtitle("Percent Difference Unemployment Rate Total U.S. vs. 
          African American (1972-2021)") + 
  theme_bw() +
  scale_colour_manual(name="Unemployment Metric",values=cols)  +
  theme(axis.title.x = element_text(size = 8)) +
  labs(
    x = "Federal Funds Rate",
    y = "Unemployment Rate"
  )
# Save the plot
ggsave(filename = 
         "~/Desktop/STAT471/unemployment-project/results/fed-funds-plot.png", 
       plot = fed_funds_plot, 
       device = "png", 
       width = 8, 
       height = 3)

## Calculate Summary Stats for Train Data

# Calculate mean unemployment 
# All data 
mean_all_years = econ_data_train %>%
  summarise("Time" = "1972-2021", "Total U.S." = mean(unemployment_rate), 
            "African American" = mean(black_unemployment))

# After 2010
mean_last_10years = econ_data_train %>% 
  filter(date > as.Date("2010-01-01")) %>%
  summarise("Time" = "2010-2021", "Total U.S." = mean(unemployment_rate), 
            "African American" = mean(black_unemployment))

mean_unemploy = rbind(mean_all_years, mean_last_10years) 

# Format and save a chart that saves mean unemployment
mean_unemploy  %>% 
  kable(format = "latex", row.names = NA, 
        booktabs = TRUE, digits = 2, 
        caption = "Mean Unemployment for Both Metrics") %>%
  kable_styling(position = "center") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/mean-unemployment-chart.pdf", 
             self_contained = T)

# Calculate mean difference between total unemployment and African American unemployment
mean_dif = econ_data_train %>%
  summarise(mean_difference = mean(percent_dif_unemploy))

top_5_dif = econ_data_train %>%
  select(date, percent_dif_unemploy) %>%
  arrange(desc(percent_dif_unemploy)) %>%
  head(5)

top_5_dif_after_2010 = econ_data_train %>% filter(date > as.Date("2010-01-01")) %>%
  select(date, percent_dif_unemploy) %>%
  arrange(desc(percent_dif_unemploy)) %>%
  head(5)

# Which year has highest difference between black and total unemployment?
top_5_dif = econ_data_train %>%
  select(date, percent_dif_unemploy) %>%
  arrange(desc(percent_dif_unemploy)) %>%
  head(5)

top_5_dif_after_2010 = econ_data_train %>% filter(date > as.Date("2010-01-01")) %>%
  select(date, percent_dif_unemploy) %>%
  arrange(desc(percent_dif_unemploy)) %>%
  head(5)






## Needs to work EDA wish did 

#Display the unemployment distribution in train with a plot. What is the median age? 

econ_data_train %>%
  ggplot(aes(x = unemployment_rate)) + 
  # create histogram for age distribution
  geom_histogram(binwidth = 1) +
  labs(x = "Unemployment Rate", y = "Count") + 
  # add vertical line to indicate median age
  geom_vline(xintercept = median(hd_train$AGE), color = "red", 
             linetype = "dashed") +
  theme_bw() + theme(legend.position = "none")

# Use a plot to explore the relationship between x and x in `econ_data_train`. What does this plot suggest?
  
```