# load libraries
library(lubridate)
library(tidyverse)
library(dplyr)

# set wd 
setwd("~/Desktop/STAT471/unemployment-project")

# load raw econ data
econ_data_raw = read_tsv(file = "data/raw/econ_data_raw.tsv")

#Inflation not reported until 1960, so only keep observations after 1960
econ_data_clean = econ_data_raw %>% 
  filter(date >= as.Date("1960-01-01")) %>%  #keeps observations after 1960
  filter(date < as.Date("2021-01-01"))  #keeps observations before 2021

# Count NA by column 
na_count = econ_data_clean %>%
  summarise_all(funs(sum(!is.na(.)))) 

# Identify columns with NA and their count
rownames = names(na_count)
na_count = cbind(rownames, transpose(na_count))

# Look to see if any amount of observations are repeated
histogram_na_count = na_count %>%
  ggplot(aes(x = V1)) +
  geom_histogram(bins = 15, fill = "grey", col =  "black") +
  labs(x = "Number of Non-Missing Variables", 
       y = "Count") +
  theme_bw()
# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/histogram_na_count.png", 
       plot = histogram_na_count, 
       device = "png", 
       width = 5, 
       height = 3)

#notice lots of variables have only 61 non-NA varaiables, yearly
#notice lots of variables have only 244, non-NA variables, quarterly 
non_na_count = na_count %>% group_by(V1) %>%
  count(V1) %>% arrange(desc(n))

#save number of years and number of quarters 
number_years = (2021-1960)
number_quarters = (number_years)*4 

reported_yearly = na_count %>% filter(V1 == number_years) %>% summarise(rownames) %>% pull()
reported_quarterly = na_count %>% filter(V1 == number_quarters) %>% summarise(rownames) %>% pull()

#turn yearly data into monthly data
clean_yearly = tibble(econ_data_clean %>% dplyr::select(date) %>% pull())
for(i in 1:length(reported_yearly)){
  varname = as.name(reported_yearly[i])
  yearly = econ_data_clean %>% 
    dplyr::select(date, varname) %>% 
    rename(local = varname) %>% 
    mutate(year= year(date))  %>% 
    group_by(year) %>% 
    summarise(mean(local, na.rm = T))
  new_values = rep(yearly %>% 
                     dplyr::select("mean(local, na.rm = T)") %>% pull(), 
                   each = 12)
  clean_yearly = cbind(clean_yearly, new_values)
}
# change column names
names(clean_yearly) = c("date", reported_yearly)

#turn quarterly data into monthly data
clean_quarterly = tibble(econ_data_clean %>% dplyr::select(date) %>% pull())
for(i in 1:length(reported_quarterly)){
  varname = as.name(reported_quarterly[i])
  quarterly = econ_data_clean %>% 
    dplyr::select(date, varname) %>% 
    rename(local = varname) %>% 
    filter(month(date) %in% c(1, 4, 7, 10))
  new_values = rep(quarterly %>% 
                     dplyr::select(local) %>% pull(), 
                   each = 3)
  clean_quarterly = cbind(clean_quarterly, new_values)
}
# change column names
names(clean_quarterly) = c("date", reported_quarterly)

#remove data name from clean yearly and quarterly
clean_yearly = tibble(clean_yearly) %>%
  select_if(function(x) all(!is.na(x))) %>%
  dplyr::select(-date)  

clean_quarterly = tibble(clean_quarterly) %>% 
  select_if(function(x) all(!is.na(x))) %>%
  dplyr::select(-date)  

# Since there are 2,003 variables remove all variables with NA
econ_data_clean = econ_data_clean %>% 
  select_if(function(x) all(!is.na(x))) #remove columns with any NA

#add back yearly and quarterly data
cleaned_data = cbind(econ_data_clean, clean_yearly, clean_quarterly)
# variables with sd of 0 are meaningless
cleaned_data = cleaned_data[, !sapply(cleaned_data, function(x) {sd(x) == 0})]

# Write cleaned data to file
write_tsv(cleaned_data, file = "data/clean/econ_data_clean.tsv")