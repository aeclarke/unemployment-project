# load libraries
library(lubridate)
library(tidyverse)

# note: majority of cleaning done when downloaded data from fredr

# set wd 
setwd("~/Desktop/STAT471/unemployment-project")

# load raw econ data
econ_data_raw = read_tsv(file = "data/raw/econ_data_raw.tsv")

# Inflation is reported yearly, so set each month's inflation equal to yearly inflation
yearly_inflation = econ_data_raw %>% summarise(year = year(date), inflation) %>% 
  group_by(year) %>% summarise(yearly_inflation = mean(inflation, na.rm = T)) %>% select(yearly_inflation) %>% pull()
# Consensus estimate for 2021 inflation
yearly_inflation[50] = 6.8
# Create sequence of inflation numbers
yearly_inflation = rep(yearly_inflation, each=12)
# Decemebr 2021 data has not be released, so leave out last month
yearly_inflation = yearly_inflation[1:nrow(econ_data_raw)]
# Add this sequence to econ data raw
econ_data_raw = econ_data_raw %>% mutate(inflation = yearly_inflation)

# Since there are 2,003 variables and many variables related, remove all variables with NA 
econ_data_clean = econ_data_raw %>% 
  select_if(function(x) all(!is.na(x))) %>% #remove columns with any NA
  #create column for percent difference between total and African American unemployment
  mutate(percent_dif_unemploy = (black_unemployment/unemployment_rate -1)*100 )

# Write cleaned data to file
write_tsv(econ_data_clean, file = "data/clean/econ_data_clean.tsv")