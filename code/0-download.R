# load libraries
library(tidyverse)
library(quantmod)
library(fredr)
library(dplyr)
library(ggplot2)
library(purrr)
library(xts)
library(ggplot2)

# set FRED API key
# note: this is my FRED key, need to insert your own
fredr_set_key("1c21ee5ac3042aee4c56fa8d2d3a5124")

#get total unemployment rate data
#only keep observations that have the same date 
unemployment_rate = fredr_series_observations(series_id = "UNRATE") %>%
  filter(date >= as.Date("1954-06-01")) %>%
  select(date, value) %>%
  rename(unemployment_rate = value) 

#include inflation, based on knowledge of Phillips curve
inflation = fredr_series_observations(series_id = "FPCPITOTLZGUSA") %>%
  filter(date >= as.Date("1954-06-01")) %>%
  select(date, value) %>%
  rename(inflation = value) 

#include federal funds rate
fed_funds = fredr_series_observations(series_id = "FEDFUNDS") %>%
  filter(date >= as.Date("1954-06-01")) %>%
  select(date, value) %>%
  rename(federal_funds_rate = value) 

#select relevant FRED releases 
releases = c(
  "Employment Situation",
  "Gross Domestic Product"
)

#Save id for all releases of data 
release_name_id = fredr_releases() %>% select(name, id) 
#Only keep relevant release ids 
release_id = release_name_id %>% filter(name %in% releases) %>% select(id) %>% pull()

#Find series IDs want to pull 
string_id = c()
for (i in 1:length(release_id)){
  val = fredr_release_series(release_id = release_id[i]) %>% select(id) %>% pull()
  string_id = append(string_id, val)
}

#save variable info, in case need later
var_info = rbind(fredr_release_series(release_id = release_id[1])%>% select(id, title, frequency, units),
fredr_release_series(release_id = release_id[2])%>% select(id, title, frequency, units))
#write variable info
write_tsv(x = var_info, file = "~/Desktop/STAT471/unemployment-project/data/clean/var_info.tsv")

#fredr has a limit of 120 requests per minute, so have to split up requests into seperate tasks 
string_id_1 = string_id[c(1:120)]
string_id_2 = string_id[c(121:240)]
string_id_3 = string_id[c(241:360)]
string_id_4 = string_id[c(361:480)]
string_id_5 = string_id[c(481:600)]
string_id_6 = string_id[c(601:720)]
string_id_7 = string_id[c(721:840)]
string_id_8 = string_id[c(841:960)]
string_id_9 = string_id[c(961:1080)]
string_id_10 = string_id[c(1081:1200)]
string_id_11 = string_id[c(1201:1320)]
string_id_12 = string_id[c(1321:1440)]
string_id_13 = string_id[c(1441:1560)]
string_id_14 = string_id[c(1561:1680)]
string_id_15 = string_id[c(1681:1800)]
string_id_16 = string_id[c(1801:2000)]

#create series of dates to use, use only after May 1954, to exclude Recession of 1953
date = seq(as.Date("1954-06-01"), as.Date("2021-11-1"), by = "months")
#create tibble that includes date series
df = tibble(date)

#pull data for all years for each set of string_ids - this will take a long time 
for (i in 1:length(string_id_1)){
  add = fredr_series_observations(series_id = string_id_1[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_2)){
  add = fredr_series_observations(series_id = string_id_2[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_3)){
  add = fredr_series_observations(series_id = string_id_3[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_4)){
  add = fredr_series_observations(series_id = string_id_4[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_5)){
  add = fredr_series_observations(series_id = string_id_5[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_6)){
  add = fredr_series_observations(series_id = string_id_6[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_7)){
  add = fredr_series_observations(series_id = string_id_7[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_8)){
  add = fredr_series_observations(series_id = string_id_8[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_9)){
  add = fredr_series_observations(series_id = string_id_9[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_10)){
  add = fredr_series_observations(series_id = string_id_10[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_11)){
  add = fredr_series_observations(series_id = string_id_11[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_12)){
  add = fredr_series_observations(series_id = string_id_12[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_13)){
  add = fredr_series_observations(series_id = string_id_13[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_14)){
  add = fredr_series_observations(series_id = string_id_14[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_15)){
  add = fredr_series_observations(series_id = string_id_15[i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}
for (i in 1:length(string_id_16)){
  add = fredr_series_observations(series_id = string_id_16 [i]) %>%
    select(date, value) %>% 
    filter(date >= as.Date("1954-06-01"))
  df = merge(df, add, by = "date", all.x = TRUE)
}

#add total unemployment, inflation, and fed_funds to the data set
df = merge(df, unemployment_rate, by = "date", all.x = TRUE)
df = merge(df, inflation, by = "date", all.x = TRUE)
df = merge(df, fed_funds, by = "date", all.x = TRUE)

#change names to correct series id
names(df) = c("date", string_id, "unemployment_rate", "inflation", "federal_funds_rate")

#write raw data file
write_tsv(x = df, file = "~/Desktop/STAT471/unemployment-project/data/raw/econ_data_raw.tsv")