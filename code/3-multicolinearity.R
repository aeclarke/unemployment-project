# load libraries
library(dplyr)
library(tidyverse)
library(ggcorrplot)                     # for correlation plots

# read in the cleaned data
setwd("~/Desktop/STAT471/unemployment-project")
#download data sets 
econ_data_train = read_tsv(file = "data/clean/econ_train.tsv")
econ_data_test = read_tsv(file = "data/clean/econ_test.tsv")

#threshold for too high of correlation is .85
#do not include date, response variables, or inflation and federal funds rate
econ_data_corr = econ_data_train %>% select(-c(date, black_unemployment, unemployment_rate, inflation, federal_funds_rate, percent_dif_unemploy)) 

#run loops until no variables have a corrleation with any other variable higher than 0.85
#when variables are highly correlates keeps the first variable and removes all variables that are highly correlated 
for(i in 1:50){
  corr = round(cor(econ_data_corr), 3)
  corr_nums = data.frame(corr)
  issues = corr_nums %>% 
    select_if(function(x) ((sum((abs(x) >.85))> 1)))
  if(length(issues) < 1){
    break
  }
  first_issue = as.name(names(issues)[1])
  #first_issue is not saved as a local variable, so have to copy from global env
  remove = rownames(issues %>% mutate(local = !!first_issue) %>% filter(abs(local) > .85))
  remove = remove[2:length(remove)]
  econ_data_corr = econ_data_corr  %>% select(-remove)
}

# create corr plot of features 
final_econ_data_corr = econ_data_train %>% select(names(econ_data_corr), inflation, federal_funds_rate)
corr = round(cor(final_econ_data_corr), 3)
corr_nums = data.frame(corr)

# visualize correlation matrix
corr_plot = ggcorrplot(corr,
                       type = "lower",
                       colors = c("#f04546", "white", "#3591d1")
) 
# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/corr-plot.png", 
       plot = corr_plot, 
       device = "png", 
       width = 8, 
       height = 8)

# create corr plot of response variables and features 
response_econ_data_corr = econ_data_train %>% 
  select(c(names(econ_data_corr), black_unemployment, unemployment_rate, inflation, 
           federal_funds_rate, percent_dif_unemploy)) 

corr = round(cor(response_econ_data_corr), 3)

corr = as.matrix(data.frame(corr) %>%  select(c("black_unemployment", "unemployment_rate", "percent_dif_unemploy")))
response_corr_plot = ggcorrplot(corr,
                                colors = c("#f04546", "white", "#3591d1")
) 

# save the plot
ggsave(filename = "~/Desktop/STAT471/unemployment-project/results/response-corr-plot.png", 
       plot = response_corr_plot, 
       device = "png", 
       width = 8, 
       height = 5)

#save  variables want to keep for modeling 
vars_to_keep = c("date", names(econ_data_corr), "black_unemployment", "unemployment_rate", "inflation", 
                 "federal_funds_rate", "percent_dif_unemploy")

#save train and test data so now only includes selected variables
econ_train = econ_data_train %>%
  select(vars_to_keep)
econ_test = econ_data_test %>%
  select(vars_to_keep)


## Defining Variables 

var_names = tribble(
  ~ID, ~variable_name, ~description,
  "AHECONS","hourly_earnings", "Average Hourly Earnings of Production and Nonsupervisory Employees, Construction (hours)",
  "AWHMAN", "hours_production_manufacturing", "Average Weekly Hours of Production and Nonsupervisory Employees, Manufacturing (hours)",
  "AWHNONAG",  "hours_production_private", "Average Weekly Hours of Production and Nonsupervisory Employees, Total Private (hours)",
  "AWOTMAN","overtime_hours_production", "Average Weekly Overtime Hours of Production and Nonsupervisory Employees, Manufacturing (hours)", 
  "CES0600000006","goods_producing_employees", " Production and Nonsupervisory Employees, Goods-Producing", 
  "CES0600000039","women_to_all_goods", "Women Employees-To-All Employees Ratio: Goods-Producing", 
  "CES1000000006", "employees_mining_logging", "Production and Nonsupervisory Employees, Mining and Logging (thousands of persons)", 
  "CES1000000007","hours_production_mining_logging", "Average Weekly Hours of Production and Nonsupervisory Employees, Mining and Logging (hours)", 
  "CES1000000010", "women_employees_mining_logging",  "Women Employees, Mining and Logging (thousands of persons)", 
  "CES1000000039", "women_to_all_mining_logging", "Women Employees-To-All Employees Ratio: Mining and Logging",
  "CES2000000006", "employee_construction", "Production and Nonsupervisory Employees, Construction (thousands of persons)", 
  "CES2000000007", "hours_production_construction", "Average Weekly Hours of Production and Nonsupervisory Employees, Construction (hours)",
  "CES3100000039", "women_to_all_durable_goods", "Women Employees-To-All Employees Ratio: Durable Goods.",       
  "CES4142000007", "hours_production_wholesale_trade", "Average Weekly Hours of Production and Nonsupervisory Employees, Wholesale Trade (hours)", 
  "CES4300000007", "hours_production_transport_warehousing", "Average Weekly Hours of Production and Nonsupervisory Employees, Transportation and Warehousing (hours)", 
  "CES4422000007", "hours_production_ultilites", "Average Weekly Hours of Production and Nonsupervisory Employees, Utilities (hours)",        
  "CES5000000007", "hours_production_information", "Average Weekly Hours of Production and Nonsupervisory Employees, Information (hours)",        
  "CES5000000010", "women_employees", "Women Employees, Information (thosands of persons)",      
  "CES5500000007", "hours_production_financial", "Average Weekly Hours of Production and Nonsupervisory Employees, Financial Activities (hours)",       
  "CES6500000007", "hours_production_education_health", "Average Weekly Hours of Production and Nonsupervisory Employees, Education and Health Services (hours)",
  "CES6500000039", "women_to_all_education_health", "Women Employees-To-All Employees Ratio: Education and Health Services",        
  "CES9091000001", "employees_federal", "All Employees, Federal (thousands of persons)",        
  "CES9091100001", "employees_federal_no_postal", "All Employees, Federal, Except U.S. Postal Service (thousands of persons)",      
  "CES9091912001", "employees_postal", "All Employees, U.S. Postal Service (thousands of persons)",       
  "CEU1011330001", "employees_logging", "All Employees, Logging (thousands of persons)",      
  "CEU2000000007", "hours_production_construction", "Average Weekly Hours of Production and Nonsupervisory Employees, Construction (hours)",    
  "LNS14000006", "black_unemployment", "Unemployment Rate - Black or African American (percent)",  
  "UNRATE", "unemployment_rate", "Unemployment Rate.",    
  "FPCPITOTLZGUSA", "inflation", "Inflation, consumer prices for the United States (percent)",           
  "FEDFUNDS", "federal_funds_rate", "Federal Funds Effective Rate (percent)",    
  "NA", "percent_dif_unemploy", "Percent difference between the Black/African American Unemployment Rate and the total U.S. Unemployment Rate (percent)"
)


#change names from ID to variable name
clean_var_names = var_names %>% select(variable_name) %>% pull()
names(econ_train) = c("date", clean_var_names)
names(econ_test) = c("date", clean_var_names)

#save id, variable name, and description in a seperate data frame 
write_tsv(x = var_names, file = "data/clean/var_names.tsv")
# resave the train and test data 
write_tsv(x = econ_train, file = "data/clean/econ_train.tsv")
write_tsv(x = econ_test, file = "data/clean/econ_test.tsv")


#save variable names as a nice chart
class_names %>% kable(format = "latex", row.names = NA,
                      booktabs = TRUE,
                      digits = 2) %>%
  column_spec(1, width = "10em") %>%
  kable_styling(latex_options="scale_down",
                "striped") %>%
  save_kable(file =
               "~/Desktop/STAT471/unemployment-project/results/variable_description.pdf")

#Additonal details to add to report 

#Unemployment
"The unemployment rate represents the number of unemployed as a percentage of the labor force. Labor force data are restricted to people 16 years of age and older, who currently reside in 1 of the 50 states or the District of Columbia, who do not reside in institutions (e.g., penal and mental facilities, homes for the aged), and who are not on active duty in the Armed Forces."

#Inflation 
"Inflation as measured by the consumer price index reflects the annual percentage change in the cost to the average consumer of acquiring a basket of goods and services that may be fixed or changed at specified intervals, such as yearly. The Laspeyres formula is generally used."

#Federal Funds
"The federal funds rate is the interest rate at which depository institutions trade federal funds (balances held at Federal Reserve Banks) with each other overnight. When a depository institution has surplus balances in its reserve account, it lends to other banks in need of larger balances. In simpler terms, a bank with excess cash, which is often referred to as liquidity, will lend to another bank that needs to quickly raise liquidity. The rate that the borrowing institution pays to the lending institution is determined between the two banks; the weighted average rate for all of these types of negotiations is called the effective federal funds rate. The effective federal funds rate is essentially determined by the market but is influenced by the Federal Reserve through open market operations to reach the federal funds rate target."

#Overtime
"Overtime hours represent that portion of average weekly hours that exceeded regular hours and for which overtime premiums were paid. If an employee were to work on a paid holiday at regular rates, receiving as total compensation his holiday pay plus straight-time pay for hours worked that day, no overtime hours would be reported. Overtime hours data are collected only from manufacturing establishments."

#Employee count
"Production and related employees include working supervisors and all nonsupervisory employees (including group leaders and trainees) engaged in fabricating, processing, assembling, inspecting, receiving, storing, handling, packing, warehousing, shipping, trucking, hauling, maintenance, repair, janitorial, guard services, product development, auxiliary production for plant's own use (for example, power plant), recordkeeping, and other services closely associated with the above production operations.Nonsupervisory employees include those individuals in private, service-providing industries who are not above the working-supervisor level. This group includes individuals such as office and clerical workers, repairers, salespersons, operators, drivers, physicians, lawyers, accountants, nurses, social workers, research aides, teachers, drafters, photographers, beauticians, musicians, restaurant workers, custodial workers, attendants, line installers and repairers, laborers, janitors, guards, and other employees at similar occupational levels whose services are closely associated with those of the employees listed"

#Hours production: 
"Average weekly hours relate to the average hours per worker for which pay was received and is different from standard or scheduled hours. Factors such as unpaid absenteeism, labor turnover, part-time work, and stoppages cause average weekly hours to be lower than scheduled hours of work for an establishment. Group averages further reflect changes in the workweek of component industries. Average weekly hours are the total weekly hours divided by the employees paid for those hours.

Production and related employees include working supervisors and all nonsupervisory employees (including group leaders and trainees) engaged in fabricating, processing, assembling, inspecting, receiving, storing, handling, packing, warehousing, shipping, trucking, hauling, maintenance, repair, janitorial, guard services, product development, auxiliary production for plant's own use (for example, power plant), recordkeeping, and other services closely associated with the above production operations.

Nonsupervisory employees include those individuals in private, service-providing industries who are not above the working-supervisor level. This group includes individuals such as office and clerical workers, repairers, salespersons, operators, drivers, physicians, lawyers, accountants, nurses, social workers, research aides, teachers, drafters, photographers, beauticians, musicians, restaurant workers, custodial workers, attendants, line installers and repairers, laborers, janitors, guards, and other employees at similar occupational levels whose services are closely associated with those of the employees listed."

#Women Employees to All:
"To obtain estimates of women worker employment, the ratio of weighted women employees to the weighted all employees in the sample is assumed to equal the same ratio in the universe. The current month's women worker ratio, thus, is estimated and then multiplied by the all-employee estimate. The weighted-difference-link-and-taper formula (described in the source) is used to estimate the current month's women worker ratio. This formula adds the change in the matched sample's women worker ratio (the weighted-difference link) to the prior month's estimate, which has been slightly modified to reflect changes in the sample composition (the taper)"




