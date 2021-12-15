
# set wd 
setwd("~/Desktop/STAT471/unemployment-project")

# read in the cleaned data
econ_data = read_tsv("data/clean/econ_data_clean.tsv")

#set seed 
set.seed(471)

# split into train and test
train_samples = sample(1:nrow(econ_data), 0.8*nrow(econ_data))
econ_train = econ_data %>% filter(row_number() %in% train_samples)
econ_test = econ_data %>% filter(!(row_number() %in% train_samples))

# save the train and test data
write_tsv(x = econ_train, file = "data/clean/econ_train.tsv")
write_tsv(x = econ_test, file = "data/clean/econ_test.tsv")