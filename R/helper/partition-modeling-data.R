
# function takes any modeling data and returns a train and test set
# optional arguments to define training set range in years

partition_modeling_data <- function(data, train_years = 2003:2016, 
                                    test_years = 2017){
  train_data <- data %>% filter(Year %in% train_years)
  test_data <- data %>% filter(Year %in% test_years)
  modeling_data <- list("train" = train_data
                        ,"test" = test_data)
  modeling_data
}