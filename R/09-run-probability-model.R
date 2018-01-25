# This function creates the RADII modeling data

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p12_sale_price_model_base.rds") {
  
  ## LEFT OFF. WHY NO 2016 SALES DATA?
  
  base_data <- read_rds(model_data_infile)
  
  partition_modeling_data <- function(data){
    train_data <- data %>% filter(Year%in%)
    test_data 
  }
  
  modeling_data <- partition_modeling_data(base_data)
  
  
  
  message("TODO: function to run the probability model")
}