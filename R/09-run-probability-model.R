# This function creates the RADII modeling data

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p12_sale_price_model_base.rds") {
  message("Running the probability model on BASE data...")
  
  # check if modeling data exists
  if(!file.exists(model_data_infile)){
    message("TODO: function to run the probability model")
    break
  }
  
  # loading data ------------------------------------------------------------
  message("Reading base data...")
  base_data <- read_rds(model_data_infile)
  message("     ...done.")
  
  
  # partition to test & train -----------------------------------------------
  source("R/helper/partition-modeling-data.R")
  message("Partitioning modeling data into train and test...")
  modeling_data <- partition_modeling_data(base_data, train_years = 2003:2016, test_years = 2017)
  message("     ...done.")
  
  
  # pre-process steps -------------------------------------------------------
  message("Running Preprocessing steps...")
  source("R/helper/pre-process-modeling-data.R")
  processed_data <- run_preprocessing_steps(modeling_data)
  if(!is.null(processed_data)) message("SUCCESS! Processing done")
  #message("TODO: function to run the probability model")
}









