

run_preprocessing_steps <- function(data, sample = 0.25) {
  # function takes a list as input. List should be length 2
  # with named elements train and test. 
  # default sample size for preprocessing is 0.25
  # Returns pre-processed data sets as enframed tidy tables. 
  
  message("Running preprocessing function...")
  
  # ================== PROCESSING =======================
  processing_time <- Sys.time()

  train <- data$train
  test <- data$test
  
  id_cols <- train %>% select(Year, bbl, Address)
  outcome_cols <- train %>% select(Sold, SALE_PRICE)
  
  id_cols_test <- test %>% select(Year, bbl, Address)
  outcome_cols_test <- test %>% select(Sold, SALE_PRICE)
  
  
  # Numeric Columns ---------------------------------------------------------
  numeric_only_train <- 
    train %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  numeric_only_test <- 
    test %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  # sample the data for pre-processing
  set.seed(1989)
  numeric_only_train_sample <- sample_frac(numeric_only_train, sample)
  
  message("Treating with NZV and MedianImpute...")
  pprocess_num_only <- 
    preProcess(numeric_only_train_sample, method = c("nzv", "medianImpute")
               , thresh = 0.99, numUnique = 2, freqCut = 98/2
               , uniqueCut = 2, cutoff = 0.99)
  
  
  message("Treating with center, scale, NZV and medianImpute")
  pprocess_num_processed <- 
    preProcess(numeric_only_train_sample, method = c("center", "scale", "nzv", "medianImpute")
               , thresh = 0.99, numUnique = 2, freqCut = 98/2
               , uniqueCut = 2, cutoff = 0.99)
  
  message("Applying preprocessing  model to full data...")
  message("     ...1 of 2")
  num_train <- predict(pprocess_num_only, numeric_only_train) %>% bind_cols(id_cols, outcome_cols)
  num_test <- predict(pprocess_num_only, numeric_only_test) %>% bind_cols(id_cols_test, outcome_cols_test)
  
  message("     ...2 of 2")
  num_processed_train <- predict(pprocess_num_processed, numeric_only_train) %>% bind_cols(id_cols, outcome_cols)
  num_processed_test <- predict(pprocess_num_processed, numeric_only_test) %>% bind_cols(id_cols_test, outcome_cols_test)
  
  #num_train <- num_train %>% filter_at(vars(SMA_Price_2_year:Percent_Change_EMA_5), any_vars(!is.nan(.)))
  
  # record processing time:
  end_processing_time <- Sys.time()
  proc_time <- end_processing_time - processing_time
  message("     ...done.")
  message("Total processing time: ",round(proc_time, 2), units(proc_time))
  
  modeling_data <- 
    list("train_numeric_only" = tbl_df(num_train)
         , "test_numeric_only" = tbl_df(num_test)
         , "train_processed" = tbl_df(num_processed_train)
         , "test_processed" = tbl_df(num_processed_test)
         )
  
  return(modeling_data)
}


