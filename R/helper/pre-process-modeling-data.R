

run_preprocessing_steps <- function(data) {
  # function takes a list as input. List should be length 2
  # with named elements train and test. 
  # Returns pre-processed data sets as enframed tidy tables. 
  
  message("Running preprocessing...")
  
  # ================== PROCESSING =======================
  processing_time <- Sys.time()

  train <- data$train
  test <- data$test
  
  id_cols <- train %>% select(Year, bbl, Address)
  outcome_cols <- train %>% select(Sold, `SALE PRICE`)
  
  # Numeric Columns ---------------------------------------------------------
  numeric_only_train <- 
    train %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  numeric_only_test <- 
    test %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  # for dev purposes:
  set.seed(1989)
  # numeric_only_train <- sample_frac(numeric_only_train,0.01)

  message("Treating with NZV and MedianImpute...")
  pprocess_num_only <- 
    preProcess(numeric_only_train, method = c("nzv", "medianImpute")
               , thresh = 0.99, numUnique = 2, freqCut = 98/2
               , uniqueCut = 2, cutoff = 0.99)
  
  num_train <- predict(pprocess_num_only, numeric_only_train)
  num_test <- predict(pprocess_num_only, numeric_only_test)
  
  
  # Processed numeric columns -----------------------------------------------
  num_train_processed <- 
    train %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  num_test_processed <- 
    test %>% select(-one_of(c(names(id_cols), names(outcome_cols)))) %>% 
    select_if(.predicate = is.numeric) %>% 
    as.data.frame()
  
  # dev
  set.seed(1989)
  # num_train_processed <- sample_frac(num_train_processed, 0.01)
  
  message("Treating with BoxCox, Center, Scale, BagImpute and NZV...")
  pprocess_num_train_processed <- 
    preProcess(num_train_processed
               , method = c("BoxCox", "center", "scale", "bagImpute", "nzv")
               )
  
  num_train_processed <- predict(pprocess_num_train_processed, num_train_processed)
  num_test_processed <- predict(pprocess_num_train_processed, num_test_processed)
  
  end_processing_time <- Sys.time()
  proc_time <- end_processing_time - processing_time
  message("     ...done.")
  message("Total processing time: ",round(proc_time, 2), units(proc_time))
  
  modeling_data <- 
    list("train_numeric_only" = tbl_df(num_train)
         , "test_numeric_only" = tbl_df(num_test)
         , "train_num_processed" = tbl_df(num_train_processed)
         , "test_num_processed" = tbl_df(num_test_processed)
         )
  retrun(modeling_data)
}


