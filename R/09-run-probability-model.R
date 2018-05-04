# This function runs the BASE PROBABILITY model

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                  , dev = FALSE 
                                  , helper_title = NA) {
  
  message(paste0("\n#============= PROBABILITY model: ",helper_title," =================#"))
  
  if(!file.exists(model_data_infile)){
    stop("Following Input data not available: ", model_data_infile)
  } 
  
  
  start_prob_time <- Sys.time()
  
  # supply dev=TRUE to run the model on sample data. good for quick model adjustments
  if(dev == TRUE) {
    warning("You are taking a sample of the modeling data, for dev purposes. 09-run-probability-model.R")
    message("Using sample data to run models...")
    model_data_list <- read_rds("data/aux data/model_data_list-temp.rds")
    
  } else { 
    # loading data ------------------------------------------------------------
    message("Reading base data...")
    base_data <- read_rds(model_data_infile)
    message("     ...done.")
    
    # for dev purposes:
    set.seed(1987)
    model_data_list_samp <- sample_frac(base_data, 0.01)
    # write to temp file for development:
    write_rds(model_data_list_samp, "data/aux data/model_data_list-temp.rds")
    
  }
  
  # TRAIN H2O Models ----------------------------------------------------------
  
  ## NOTE: YOU MAY NEED TO INSTALL JAVA. > system("sudo apt-get install default-jre")
  
  message("Initiating h2o clusters...")
  suppressMessages({
    suppressWarnings({
      sink(".sink-output")
      h2o.init(nthreads = -1) #Number of threads -1 means use all cores on your machine
      options("h2o.use.data.table"=TRUE)
      h2o.no_progress()
      h2o.removeAll()
      sink(NULL)
    })
  })
  
  # split in to train, test and val by year
  train <- base_data %>% filter(!Year%in%c(2016, 2017))
  validate <- base_data %>% filter(Year==2016)
  test <- base_data %>% filter(Year==2017)
  
  # train
  X <- train %>% select(-Sold, -`SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
  X_names <- names(X)
  Y <- train %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
  names(Y) <- Y_names <- "Sold"
  training_frame <- as.h2o(bind_cols(X,Y))
  
  # validate
  X_val <- validate %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
  X_val_names <- names(X_val)
  Y_val <- validate %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
  names(Y_val) <- Y_val_names <- "Sold"
  validation_frame <- as.h2o(bind_cols(X_val,Y_val))
  
  # run first model. take top 80% of influential vairbles and re-run in subsequent step
  message("     Training Model for Variable Selection Step...")
  start_time <- Sys.time()
  bst <- h2o.randomForest(x = X_names,
                          y = Y_names,
                          training_frame = training_frame,
                          validation_frame = validation_frame, 
                          model_id = "h2o_rf_fit",
                          ntrees = 200,
                          stopping_rounds = 10,
                          stopping_metric = "AUC",
                          seed = 1)
  end_time <- Sys.time()
  message("Model training time:" , round(end_time - start_time, 2), units(end_time - start_time))
  
  # re-run with top variables
  message("   Running model with variables accounting for 80% of VarImp...")
  best_variables <- h2o.varimp(bst) %>% 
    mutate(Cumulative = cumsum(percentage)) %>% 
    filter(Cumulative<=0.80) %>% 
    select(variable)
  
  base_data_simplified <- base_data %>% 
    select(bbl, Year, Borough, Sold, `SALE PRICE`
           , Building_Type:Annual_Sales
           , best_variables$variable)
  
  # re-split into train test val
  train <- base_data_simplified %>% filter(!Year%in%c(2016, 2017))
  validate <- base_data_simplified %>% filter(Year==2016)
  test <- base_data_simplified %>% filter(Year==2017)
  
  # train
  X <- train %>% select(-Sold, -`SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
  X_names <- names(X)
  Y <- train %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
  names(Y) <- Y_names <- "Sold"
  training_frame <- as.h2o(bind_cols(X,Y))
  
  # validate
  X_val <- validate %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
  X_val_names <- names(X_val)
  Y_val <- validate %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
  names(Y_val) <- Y_val_names <- "Sold"
  validation_frame <- as.h2o(bind_cols(X_val,Y_val))
  
  message("     Training Actual Model...")
  start_time <- Sys.time()
  bst <- h2o.randomForest(x = X_names,
                          y = Y_names,
                          training_frame = training_frame,
                          validation_frame = validation_frame, 
                          model_id = "h2o_rf_fit",
                          ntrees = 1000,
                          stopping_rounds = 200,
                          stopping_metric = "AUC",
                          seed = 1)
  end_time <- Sys.time()
  message("Model training time:" , round(end_time - start_time, 2), units(end_time - start_time))
  
  # test data
  X_test <- test %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
  X_test_names <- names(X_test)
  Y_test <- test %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
  names(Y_test) <- Y_test_names <- "Sold"
  test_frame <- as.h2o(bind_cols(X_test,Y_test))
  test_frame$preds <- predict(bst, newdata = test_frame, type = "probs")$predict
  probs <- as.numeric(as.data.frame(predict(bst, newdata = test_frame, type = "probs")$p1)$p1)
  
  actual <- recode(as.numeric(as.data.frame(test_frame)$Sold), 0, 1)
  pred <- recode(as.numeric(as.data.frame(test_frame)$preds), 0, 1)
  
  final_model_object <- list()
  final_model_object[["bbl"]] <- as_data_frame(test_frame$bbl)
  final_model_object[["Building_Type"]] <- as_data_frame(test_frame$Building_Type)
  final_model_object[["Borough"]] <- as_data_frame(test_frame$Borough)
  final_model_object[["actual"]] <- actual
  final_model_object[["probs"]] <- probs
  final_model_object[["pred"]] <- pred
  final_model_object[["model"]] <- bst
  
  roc <- pROC::roc(actual, probs)
  
  message("Probability Model AUC: ", round(as.numeric(roc$auc), 4))
  
  message("Writing model results to disk...")
  write_rds(final_model_object, outfile)
  message("     ...done.")
  
  # output time
  end_prob_time <- Sys.time()
  total_prob_time <- end_prob_time - start_prob_time
  
  message("Done. Total PROBABILITY model time: ", round(total_prob_time, 2),units(total_prob_time))
  message("Probability modeling output written to ", outfile)
  
}









