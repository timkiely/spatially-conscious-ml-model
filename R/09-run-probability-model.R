# This function creates the RADII modeling data

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p12_sale_price_model_base.rds") {
  message("Running the probability model on BASE data...")
  
  # check if modeling data exists
  if(file.exists(model_data_infile)){
  

  # loading data ------------------------------------------------------------
  message("Reading base data...")
  # base_data <- read_rds(model_data_infile)
  
  # FOR DEV PURPOSES
  warning("You are taking a sample of the modeling data, for dev purposes. 09-run-probability-model.R")
  # set.seed(1989)
  # base_data_samp <- sample_frac(base_data, 0.2)
  # write_rds(base_data_samp, "data/base-data-sample.rds")
  base_data <- read_rds("data/base-data-sample.rds")

  message("     ...done.")
  
  
  # partition to test & train -----------------------------------------------
  source("R/helper/partition-modeling-data.R")
  message("Partitioning modeling data into train and test...")
  modeling_data <- partition_modeling_data(base_data, train_years = 2003:2016, test_years = 2017)
  message("     ...done.")
  
  
  # creating processing frame ------------------------------------------------
  message("Running Preprocessing steps...")
  source("R/helper/pre-process-modeling-data.R")
  
  ## some preprocessing steps can't handle variable names with spaces
  # converting variable names to snake notation:
  modeling_data <- modeling_data %>% map(function(x) {
    names(x) <- gsub(" ","_",names(x))
    return(x)
  }
  )
  
  processed_data <- run_preprocessing_steps(modeling_data, sample = 0.2)
  message("     ...processing done")

  # for dev purposes:
  set.seed(1987)
  model_data_list <- processed_data %>% map(model_data_list, .f = ~{sample_frac(.x,0.1)})
  
  # turn model data list into a tidy data frame ---------------------------
  train_test_data <- 
    model_data_list %>%
    enframe(name = 'id', value = 'rawdata') %>% 
    mutate(rawdata = map(.x = rawdata, .f = ~as.data.frame(.x))) %>% # caret doesn't play nice with tibbles
    mutate(data_group = rep(1:(length(names(model_data_list))/2),each = 2)) %>% 
    mutate("train_test" = factor(ifelse(grepl("train",id),"Train","Test"), levels = c("Train","Test"))) %>% 
    select(-id) %>% 
    spread(train_test, rawdata) %>% 
    mutate(data_group = names(model_data_list)[str_detect(names(model_data_list),"train")] %>% str_replace("train_","")) %>% 
    rename("id" = data_group) %>%   
    transmute(
      id
      , train.X = map(Train,  ~ .x %>% select(-SALE_PRICE))
      , train.Y = map(Train, ~ .x$SALE_PRICE)
      , test.X = map(Test, ~.x %>% select(-SALE_PRICE))
      , test.Y = map(Test, ~.x$SALE_PRICE)
      )
  
  
  # Define Models -----------------------------------------------------------
  
  model_list <- read_rds('data/aux data/model-list.rds')
  
  # build modeling tibble ---------------------------------------------------
  train_df <- model_list %>% arrange(modelName) %>%mutate(idx = 1:n())
  
  
  # split in to xgboost and the rest ----------------------------------------
  train_df_xgb <- train_df %>% filter(grepl("xgb|lassoRegModel|RBPModel",modelName))
  train_df_h2o <- train_df %>% filter(grepl("h2oRFmodel|h2oGBMmodel", modelName))
  train_df_caret <- anti_join(train_df,bind_rows(train_df_xgb,train_df_h2o), by = "idx")
  
  
  
  
  # TRAIN THE MODELS --------------------------------------------------------
  
  # progress bar (pb$tick() is built into the model training functions)
  pb <- progress::progress_bar$new(
    total = nrow(train_df_caret)
    , format = "running model #:current of :total :elapsed :what [:bar]"
    , clear = FALSE
  )
  
  
  
  
  # TRAIN CARET MODELS -------------------------------------------------
  
  # parallel:
  num_cores <- parallel::detectCores()-2
  cl <- makeSOCKcluster(num_cores)
  registerDoSNOW(cl)
  iter_model_list <- train_df_caret$modelName
  opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))
  
  
  run_start <- Sys.time()
  train_df_caret <- foreach(i = 1:nrow(train_df_caret)
                            , .export = c("pb","iter_model_list", "train_test_data")
                            , .verbose = FALSE
                            , .errorhandling = "pass"
                            , .options.snow = opts ) %dopar% {
                              
                              source("R/helper/load-packages.R")
                              
                              mod_interest <- 
                                train_df_caret %>% 
                                filter(row_number()==i) %>% 
                                .[rep(seq_len(nrow(.)), each=2),] %>% 
                                bind_cols(train_test_data) %>% 
                                mutate(params = map2(train.X, train.Y,  ~ list(X = .x, Y = .y)))
                              
                              start_time <- Sys.time()-14400
                              out <- mod_interest %>% mutate(modelFits = invoke_map(.f = model, .x = params))
                              end_time <- Sys.time()-14400
                              
                              run_time <- round(as.numeric(end_time - start_time),2)
                              run_time_units <- units(end_time - start_time)
                              time_list <- data.frame("model" = paste0(mod_interest$modelName," : ",mod_interest$id)
                                                      ,"start_time" = start_time
                                                      ,"end_time"  = end_time
                                                      ,"run_time" = run_time
                                                      ,"run_time_units" = run_time_units)
                              out <- bind_cols(out, nest(time_list, .key = "run_times_list"))
                              out <- out %>% mutate(run_time = paste0(round(time_list$run_time,2)," ",time_list$run_time_units))
                              
                              return(out) 
                            }
  
  
  run_end <- Sys.time()
  stopCluster(cl)
  stopImplicitCluster()
  closeAllConnections()
  
  message("Trained ",length(train_df_caret)," models in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))
  
  
  
  
  
  } else message("TODO: data not available yet") #remove this at the end
}









