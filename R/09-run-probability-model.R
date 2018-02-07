# This function runs the BASE PROBABILITY model

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                  , dev = FALSE 
                                  , helper_title = NA) {
  
  message(paste0("\n#============= PROBABILITY model: ",helper_title," =================#"))
  
  if(file.exists(model_data_infile)){
    start_prob_time <- Sys.time()
    
    
    if(dev == TRUE) {
      warning("You are taking a sample of the modeling data, for dev purposes. 09-run-probability-model.R")
      message("Using sample data to run models...")
      model_data_list <- read_rds("data/model_data_list-temp.rds")
      
    } else { 
      # loading data ------------------------------------------------------------
      message("Reading base data...")
      base_data <- read_rds(model_data_infile)
      message("     ...done.")
      
      
      # partition to test & train -----------------------------------------------
      source("R/helper/partition-modeling-data.R")
      message("Partitioning modeling data into train and test...")
      modeling_data <- partition_modeling_data(base_data, train_years = 2003:2016, test_years = 2017)
      message("     ...done.")
      
      
      # creating processing frame ------------------------------------------------
      message("Running Preprocessing steps...")
      # some preprocessing steps can't handle variable names with spaces
      # converting variable names to snake notation:
      modeling_data <- modeling_data %>% map(function(x) {
        names(x) <- gsub(" ","_",names(x))
        return(x)
      }
      )
      
      # processing function:
      source("R/helper/pre-process-modeling-data.R")
      processed_data <- run_preprocessing_steps(modeling_data, sample = 0.2)
      message("     ...processing done")
      
      # for dev purposes:
      set.seed(1987)
      model_data_list_samp <- processed_data %>% map(model_data_list, .f = ~{sample_frac(.x,0.01)})
      # write to temp file for development:
      write_rds(model_data_list_samp, "data/model_data_list-temp.rds")
      message("     ...done.")
      
      model_data_list <- processed_data
      
    }
    
    
    # turn model data list into a tidy data frame ---------------------------
    message("Creating modeling frame...")
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
        , train.X = map(Train,  ~ .x %>% select(-SALE_PRICE, -Sold, -Annual_Sales, -bbl, -Address, -Years_Since_Last_Sale, -SALE_YEAR, -TOTAL_SALES, -GROSS_SQUARE_FEET))
        , train.Y = map(Train, ~ .x$Sold)
        , test.X = map(Test, ~.x %>% select(-SALE_PRICE, -Sold, -Annual_Sales, -bbl, -Address, -Years_Since_Last_Sale, -SALE_YEAR, -TOTAL_SALES, -GROSS_SQUARE_FEET))
        , test.Y = map(Test, ~.x$Sold)
      )
    
    # write to disk for later use:
    write_rds(train_test_data, "data/aux data/train_test_data_prob.rds")
    message("     ...done.")
    
    # Define Models -----------------------------------------------------------
    train_df <- read_rds('data/aux data/model-list.rds') %>% arrange(modelName) %>% mutate(idx = 1:n())
    
    # split in to xgboost, h2o and the rest ------------------------------------
    train_df_xgb <- train_df %>% filter(grepl("xgb|lassoRegModel|RBPModel",modelName))
    train_df_h2o <- train_df %>% filter(grepl("h2oRFmodel|h2oGBMmodel", modelName))
    train_df_caret <- anti_join(train_df,bind_rows(train_df_xgb,train_df_h2o), by = "idx")
    
    # TRAIN H2O Models ----------------------------------------------------------
    
    ## NOTE: YOU MAY NEED TO INSTALL JAVA. > system("sudo apt-get install default-jre")
    
    # initialize h2o cluster
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
    
    train_df_h2o_all <- train_df_h2o[rep(seq_len(nrow(train_df_h2o)), each = nrow(train_test_data)), ]
    train_df_h2o_all$data_idx <- rep(1:nrow(train_test_data), nrow(train_df_h2o))
    
    # h2o has parellelization built in, so best to run them in sequence
    registerDoSEQ()
    iter_model_list <- train_df_h2o_all$modelName
    
    # progress bar (pb$tick() is built into the model training functions)
    pb <<- progress::progress_bar$new(
      total = nrow(train_df_h2o_all)
      , format = "running model #:current of :total :elapsed :what [:bar]"
      , clear = FALSE
    )
    opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))
    
    message("Starting h2o models...")
    run_start_3 <- Sys.time()
    train_out_h2o <- foreach(i = 1:nrow(train_df_h2o_all)
                             , .verbose = FALSE
                             , .errorhandling = "pass") %do% {
                               
                               mod_interest <- train_df_h2o_all %>% filter(row_number()==i)
                               model_data <- train_test_data[mod_interest$data_idx,]
                               
                               X <- model_data %>% select(train.X) %>% unnest()
                               X_names <- names(X)
                               Y <- model_data %>% select(train.Y) %>% unnest()
                               names(Y) <- Y_names <- "Sold"
                               training_frame <- as.h2o(bind_cols(X,Y))
                               
                               X_val <- model_data %>% select(test.X) %>% unnest()
                               X_val_names <- names(X_val)
                               Y_val <- model_data %>% select(test.Y) %>% unnest()
                               names(Y_val) <- Y_val_names <- "Sold"
                               validation_frame <- as.h2o(bind_cols(X_val,Y_val))
                               
                               h2o_model <- mod_interest %>% select(model) %>% unlist() %>% .[[1]]
                               
                               start_time <- Sys.time()-14400
                               h2o_mod <- h2o_model(X = X_names, Y = Y_names, training_frame=training_frame, validation_frame=validation_frame)
                               end_time <- Sys.time()-14400
                               
                               
                               run_time <- round(as.numeric(end_time - start_time),2)
                               run_time_units <- units(end_time - start_time)
                               time_list <- data.frame("model" = paste0(mod_interest$modelName," : ",mod_interest$idx)
                                                       ,"start_time" = start_time
                                                       ,"end_time"  = end_time
                                                       ,"run_time" = run_time
                                                       ,"run_time_units" = run_time_units)
                               mod_interest <- mod_interest %>% mutate(modelFits = list(h2o_mod))
                               mod_interest <- bind_cols(mod_interest, nest(time_list, .key = "run_times_list"))
                               mod_interest <- mod_interest %>% mutate(run_time = paste0(round(time_list$run_time,2)," ",time_list$run_time_units))
                               data_fit <- tbl_df(predict(h2o_mod, newdata = training_frame, type="prob"))
                               y_hat <- tbl_df(predict(h2o_mod, newdata = validation_frame, type="prob"))
                               mod_interest <- 
                                 mod_interest %>% 
                                 mutate(data_fit = list(data_fit)
                                        , y_hat = list(y_hat)
                                        , test.Y = model_data$test.Y
                                        , train.Y = model_data$train.Y
                                        , data_id = model_data$id)
                             }
    run_end_3 <- Sys.time()
    closeAllConnections()
    rm(pb, envir = globalenv())
    
    message("Trained ",length(train_out_h2o)," h2o models in ",round(difftime(run_end_3,run_start_3),3)," ",units(difftime(run_end_3,run_start_3)))
    
    message("Writing model results to disk...")
    final_model_object <- bind_rows(train_out_h2o)
    write_rds(final_model_object, outfile)
    message("     ...done.")
    
    # output time
    end_prob_time <- Sys.time()
    
    total_prob_time <- end_prob_time - start_prob_time
    
    message("Done. Total PROBABILITY model time: ", round(total_prob_time, 2),units(total_prob_time))
    message("Probability modeling output written to ", outfile)
    
  } else warning("Following Input data not available: ", model_data_infile)
}









