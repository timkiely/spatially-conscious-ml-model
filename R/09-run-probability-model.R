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
  
  
  # creating processing frame ------------------------------------------------
  message("Running Preprocessing steps...")
  source("R/helper/pre-process-modeling-data.R")
  processed_data <- run_preprocessing_steps(modeling_data)
  message("     ...processing done")

  # for dev purposes:
  set.seed(1987)
  model_data_list <- processed_data %>% map(model_data_list, .f = ~{sample_frac(.x,0.01)})
  
  # turn model data list into a tidy data frame ---------------------------
  starter_df <- 
    model_data_list %>%
    enframe(name = 'id', value = 'rawdata') %>% 
    mutate(rawdata = map(.x = rawdata, .f = ~as.data.frame(.x))) %>% # caret doesn't play nice with tibbles
    mutate(data_group = rep(1:(length(names(model_data_list))/2),each = 2)) %>% 
    mutate("train_test" = factor(ifelse(grepl("train",id),"Train","Test"), levels = c("Train","Test"))) %>% 
    select(-id) %>% 
    spread(train_test, rawdata) %>% 
    mutate(data_group = names(model_data_list)[str_detect(names(model_data_list),"train")] %>% str_replace("train_","")) %>% 
    rename("id" = data_group)
  
  # Create X predictors and Y column
  starter_df %<>% 
    transmute(
      id
      , train.X = map(Train,  ~ .x %>% select(-`SALE PRICE`))
      , train.Y = map(Train, ~ .x$`SALE PRICE`)
      , test.X = Test
      , test.Y = map(test.X, ~.x %>% pull(`SALE PRICE`) %>% as.matrix() %>% as.numeric())
      , test.X = map(test.X, ~.x %>% select(-`SALE PRICE`))
    )
  
  
  
  
  # Define Models -----------------------------------------------------------
  
  model_list <- read_rds('data/aux data/model-list.rds')
  
  
  # build modeling tibble ---------------------------------------------------
  
  train_df <- starter_df[rep(1:nrow(starter_df),nrow(model_list)),]
  train_df <- 
    train_df %>%
    bind_cols(model_list[rep(1:nrow(model_list),nrow(starter_df)),] %>% arrange(modelName)) %>%
    
    # generic caret models:
    mutate(params = map2(train.X, train.Y,  ~ list(X = .x, Y = .y))) %>% 
    
    # xgb Random Forrest model
    mutate(params = ifelse(modelName == "xgbRFmodel", map2(train.X, train.Y,  ~ list(X = data.matrix(.x), Y = data.matrix(.y))), params)) %>% 
    
    # for xgboost models:
    mutate(dtrain =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(train.X, train.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
    mutate(dtest =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(test.X, test.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
    mutate(watchlist = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, dtest, ~ list(train = .x, test = .y)),NA)) %>% 
    mutate(params = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, watchlist,  ~ list(X = .x, watchlist = .y)), params)) %>% 
    select(-dtrain, -dtest, -watchlist) %>% 
    mutate(idx = 1:n())
  
  
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
  num_cores <- 6
  cl <- makeSOCKcluster(num_cores)
  registerDoSNOW(cl)
  iter_model_list <- train_df_caret$modelName
  opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))
  
  
  run_start <- Sys.time()
  train_df_caret <- foreach(i = 1:nrow(train_df_caret)
                            , .export = c("pb","iter_model_list")
                            , .verbose = FALSE
                            , .errorhandling = "pass"
                            , .options.snow = opts ) %dopar% {
                              
                              source("R/00aa-load-packages.R")
                              
                              mod_interest <- train_df_caret %>% filter(row_number()==i)
                              
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
  
  
  
  
  
  
  # TRAIN XGBOOST MODELS ----------------------------------------------------
  
  # progress bar (pb$tick() is built into the model training functions)
  pb <- progress::progress_bar$new(
    total = nrow(train_df_xgb)
    , format = "running model #:current of :total :elapsed :what [:bar]"
    , clear = FALSE
  )
  
  
  # xgboost has parellelization built in, so best to run them in sequence
  registerDoSEQ()
  iter_model_list <- train_df_xgb$modelName
  opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))
  
  run_start_2 <- Sys.time()
  
  train_df_xgb <- foreach(i = 1:nrow(train_df_xgb)
                          , .export = c("pb","iter_model_list")
                          , .verbose = FALSE
                          #, .combine = list
                          , .errorhandling = "pass"
                          , .options.snow = opts ) %do% {
                            
                            source("R/00aa-load-packages.R")
                            
                            mod_interest <- train_df_xgb %>% filter(row_number()==i)
                            
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
  
  run_end_2 <- Sys.time()
  stopImplicitCluster()
  closeAllConnections()
  
  
  
  # TRAIN H2O Models ----------------------------------------------------------
  sink(".sink-output")
  h2o.init(nthreads = -1) #Number of threads -1 means use all cores on your machine
  options("h2o.use.data.table"=TRUE)
  h2o.no_progress()
  h2o.removeAll()
  sink(NULL)
  
  
  
  pb <- progress::progress_bar$new(
    total = nrow(train_df_h2o)
    , format = "running model #:current of :total :elapsed :what [:bar]"
    , clear = FALSE
  )
  
  
  # h2o has parellelization built in, so best to run them in sequence
  registerDoSEQ()
  iter_model_list <- train_df_h2o$modelName
  opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))
  
  
  run_start_3 <- Sys.time()
  train_df_h2o <- foreach(i = 1:nrow(train_df_h2o)
                          , .verbose = FALSE
                          , .errorhandling = "pass") %do% {
                            
                            mod_interest <- train_df_h2o %>% filter(row_number()==i)
                            
                            X <- mod_interest %>% select(train.X) %>% unnest()
                            X_names <- names(X)
                            Y <- mod_interest %>% select(train.Y) %>% unnest()
                            names(Y) <- Y_names <- "SALE.PRICE"
                            training_frame <- as.h2o(bind_cols(X,Y))
                            
                            X_val <- mod_interest %>% select(test.X) %>% unnest()
                            X_val_names <- names(X_val)
                            Y_val <- mod_interest %>% select(test.Y) %>% unnest()
                            names(Y_val) <- Y_val_names <- "SALE.PRICE"
                            validation_frame <- as.h2o(bind_cols(X_val,Y_val))
                            
                            h2o_model <- mod_interest %>% select(model) %>% unlist() %>% .[[1]]
                            
                            start_time <- Sys.time()-14400
                            h2o_mod <- h2o_model(X = X_names, Y = Y_names, training_frame=training_frame, validation_frame=validation_frame)
                            end_time <- Sys.time()-14400
                            
                            
                            run_time <- round(as.numeric(end_time - start_time),2)
                            run_time_units <- units(end_time - start_time)
                            time_list <- data.frame("model" = paste0(mod_interest$modelName," : ",mod_interest$id)
                                                    ,"start_time" = start_time
                                                    ,"end_time"  = end_time
                                                    ,"run_time" = run_time
                                                    ,"run_time_units" = run_time_units)
                            mod_interest <- mod_interest %>% mutate(modelFits = list(h2o_mod))
                            mod_interest <- bind_cols(mod_interest, nest(time_list, .key = "run_times_list"))
                            mod_interest <- mod_interest %>% mutate(run_time = paste0(round(time_list$run_time,2)," ",time_list$run_time_units))
                            data_fit <- tbl_df(predict(h2o_mod, newdata = training_frame))
                            y_hat <- tbl_df(predict(h2o_mod, newdata = validation_frame))
                            
                            mod_interest <- 
                              mod_interest %>% 
                              mutate(data_fit = list(data_fit)
                                     , y_hat = list(y_hat)
                              )
                            
                          }
  run_end_3 <- Sys.time()
  closeAllConnections()
  
  message("Trained ",length(train_df_h2o)," h2o models in ",round(difftime(run_end_3,run_start_3),3)," ",units(difftime(run_end_3,run_start_3)))
  
  
  
  # Checking the results for errors ----------------------------------------
  caret_error_vec <- map(.x = train_df_caret, .f = ~class(.x)[[1]]=="tbl_df") %>% unlist()
  xgb_error_vec <- map(.x = train_df_xgb, .f = ~class(.x)[[1]]=="tbl_df") %>% unlist()
  h2o_error_vec <- map(.x = train_df_h2o, .f = ~class(.x)[[1]]=="tbl_df") %>% unlist()
  
  
  xgb_errors <- train_df_xgb[!xgb_error_vec]
  if(length(xgb_errors)==0) xgb_errors <- "no xgb errors"
  
  caret_errors <- train_df_caret[!caret_error_vec]
  if(length(caret_errors)==0) caret_errors <- "no caret errors"
  
  h2o_errors <- train_df_h2o[!h2o_error_vec]
  if(length(h2o_errors)==0) h2o_errors <- "no H2O errors"
  
  
  train_df_caret <- train_df_caret[caret_error_vec]
  train_df_xgb <- train_df_xgb[xgb_error_vec]
  train_df_h2o <- train_df_h2o[h2o_error_vec]
  
  
  message("Trained ",length(train_df_caret)," caret models in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors",
          "\nTrained ",length(train_df_xgb)," xgboost models in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors",
          "\nTrained ",length(train_df_h2o)," h2o models in ",round(difftime(run_end_3,run_start_3),3)," ",units(difftime(run_end_3,run_start_3)), " with ",length(h2o_error_vec[h2o_error_vec==FALSE]), " errors"
  )
  
}









