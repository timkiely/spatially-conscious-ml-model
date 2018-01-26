# This function creates the RADII modeling data

run_probability_model <- function(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                                  , outfile = "data/processing steps/p12_sale_price_model_base.rds") {
  message("Running the probability model on BASE data...")
  
  # check if modeling data exists
  if(file.exists(model_data_infile)){
  
  
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
  
  
  } else message("TODO: data not available yet") #remove this at the end
}









