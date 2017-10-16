



# init --------------------------------------------------------------------
rm(list=ls()[!ls()%in%c("model_data_list","sale_augmented")])
start_global <- Sys.time()-14400; message("Starting run at ",start_global)
source("R/00aa-load-packages.R")
source("R/tune-model-objects.R")
if(!dir.exists("log")) dir.create("log")
options(tibble.print_max = 50)




# our main modeling data --------------------------------------------------
model_data_list <- read_rds("data/processed-modeling-data.rds")
# for dev purposes:
# set.seed(1987)
# model_data_list <- map(model_data_list, .f = ~{sample_frac(.x,0.01)})



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
    , train.X = map(Train,  ~ .x %>% select(-SALE.PRICE))
    , train.Y = map(Train, ~ .x$SALE.PRICE)
    , test.X = Test
    , test.Y = map(test.X, ~.x %>% pull(SALE.PRICE) %>% as.matrix() %>% as.numeric())
    , test.X = map(test.X, ~.x %>% select(-SALE.PRICE))
  )



# Define Models -----------------------------------------------------------

model_list <- read_rds('data/model-list.rds')


# build modeling tibble ---------------------------------------------------

train_df <- starter_df[rep(1:nrow(starter_df),nrow(model_list)),]
library(h2o)
h2o.removeAll()


train_df <- 
  train_df %>%
  bind_cols(model_list[rep(1:nrow(model_list),nrow(starter_df)),] %>% arrange(modelName)) %>%
  #filter(modelName%in%c("h2oRFmodel")) %>% 
  
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
  
  #for h2o:
  # mutate(test_names = "SALE.PRICE") %>% 
  # mutate(train_names = map2(.x = train.X, .y = test_names, .f = ~setdiff(names(.x),.y))) %>% 
  # mutate(training_frame = map2(train.X, train.Y, .f = ~bind_cols(list(.x,data.frame("SALE.PRICE"=.y))))) %>% 
  # mutate(validation_frame = ifelse(modelName%in%c("h2oRFmodel")
  #                                  , map2(test.X, test.Y, .f = ~bind_cols(list(.x,data.frame("SALE.PRICE"=.y))))
  #                                  , NA)
  #        ) %>% 
  # 
  # mutate(training_frame = ifelse(modelName%in%c("h2oRFmodel")
#                                , map(training_frame, ~as.h2o(.x, verbose = FALSE, showProgress = FALSE))
#                                ,NA)
#        ) %>% 
# mutate(validation_frame = map(validation_frame, ~as.h2o(.x, verbose = FALSE, showProgress = FALSE))) %>% 
# 
# mutate(params = ifelse(modelName %in% c("h2oRFmodel"), map(train_names, test_names
#                                                                  , training_frame, validation_frame
#                                                             , .f = ~list("X" = ..1,"Y" = ..2
#                                                                          ,"training_frame" = ..3
#                                                                          ,"validation_frame" = ..4)
#                                                             )
# , params )) %>% 
mutate(idx = 1:n())


# split in to xgboost and the rest ----------------------------------------
train_df_xgb <- train_df %>% filter(grepl("xgb|lassoRegModel",modelName))
train_df_h2o <- train_df %>% filter(grepl("h2oRFmodel", modelName))
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

message("Trained ",length(train_df_caret)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))






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
                        , .options.snow = opts
) %do% {
  
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



# H2O Models --------------------------------------------------------------
options("h2o.use.data.table"=TRUE)
h2o.no_progress()
h2o.init(nthreads = -1) #Number of threads -1 means use all cores on your machine
h2o.removeAll()



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
train_df_h2o <- 
  foreach(i = 1:nrow(train_df_h2o)) %do% {
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
    
    start_time <- Sys.time()-14400
    h2o_mod <- h2oRFmodel(X = X_names, Y = Y_names, training_frame=training_frame, validation_frame=validation_frame)
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
    mod_interest <- mod_interest %>% 
      mutate(data_fit = list(data_fit)
             , y_hat = list(y_hat))
    
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



message("Trained ",length(train_df_caret)," caret models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_h2o)," h2o models with 5 fold-CV in ",round(difftime(run_end_3,run_start_3),3)," ",units(difftime(run_end_3,run_start_3)), " with ",length(h2o_error_vec[h2o_error_vec==FALSE]), " errors")


# Calculate Evaluation metrics -------------------------------------------
train_out_h2o <- train_df_h2o %>% bind_rows()
train_out_other <- bind_rows(train_df_xgb, train_df_caret)

train_out_h2o <- 
  train_out_h2o %>% 
  mutate(RMSE = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Rsq = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Spearman = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))) %>%
  mutate(Test_RMSE = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  ) %>% 
  arrange(-Test_Rsq)


train_out_other <- 
  train_out_other %>% 
  mutate(data_fit = map2(.x = train.X, .y = modelFits, ~as.numeric(predict(.y, newdata = data.matrix(.x))))) %>% 
  mutate(RMSE = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Rsq = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Spearman = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))) %>%
  mutate(y_hat = map2(.x = test.X, .y = modelFits, ~predict(.y, newdata = data.matrix(.x)))) %>% 
  mutate(Test_RMSE = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  ) %>% 
  arrange(-Test_Rsq)

train_out <- bind_rows(train_out_h2o, train_out_other)

train_out_log <- train_out %>% select(-train.X,-train.Y,-test.X,-test.Y,-model,-params,-modelFits,-data_fit,-y_hat)
write_rds(train_out_log, paste0("log/train-out-",Sys.time(),".rds"), compress = "gz")


# LOGGER ------------------------------------------------------------------
logger                <- list()
logger$Start_time     <- start_global
logger$Top_model      <- train_out %>% head(1) %>% select(modelName, id) %>% transmute(paste0(modelName, " : ",id)) %>% as.character()
logger$Best_Rsq       <- train_out %>% head(1) %>% select(Test_Rsq) %>% as.numeric() %>% round(3)
logger$Best_RMSE      <- train_out %>% head(1) %>% select(Test_RMSE) %>% as.numeric() %>% round(3)
logger$Sytem          <- as.character(Sys.info()["sysname"])
logger$n_rows         <- scales::comma(nrow(model_data_list[[1]]))
logger$data_versions  <- nrow(starter_df)
logger$unique_models  <- nrow(model_list)
logger$function_names <- as.character(paste0(model_list$modelName, collapse = " "))
logger$total_models   <- nrow(train_df)
logger$model_combos   <- as.character(paste0(train_df$modelName,":",train_df$id, collapse = " "))
logger$all_funs       <- paste0(model_list$modelName,gsub("\n","",paste0(as.list(model_list$model))),collapse = " * ")

if(!file.exists("log/logger.csv")){
  write_csv(as.data.frame(logger), "log/logger.csv", append = FALSE)
} else {
  write_csv(as.data.frame(logger), "log/logger.csv", append = TRUE)
}


# send completion email ---------------------------------------------------
# 1) if Java is not installed, install it: https://www.digitalocean.com/community/tutorials/how-to-install-java-on-ubuntu-with-apt-get
# 2) install rJava, then mailR
# 3) if Sys.setenv(JAVA_HOME='/Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home') returns an error, do:
#    $ sudo rstudio-server stop
#    $ export LD_LIBRARY_PATH=/usr/lib/jvm/jre/lib/amd64:/usr/lib/jvm/jre/lib/amd64/default
#    $ sudo rstudio-server start
library(mailR)

sample_out_frame <- 
  train_out %>% 
  mutate(train.X = map_dbl(train.X, ~nrow(.x))) %>% 
  select(Test_Rsq, Test_RMSE, modelName, id, "rows" = train.X, idx,Test_Spearman) %>% 
  arrange(-Test_Rsq) %>% 
  mutate_if(.predicate = is.numeric, .f = ~round(.,3)) %>% 
  as.data.frame()


msg_out_1 <- paste0("Trained ",length(train_df_caret)," caret models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors")
msg_out_2 <- paste0("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors")
rich_template <- paste("Your models have finished training"
                       , paste0("Script run started ",format(start_global, "%a %b %d %X %Y"))
                       , msg_out_1
                       , msg_out_2
                       , paste0("system: ",as.character(Sys.info()["sysname"]))
                       ,"Summary of y actual:"
                       , pander::pandoc.table.return(round(as.matrix(summary(model_data_list$test_vtreated$SALE.PRICE)),2), style = "grid")
                       ,"Summary of best model yhat:"
                       , pander::pandoc.table.return(
                         round(train_out %>%arrange(-Test_Rsq) %>%head(1) %>%
                                 select(y_hat) %>%unnest() %>%as.matrix() %>%
                                 as.numeric() %>%summary() %>%as.matrix(),2)
                         , style = "grid")
                       ,"Summary of results:"
                       , pander::pandoc.table.return(sample_out_frame, style = "grid")
                       , "Errors from caret:"
                       , pander::pandoc.table.return(caret_errors, style = "grid")
                       , "Errors from xgb:"
                       , pander::pandoc.table.return(xgb_errors, style = "grid")
                       , "This is a friendly email from me."
                       , sep = "\n")

current_time <- Sys.time()-14400 

sender <- "timothy.j.kiely@gmail.com"
recipients <- c("timothy.j.kiely@gmail.com", "tkiely@hodgeswardelliott.com")
send.mail(from = sender,
          to = recipients,
          subject = paste0("Model Training Finished ",format(Sys.time()-14400, "%a %b %d %X %Y")), #14,400 seconds in 4 hours, which offsets Zulu to EST
          body = rich_template,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name="timothy.j.kiely@gmail.com", passwd=readLines('pwrd.txt'), ssl=TRUE),
          authenticate = TRUE,
          send = TRUE)



message("Trained ",length(train_df_caret)," caret models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_h2o)," h2o models with 5 fold-CV in ",round(difftime(run_end_3,run_start_3),3)," ",units(difftime(run_end_3,run_start_3)), " with ",length(h2o_error_vec[h2o_error_vec==FALSE]), " errors")

