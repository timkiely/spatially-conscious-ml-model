

rm(list=ls()[!ls()%in%c("model_data_list","sale_augmented")])
start_global <- Sys.time()
source("R/00aa-load-packages.R")
source("R/tune-model-objects.R")
options(tibble.print_max = 25)

model_data_list <- read_rds("data/processed-modeling-data.rds")
# for dev purposes:
set.seed(1987)
model_data_list <- map(model_data_list, .f = ~{sample_frac(.x,0.1)})




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

train_df <- 
  train_df %>%
  bind_cols(model_list[rep(1:nrow(model_list),nrow(starter_df)),] %>% arrange(modelName)) %>%
  mutate(params = map2(train.X, train.Y,  ~ list(X = .x, Y = .y))) %>% 
  mutate(params = ifelse(modelName == "xgbRFmodel", map2(train.X, train.Y,  ~ list(X = data.matrix(.x), Y = data.matrix(.y))), params)) %>% 
  mutate(dtrain =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(train.X, train.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
  mutate(dtest =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(test.X, test.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
  mutate(watchlist = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, dtest, ~ list(train = .x, test = .y)),NA)) %>% 
  mutate(params = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, watchlist,  ~ list(X = .x, watchlist = .y)), params)) %>% 
  select(-dtrain, -dtest, -watchlist) %>% 
  mutate(idx = 1:n())


# split in to xgboost and the rest ----------------------------------------
train_df_xgb <- train_df %>% filter(grepl("xgb|lassoRegModel",modelName))
train_df_caret <- anti_join(train_df,train_df_xgb, by = "idx")




# TRAIN THE MODELS --------------------------------------------------------

# progress bar (pb$tick() is built into the model training functions)
pb <- progress::progress_bar$new(
  total = nrow(train_df_caret)
  , format = "running model #:current of :total :elapsed :what [:bar]"
  , clear = FALSE
)




# train caret models -------------------------------------------------
# cores for parallel
num_cores <- parallel::detectCores()-2
cl <- makeSOCKcluster(num_cores)
#registerDoSNOW(cl)
registerDoSEQ()
iter_model_list <- train_df_caret$modelName
opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))


run_start <- Sys.time()
train_df_caret <- foreach(i = 1:nrow(train_df_caret)
                          , .export = c("pb","iter_model_list")
                          , .verbose = FALSE
                          #, .combine = list
                          , .errorhandling = "pass"
                          , .options.snow = opts
) %dopar% {
  
  source("R/00aa-load-packages.R")
  
  out <-   
    train_df_caret %>% 
    filter(row_number()==i) %>% 
    mutate(modelFits = invoke_map(.f = model, .x = params))
  
  return(out)
  
}

run_end <- Sys.time()
stopCluster(cl)
stopImplicitCluster()

message("Trained ",length(train_df_caret)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))






# train xgboost models ----------------------------------------------------

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
) %dopar% {
  
  source("R/00aa-load-packages.R")
  
  out <-   
    train_df_xgb %>% 
    filter(row_number()==i) %>% 
    mutate(modelFits = invoke_map(.f = model, .x = params))
  
  return(out)
  
}

run_end_2 <- Sys.time()
stopImplicitCluster()



# Checking the results for errors ----------------------------------------
caret_error_vec <- map(.x = train_df_caret, .f = ~class(.x)[[1]]=="tbl_df") %>% unlist()
xgb_error_vec <- map(.x = train_df_xgb, .f = ~class(.x)[[1]]=="tbl_df") %>% unlist()


xgb_errors <- train_df_xgb[!xgb_error_vec]
if(length(xgb_errors)==0) xgb_errors <- "no caret errors"

caret_errors <- train_df_caret[!caret_error_vec]
if(length(caret_errors)==0) caret_errors <- "no caret errors"

train_df_caret <- train_df_caret[caret_error_vec]
train_df_xgb <- train_df_xgb[xgb_error_vec]


  

message("Trained ",length(train_df_caret)," caret models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors")






# Combine results ---------------------------------------------------------



train_out <- bind_rows(train_df_caret,train_df_xgb) %>% mutate(model_class = map_chr(modelFits, ~as.character(class(.x))))
train_out_1 <- train_out %>% filter(model_class == "train")
train_out_2 <- train_out %>% filter(model_class == "xgb.Booster")


# Calculate Evaluation metrics -------------------------------------------
train_out_1 <- 
  train_out_1 %>% 
  mutate(data_fit = map2(.x = train.X, .y = modelFits, ~as.numeric(predict(.y, newdata = .x)))) %>% 
  mutate(RMSE = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Rsq = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Spearman = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))) %>%
  mutate(y_hat = map2(.x = test.X, .y = modelFits, ~predict(.y, newdata = .x))) %>% 
  mutate(Test_RMSE = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  )

train_out_2 <- 
  train_out_2 %>% 
  mutate(data_fit = map2(.x = train.X, .y = modelFits, ~predict(.y, newdata = data.matrix(.x)))) %>% 
  mutate(RMSE = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Rsq = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Spearman = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  ) %>% 
  mutate(y_hat = map2(.x = test.X, .y = modelFits, ~predict(.y, newdata = data.matrix(.x)))) %>% 
  mutate(Test_RMSE = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  )

train_out <- bind_rows(train_out_1, train_out_2)







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
  


msg_out_1 <- paste0("Trained ",length(train_df_caret)," caret models in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))
msg_out_2 <- paste0("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)))
rich_template <- paste("Your models have finished training"
                       , paste0("Script run started ",format(start_global-14400, "%a %b %d %X %Y"))
                       , msg_out_1
                       , msg_out_2
                       ,"Summary of y actual:"
                       , pander::pandoc.table.return(round(as.matrix(summary(model_data_list$test_vtreated$SALE.PRICE)),2), style = "grid")
                       ,"Summary of best model yhat:"
                       , pander::pandoc.table.return(
                         round(
                           train_out %>%
                             arrange(-Test_Rsq) %>%
                             head(1) %>%
                             select(y_hat) %>%
                             unnest() %>%
                             as.matrix() %>%
                             as.numeric() %>%
                             summary() %>%
                             as.matrix()
                           ,2)
                        , style = "grid")
                       ,"Summary of results:"
                       , pander::pandoc.table.return(sample_out_frame, style = "grid")
                       , "Errors from caret:"
                       , pander::pandoc.table.return(caret_errors, style = "grid")
                       , "Errors from xgb:"
                       , pander::pandoc.table.return(xgb_errors, style = "grid")
                       , "This is a friendly email from me."
                       , sep = "\n\n")

current_time <- Sys.time()-14400 

sender <- "timothy.j.kiely@gmail.com"
recipients <- c("timothy.j.kiely@gmail.com", "tkiely@hodgeswardelliott.com")
send.mail(from = sender,
          to = recipients,
          subject= paste0("Model Training Finished ",format(Sys.time()-14400, "%a %b %d %X %Y")),#14,400 seconds in 4 hours, which offsets Zulu to EST
          body = rich_template,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name="timothy.j.kiely@gmail.com", passwd="Cestina2017!", ssl=TRUE),
          authenticate = TRUE,
          send = TRUE)


message("Trained ",length(train_df_caret)," caret models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)), " with ",length(caret_error_vec[caret_error_vec==FALSE]), " errors")
message("Trained ",length(train_df_xgb)," xgboost models with 5 fold-CV in ",round(difftime(run_end_2,run_start_2),3)," ",units(difftime(run_end_2,run_start_2)), " with ",length(xgb_error_vec[xgb_error_vec==FALSE]), " errors")


