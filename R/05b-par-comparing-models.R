

rm(list=ls()[!ls()%in%c("model_data_list","sale_augmented")])
source("R/00aa-load-packages.R")
source("R/tune-model-objects.R")
options(tibble.print_max = 25)

model_data_list <- read_rds("data/processed-modeling-data.rds")
# for dev purposes:
set.seed(1987)
model_data_list <- map(model_data_list, .f = ~{sample_n(.x,2000)})




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
  mutate(dtrain =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(train.X, train.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
  mutate(dtest =  ifelse(modelName %in% c("xgbTreeModel2"),  map2(test.X, test.Y,  ~ xgb.DMatrix(data = data.matrix(.x), label = .y)), NA)) %>% 
  mutate(watchlist = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, dtest, ~ list(train = .x, test = .y)),NA)) %>% 
  mutate(params = ifelse(modelName %in% c("xgbTreeModel2"), map2(dtrain, watchlist,  ~ list(X = .x, watchlist = .y)), params)) %>% 
  select(-dtrain, -dtest, -watchlist) %>% 
  mutate(idx = 1:n())


# split in to xgboost and the rest ----------------------------------------
train_df_xgb <- train_df %>% filter(grepl("xgb",modelName))
train_df_other <- anti_join(train_df,train_df_xgb, by = "idx")

# TRAIN THE MODELS --------------------------------------------------------

# progress bar (pb$tick() is built into the model training functions)
pb <- progress::progress_bar$new(
  total = nrow(train_df_other)
  , format = "running model #:current of :total :elapsed :what [:bar]"
  , clear = FALSE
)




# train non-xgbost models -------------------------------------------------
# cores for parallel
num_cores <- parallel::detectCores()-2
cl <- makeSOCKcluster(num_cores)
registerDoSNOW(cl)
iter_model_list <- train_df_other$modelName
opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))


run_start <- Sys.time()
train_out_other <- foreach(i = 1:nrow(train_df_other)
                           , .export = c("pb","iter_model_list")
                           , .verbose = FALSE
                           #, .combine = list
                           , .errorhandling = "pass"
                           , .options.snow = opts
) %dopar% {
  
  source("R/00aa-load-packages.R")
  
  out <-   
    train_df_other %>% 
    filter(row_number()==i) %>% 
    mutate(modelFits = invoke_map(.f = model, .x = params))
  
  return(out)
  
}

run_end <- Sys.time()
stopCluster(cl)
stopImplicitCluster()

message("Trained ",nrow(train_df_other)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))






# train xgboost models ----------------------------------------------------

# progress bar (pb$tick() is built into the model training functions)
pb <- progress::progress_bar$new(
  total = nrow(train_df_xgb)
  , format = "running model #:current of :total :elapsed :what [:bar]"
  , clear = FALSE
)


# cores for parallel
# speed up model training with parallelization
registerDoSEQ()
iter_model_list <- train_df_xgb$modelName
opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = iter_model_list[n])))

run_start <- Sys.time()

train_out_xgb <- foreach(i = 1:nrow(train_df_xgb)
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

run_end <- Sys.time()
stopImplicitCluster()




message("Trained ",nrow(train_df_xgb)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))




# Combine results ---------------------------------------------------------

train_out <- bind_rows(train_out_other,train_out_xgb) %>% mutate(model_class = map_chr(modelFits, ~as.character(class(.x))))
train_out_1 <- train_out %>% filter(model_class == "train")
train_out_2 <- train_out %>% filter(model_class == "xgb.Booster")


# Evaluate ----------------------------------------------------------------
train_out_1 <- 
  train_out_1 %>% 
  mutate(data_fit = map2(.x = train.X, .y = modelFits, ~as.numeric(predict(.y, newdata = .x)))) %>% 
  mutate(
    RMSE = map_dbl(modelFits, ~max(.x$results$RMSE)),
    RMSESD = map_dbl(modelFits, ~max(.x$results$RMSESD)),
    Rsq = map_dbl(modelFits, ~max(.x$results$Rsquared)),
    bestTune = map(modelFits, ~.x$bestTune)
  ) %>% 
  mutate(y_hat = map2(.x = test.X, .y = modelFits, ~predict(.y, newdata = .x))) %>% 
  mutate(Eval_RMSE = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Eval_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Eval_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  )

train_out_2 <- 
  train_out_2 %>% 
  mutate(data_fit = map2(.x = train.X, .y = modelFits, ~predict(.y, newdata = data.matrix(.x)))) %>% 
  mutate(RMSE = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Rsq = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Spearman = map2_dbl(.x = data_fit, .y = train.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  )
  
  



lattice::dotplot(Rsq~id|modelName,train_out_1)

train_out %>% 
  filter(modelName!="linearRegModel") %>% 
  ggplot()+
  aes(x=id,color=modelName)+
  geom_point(aes(y=RMSE),size=2)+
  geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.15)+
  facet_wrap(~modelName)

train_out %>% 
  filter(id == 'vtreated', modelName == "xgbLinearModel") %>% 
  select(test,Preds) %>% 
  mutate(test = map(test, ~.x$SALE.PRICE)) %>% 
  summarise(caret::RMSE(Preds,test))
unnest() %>% 
  ggplot()+
  aes(x = test, y = Preds)+
  geom_point()+
  geom_smooth()


# plot(train_df$modelFits[train_df$modelName=='linearRegModel' & train_df$id=='train_treated'][[1]])
varImp(train_out$modelFits[train_out$modelName=='xgbModel' & train_out$id=='vtreated'][[1]])






