

rm(list=ls()[!ls()%in%c("model_data_list","sale_augmented")])
source("R/00aa-load-packages.R")
source("R/tune-model-objects.R")


model_data_list <- read_rds("data/processed-modeling-data.rds")
# for dev purposes:
set.seed(1987)
model_data_list <- map(model_data_list, .f = ~{sample_n(.x,5000)})




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
    , test = Test
  )



# Define Models -----------------------------------------------------------

model_list <- read_rds('data/model-list.rds')


# build modeling tibble ---------------------------------------------------

train_df <- starter_df[rep(1:nrow(starter_df),nrow(model_list)),]

train_df <- 
  train_df %>%
  bind_cols(
    model_list[rep(1:nrow(model_list),nrow(starter_df)),] %>% 
      arrange(modelName)
  ) %>%
  mutate(idx=1:n())



# TRAIN THE MODELS --------------------------------------------------------

# speeds up model training with parallelization
# doMC::registerDoMC( cores = parallel::detectCores()-2 )


# progress bar (pb$tick() is built into the model training functions)
pb <- progress::progress_bar$new(
  total = nrow(train_df)
  , format = "(:spin) running model #:current of :total :what :elapsed [:bar]"
  , clear = FALSE
)

num_cores <- parallel::detectCores()-2
cl <- makeCluster(num_cores, outfile = "")
registerDoParallel(cl)

run_start <- Sys.time()

train_out <- foreach(i = 1:nrow(train_df)
                     , .export = "pb"
                     , .verbose = TRUE
                     #, .combine = list
                     , .errorhandling = "pass"
                     ) %dopar% {
                       source("R/00aa-load-packages.R")
                       out <-   
                         train_df %>% 
                         filter(row_number()==i) %>% 
                         mutate(params = map2(train.X, train.Y,  ~ list(X = .x, Y = .y))
                                , modelFits = invoke_map(.f = model, .x = params)
                         )
                       
                       return(out)
                       
                     }

run_end <- Sys.time()
stopCluster(cl)
stopImplicitCluster()

message("Trained ",nrow(train_df)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))





# Evaluate ----------------------------------------------------------------
train_out <- 
  train_out %>% 
  mutate(
    RMSE = map_dbl(modelFits, ~max(.x$results$RMSE)),
    RMSESD = map_dbl(modelFits, ~max(.x$results$RMSESD)),
    Rsq = map_dbl(modelFits, ~max(.x$results$Rsquared)),
    bestTune = map(modelFits, ~.x$bestTune)
  ) %>% 
  mutate(y_hat = map2(.x = test, .y = modelFits, ~predict(.y, newdata = .x))) %>% 
  mutate(test_y = map(test, ~.x$SALE.PRICE)) %>% 
  mutate(Eval_RMSE = map2_dbl(.x = y_hat, .y = test_y, .f = ~as.numeric(sqrt(mean((.x - .y)^2))))
         , Eval_Rsq = map2_dbl(.x = y_hat, .y = test_y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
         , Eval_Spearman = map2_dbl(.x = y_hat, .y = test_y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
  )

train_out %>% select(id,modelName,contains("Eval")) %>% arrange(Eval_RMSE)

lattice::dotplot(Rsq~id|modelName,train_out)

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






