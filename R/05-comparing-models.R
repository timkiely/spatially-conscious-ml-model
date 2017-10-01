

rm(list=ls()[!ls()%in%c("model_data_list","sale_augmented")])
source("R/00aa-load-packages.R")

if(!exists("model_data_list")){
  model_data_list <- read_rds("data/processed-modeling-data.rds")
  
  # for dev purposes:
  # model_data_list <- map(model_data_list, .f = ~{head(.x,1000)})
}


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

p <- progress_estimated(nrow(train_df))

# speeds up model training with parallelization
doMC::registerDoMC( cores = parallel::detectCores()-2 )

run_start <- Sys.time()

train_out <- 
  train_df %>% 
  #filter(modelName == "linearRegModel") %>% 
  mutate(params = map2(train.X, train.Y,  ~ list(X = .x, Y = .y))
         , modelFits = invoke_map(.f = model, .x = params)
  )

run_end <- Sys.time()

message("Trained ",nrow(train_df)," models with 5 fold-CV in ",round(difftime(run_end,run_start),3)," ",units(difftime(run_end,run_start)))




# Evaluate ----------------------------------------------------------------
train_out <- 
  train_out %>% 
  mutate(
    RMSE = map_dbl(modelFits, ~max(.x$results$RMSE)),
    RMSESD = map_dbl(modelFits, ~max(.x$results$RMSESD)),
    Rsq = map_dbl(modelFits, ~max(.x$results$Rsquared)),
    bestTune = map(modelFits, ~.x$bestTune)
  )

lattice::dotplot(Rsq~id|modelName,train_out)


train_df %>% 
  ggplot()+
  aes(x=id,color=modelName)+
  geom_point(aes(y=RMSE),size=2)+
  geom_errorbar(aes(ymin = RMSE-RMSESD,ymax= RMSE+RMSESD),size=.5,width=.15)


# plot(train_df$modelFits[train_df$modelName=='linearRegModel' & train_df$id=='train_treated'][[1]])
varImp(train_df$modelFits[train_df$modelName=='linearRegModel' & train_df$id=='train_treated'][[1]])




