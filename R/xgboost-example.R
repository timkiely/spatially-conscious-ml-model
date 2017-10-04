


rm(list=ls()[!ls()%in%c("sale_augmented")])
source("R/00aa-load-packages.R")


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


train.x <- train_df %>% filter(idx == 17) %>% select(train.X) %>% unnest() %>% as.matrix()
train.y <- train_df %>% filter(idx == 17) %>% select(train.Y) %>% unnest() %>% as.matrix() %>% as.numeric()
test.x <- train_df %>% filter(idx == 17) %>% select(test) %>% unnest() %>% as.matrix()
test.y <- train_df %>% filter(idx == 17) %>% select(test) %>% transmute(test_y = map(test, ~.x$SALE.PRICE)) %>% unnest() %>% as.matrix() %>% as.numeric()

library(xgboost)
library(Matrix)
params <- list(
  "objective"           = "reg:linear",
  "eval_metric"         = "mae", #"rmse"
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 2,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)


dtrain <- xgb.DMatrix(data = data.matrix(train.x), label = train.y)
dtest <- xgb.DMatrix(data = data.matrix(test.x), label= test.y)

watchlist <- list(train=dtrain, test=dtest)

start_time <- Sys.time()
bst <- xgb.train(data = dtrain
                 , params = params
                 , watchlist = watchlist
                 , booster = "gbtree" #gblinear
                 , nrounds = 500
                 , early_stopping_rounds = 50
                 , nthread = parallel::detectCores()-2)

end_time <- Sys.time()

cat("Model trained in ",round(difftime(end_time, start_time),3),units(difftime(end_time, start_time)))

( importance <- xgb.importance(colnames(dtrain), model = bst) )

# xgb.ggplot.importance(importance)


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







