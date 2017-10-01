


rm(list=ls()[!ls()%in%c("sale_augmented")])
source("R/00aa-load-packages.R")


if(!exists("sale_augmented")){
  sale_augmented <- tbl_df(read_rds("data/sales_augmented.rds"))
}

sale_modeling <- 
  sale_augmented %>% 
  filter(SALE.PRICE>0) %>% 
  filter(!BUILDING.CLASS.AT.TIME.OF.SALE%in%c("H3","H1")) %>% 
  select(BOROUGH:BUILDING.CLASS.AT.PRESENT,-ADDRESS,-BBL_derive
         , ZIP.CODE:SALE_YEAR,condoflag, lat, lon, Building_Type
         ,LotArea:ExemptTotal
  ) %>% 
  ungroup() %>% 
  mutate(Id = 1:n())


# Train / Test datasets ---------------------------------------------------
set.seed(1)
train <- sample_frac(sale_modeling, 0.7)
train_label <- train$SALE.PRICE


# USE ONE-HOT ENCONDING
# vtreat: http://www.win-vector.com/blog/2017/04/encoding-categorical-variables-one-hot-and-beyond/
# sparse.model.matrix: https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
train <- train %>% select_if(.predicate = is.numeric)

test <- anti_join(sale_modeling,train, by = 'Id')
test_label <- test$SALE.PRICE
test <- test %>% select_if(.predicate = is.numeric)

train$id <- NULL
train$SALE.PRICE <- NULL
test$id <- NULL
test$SALE.PRICE <- NULL

gc()



library(xgboost)
library(Matrix)
params <- list(
  "objective"           = "reg:linear",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.77,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

sparse_matrix <- sparse.model.matrix(Improved~.-1, data = df)

dtrain <- xgb.DMatrix(data = data.matrix(train), label = train_label)
dtest <- xgb.DMatrix(data = data.matrix(test), label=test_label)

watchlist <- list(train=dtrain, test=dtest)
bst <- xgb.train(data=dtrain, params = params, watchlist=watchlist, nrounds = 10)


( importance <- xgb.importance(colnames(dtrain), model = bst) )
# xgb.ggplot.importance(importance)








