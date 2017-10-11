

# model list



# Linear Regression -------------------------------------------------------

linearRegModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Linear Regression Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'lm',
    trControl = ctrl,
    preProc = c('center', 'scale')
    
  )
  
  
}




# LASSO regression -------------------------------------------------------

lassoRegModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Lasso Regression Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'lasso',
    trControl = ctrl,
    tuneGrid = expand.grid(fraction =0.9),
    preProc = c('center', 'scale')
    
  )
  
  
}



# KNN mean ----------------------------------------------------------------


KNNModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "KNN Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'knn',
    trControl = ctrl,
    tuneGrid = expand.grid(k = c(5,10,15,20)),
    preProc = c('center', 'scale')
    
  )
  
  
}


# MLP ---------------------------------------------------------------------

MLPModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "MLP Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'mlp',
    trControl = ctrl,
    tuneGrid = expand.grid(size = c(1,3,5,7)),
    preProc = c('center', 'scale')
    
  )
  
  
}



# RBP ---------------------------------------------------------------------

RBPModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Radial Basis Function Netowrk Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'rbf',
    trControl = ctrl,
    tuneGrid = expand.grid(size = c(3)),
    preProc = c('center', 'scale')
    
  )
  
  
}



# RPART -------------------------------------------------------------------
rpartModel <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Rpart Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'rpart2',
    trControl = ctrl,
    tuneGrid = data.frame(maxdepth=c(2,3,4,5)),
    preProc = c('center', 'scale')
    
  )
  
  
}




# XGBoost -----------------------------------------------------------------
xgbTreeModel <- function(X, Y){
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x=X,
    y=Y,
    method = 'xgbTree',
    trControl = ctrl,
    #objective = "reg:linear",
    tuneGrid = expand.grid(nrounds = c(150), 
                           max_depth = 6,
                           eta = 0.1,
                           gamma = 0, 
                           colsample_bytree = 1, 
                           min_child_weight = 1, 
                           subsample = 1)
    , preProc = c('center', 'scale')
    
  )
  
}



# native implementation of xgboost with early stopping
xgbTreeModel2 <- function(X, watchlist){
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost2 Model"))
  }
  
  ctrl <- list(
    "objective"           = "reg:linear",
    "eval_metric"         = "rmse", #"mae"
    "eta"                 = 0.1,
    "max_depth"           = 6,
    "min_child_weight"    = 2,
    "gamma"               = 0.70,
    "subsample"           = 0.77,
    "colsample_bytree"    = 0.95,
    "alpha"               = 2e-05,
    "lambda"              = 10
  )
  
  bst <- xgb.train(data = X
                   , params = ctrl
                   , watchlist = watchlist
                   , booster = "gbtree" #gblinear
                   , nrounds = 500
                   , early_stopping_rounds = 100
                   , verbose = 0
                   #, nthread = 1
  )
}


# XGBoost Linear -----------------------------------------------------------------
xgbLinearModel <- function(X, Y){
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost Linear Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x=X,
    y=Y,
    method = 'xgbLinear',
    trControl = ctrl,
    #objective = "reg:linear",
    tuneGrid = expand.grid(nrounds = c(150),  
                           lambda = c(0.5), 
                           alpha = c(0.5), 
                           eta = 0.1),
    preProc = c('center', 'scale'),
    allowParallel = TRUE
    
  )
  
}


# Random Forrest ----------------------------------------------------------
RFModel <- function(X, Y){
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Random Forrest Model"))
  }
  
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 5
  )
  train(
    x=X,
    y=Y,
    method = 'rf',
    trControl = ctrl,
    tuneGrid = expand.grid(.mtry =  sqrt(ncol(X))),
    metric = 'RMSE',
    preProc = c('center', 'scale')
    
  )
  
}



# native implementation of xgboost with early stopping
xgbRFmodel <- function(X, Y){
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost RF Model"))
  }
  
  bst <- xgboost(data = X
                 , label = Y
                 , max_depth = 10
                 , num_parallel_tree = 10000
                 , subsample = 0.2
                 , colsample_bytree = 0.2
                 , nrounds = 1
                 , objective = "reg:linear"
                 , eta = 0.01
                 , verbose = 0)
  bst
}


# Model List --------------------------------------------------------------
model_list <- 
  tibble::enframe(
    list(
      rpartModel = rpartModel
      , xgbModel = xgbTreeModel
      , xgbTreeModel2 = xgbTreeModel2
      , xgbLinearModel = xgbLinearModel
      , RFModel = RFModel
      , xgbRFmodel = xgbRFmodel
      , linearRegModel = linearRegModel
      , lassoRegModel = lassoRegModel
      , KNNModel = KNNModel
      , MLPModel = MLPModel
      #, RBPModel = RBPModel 
      
    ) 
    ,name = 'modelName',value = 'model')

readr::write_rds(model_list, 'data/model-list.rds')
