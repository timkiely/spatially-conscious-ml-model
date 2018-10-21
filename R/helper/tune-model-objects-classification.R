

# model list


# LASSO regression -------------------------------------------------------

logisticRegression <- function(X, Y) {
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "Logistic Regression Model"))
  }
  
  ctrl <- trainControl(
    ## 5-fold CV
    method = "repeatedcv", 
    number = 5
  )
  train(
    x = X,
    y = Y,
    method = 'plr',
    trControl = ctrl,
    tuneGrid = expand.grid(cp = c(0, .5, 1),
                           lambda = c(.1, 1)),
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
    method = "none"
  )
  train(
    x = X,
    y = Y,
    method = 'kknn',
    trControl = ctrl,
    tuneGrid = expand.grid(kmax = c(50)
                           , distance = 2
                           , kernel = "optimal"
    ),
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
    pb$tick(tokens = list(what = "Radial Basis Function Network Model"))
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




# XGBoost (caret) -----------------------------------------------------------------
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




# XGBoost GBM -------------------------------------------------------------

xgbTreeModel2 <- function(X, watchlist){
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost2 Model"))
  }
  
  ctrl <- list(
    #"objective"           = "reg:linear",
    "objective"           = "binary:logistic",
    "eval_metric"         = "error", #"rmse"
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
  )
}


# H2O GBM -----------------------------------------------

h2oGBMmodel <- function(X, Y, training_frame, validation_frame){
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "h2o GBM Model"))
  }
  
  
  
  bst <- h2o.gbm(x = X,
                 y = Y,
                 training_frame = training_frame,
                 validation_frame = validation_frame, 
                 model_id = "h2o_gbm_fit",
                 ntrees = 500,
                 max_depth = 6, 
                 stopping_rounds = 50,
                 stopping_metric = "AUTO",
                 learn_rate = 0.01, 
                 seed = 1)
  
  bst
}




# XGBoost Random Forrest --------------------------------------------------
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
                 , objective = "binary:logistic"
                 , eta = 0.01
                 , verbose = 0)
  bst
}



# H2O Random Forrest ------------------------------------------------------
h2oRFmodel <- function(X, Y, training_frame, validation_frame){
  
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "h2o RF Model"))
  }
  
  
  
  bst <- h2o.randomForest(x = X,
                          y = Y,
                          training_frame = training_frame,
                          validation_frame = validation_frame, 
                          model_id = "h2o_rf_fit",
                          ntrees = 200,
                          stopping_rounds = 5,
                          stopping_metric = "AUTO",
                          seed = 1)
  
  bst
}





# Model List --------------------------------------------------------------
model_list <- 
  tibble::enframe(
    list(
      rpartModel = rpartModel
      , xgbModel = xgbTreeModel
      , xgbTreeModel2 = xgbTreeModel2
      , xgbRFmodel = xgbRFmodel
      , logisticRegModel = logisticRegression
      , KNNModel = KNNModel
      , MLPModel = MLPModel
      , RBPModel = RBPModel 
      , h2oRFmodel = h2oRFmodel
      , h2oGBMmodel = h2oGBMmodel)
      ,name = 'modelName',value = 'model'
    ) 
  

readr::write_rds(model_list, 'data/aux data/model-list-classification.rds')
