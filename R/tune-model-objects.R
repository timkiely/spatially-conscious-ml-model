

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




# LASSO regreession -------------------------------------------------------




# CHAID -------------------------------------------------------------------




# KNN mean ----------------------------------------------------------------




# KNN median --------------------------------------------------------------




# MLP ---------------------------------------------------------------------




# RBP ---------------------------------------------------------------------






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
    tuneGrid = expand.grid(nrounds = 500, 
                           max_depth = 6,
                           eta = 0.1,
                           gamma = 0, 
                           colsample_bytree = 1, 
                           min_child_weight = 1, 
                           subsample = 1)
    , preProc = c('center', 'scale')
    
  )
  
}

xgbTreeModel2 <- function(X, Y){
  # tick the progress bar forward
  if(exists("pb",envir = globalenv())){
    pb$tick(tokens = list(what = "XGBoost2 Model"))
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
    tuneGrid = expand.grid(nrounds = 500, 
                           max_depth = 6,
                           eta = 0.1,
                           gamma = 0, 
                           colsample_bytree = 1, 
                           min_child_weight = 1, 
                           subsample = 1)
    , preProc = c('center', 'scale')
    
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
    tuneGrid = expand.grid(nrounds = 500, 
                           lambda = 0, 
                           alpha = 0, 
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





# Model List --------------------------------------------------------------
model_list <- list(
  rpartModel = rpartModel
  , xgbModel = xgbTreeModel
  , xgbLinearModel = xgbLinearModel
  , RFModel = RFModel
  , linearRegModel = linearRegModel
  
) %>%
  enframe(name = 'modelName',value = 'model')

write_rds(model_list, 'data/model-list.rds')
