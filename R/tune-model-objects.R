

# model list



# Linear Regression -------------------------------------------------------

linearRegModel <- function(X, Y, idx) {
  p$tick()$print()
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
  p$tick()$print()
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
  p$tick()$print()
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
    tuneGrid = expand.grid(nrounds = c(100,300,500), 
                           max_depth = c(2,4,6) ,
                           eta = 0.1,
                           gamma = 1, 
                           colsample_bytree = 1, 
                           min_child_weight = 1, 
                           subsample = 1),
    preProc = c('center', 'scale')
    
  )
}



# Random Forrest ----------------------------------------------------------
RFModel <- function(X, Y){
  p$tick()$print()
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
  , RFModel = RFModel
  , linearRegModel = linearRegModel
  
  ) %>%
  enframe(name = 'modelName',value = 'model')

write_rds(model_list, 'data/model-list.rds')
