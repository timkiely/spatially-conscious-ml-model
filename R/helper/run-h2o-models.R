library(tidyverse)
library(h2o)


# classification models: glm, RF, GBM, Deep Learning, Naive Bayes, Ensemble, XGBoost


# 1) GLM
run_glm_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  
  suppressWarnings(suppressWarnings(h2o.init()))
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  
  # Set predictor and response variables
  Y = "Sold"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.glm(training_frame=train_x
                   , x = X
                   , y = Y
                   , family = "binomial"
                   , alpha = 0.5) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,AUC = h2o.perf@metrics$AUC
      ,MSE = h2o.perf@metrics$MSE
      ,RMSE = h2o.perf@metrics$RMSE
      ,R2 = h2o.perf@metrics$r2
      ,Logloss = h2o.perf@metrics$logloss
      ,Gini = h2o.perf@metrics$Gini
      ,Mean_per_class_error = h2o.perf@metrics$mean_per_class_error
      ,AIC = h2o.perf@metrics$AIC
    )
  
  preds <- predict(model, newdata = test_x)
  
  test_errors <-
    test_x %>% 
    as_tibble() %>% 
    select(Sold) %>% 
    bind_cols(as_tibble(preds))
  
  # Create a basic roc object
  roc_obj <- pROC::roc(test_errors$Sold, test_errors$p1)
  roc_df <- tibble(TPR=rev(roc_obj$sensitivities),FPR=rev(1 - roc_obj$specificities))
  
  
  out_list <- list(roc_df, vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," ROC df")
                       ,paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}


# 2 Random Forrest
run_rf_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "Sold"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.randomForest(training_frame=train_x
                            , validation_frame = valid_x
                            , x = X
                            , y = Y) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,AUC = h2o.perf@metrics$AUC
      ,MSE = h2o.perf@metrics$MSE
      ,RMSE = h2o.perf@metrics$RMSE
      ,R2 = h2o.perf@metrics$r2
      ,Logloss = h2o.perf@metrics$logloss
      ,Gini = h2o.perf@metrics$Gini
      ,Mean_per_class_error = h2o.perf@metrics$mean_per_class_error
      ,AIC = NA
    )
  
  preds <- predict(model, newdata = test_x)
  test_errors <-
    test_x %>% 
    as_tibble() %>% 
    select(Sold) %>% 
    bind_cols(as_tibble(preds))
  
  # Create a basic roc object
  roc_obj <- pROC::roc(test_errors$Sold, test_errors$p1)
  roc_df <- tibble(TPR=rev(roc_obj$sensitivities),FPR=rev(1 - roc_obj$specificities))
  
  
  out_list <- list(roc_df, vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," ROC df")
                       ,paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}


# 3 GBM
run_gbm_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "Sold"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.gbm(training_frame=train_x
                   , validation_frame = valid_x
                   , x = X
                   , y = Y) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,AUC = h2o.perf@metrics$AUC
      ,MSE = h2o.perf@metrics$MSE
      ,RMSE = h2o.perf@metrics$RMSE
      ,R2 = h2o.perf@metrics$r2
      ,Logloss = h2o.perf@metrics$logloss
      ,Gini = h2o.perf@metrics$Gini
      ,Mean_per_class_error = h2o.perf@metrics$mean_per_class_error
      ,AIC = NA
    )
  
  preds <- predict(model, newdata = test_x)
  test_errors <-
    test_x %>% 
    as_tibble() %>% 
    select(Sold) %>% 
    bind_cols(as_tibble(preds))
  
  # Create a basic roc object
  roc_obj <- pROC::roc(test_errors$Sold, test_errors$p1)
  roc_df <- tibble(TPR=rev(roc_obj$sensitivities),FPR=rev(1 - roc_obj$specificities))
  
  
  out_list <- list(roc_df, vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," ROC df")
                       ,paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}



# 4 FF ML ANN
run_dl_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "Sold"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.deeplearning(training_frame=train_x
                            , validation_frame = valid_x
                            , x = X
                            , y = Y) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,AUC = h2o.perf@metrics$AUC
      ,MSE = h2o.perf@metrics$MSE
      ,RMSE = h2o.perf@metrics$RMSE
      ,R2 = h2o.perf@metrics$r2
      ,Logloss = h2o.perf@metrics$logloss
      ,Gini = h2o.perf@metrics$Gini
      ,Mean_per_class_error = h2o.perf@metrics$mean_per_class_error
      ,AIC = NA
    )
  preds <- predict(model, newdata = test_x)
  test_errors <-
    test_x %>% 
    as_tibble() %>% 
    select(Sold) %>% 
    bind_cols(as_tibble(preds))
  
  # Create a basic roc object
  roc_obj <- pROC::roc(test_errors$Sold, test_errors$p1)
  roc_df <- tibble(TPR=rev(roc_obj$sensitivities),FPR=rev(1 - roc_obj$specificities))
  
  
  out_list <- list(roc_df, vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," ROC df")
                       ,paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}




# 5 Regression GLM
run_glm_regression_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  
  # Set predictor and response variables
  Y = "SALE PRICE"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  glm_model <- h2o.glm(training_frame=train_x
                       , x = X
                       , y = Y
                       , family = "gaussian"
                       , alpha = 0.5) 
  
  # View model information: training statistics,performance, important variables
  
  # Predict using GLM model
  h2o.perf = h2o.performance(glm_model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(glm_model)
  
  glm_final_results <- 
    tibble(
      model = model_name
      ,RMSE = h2o.perf@metrics$RMSE
      ,MAE = h2o.perf@metrics$mae
      ,MSE = h2o.perf@metrics$MSE
      ,R2 = h2o.perf@metrics$r2
      ,AIC = h2o.perf@metrics$AIC
    )
  out_list <- list(vars_imp, glm_final_results) 
  names(out_list) <- c(paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}




# 6 Regression Random Forrest
run_rf_regression_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "SALE PRICE"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.randomForest(training_frame=train_x
                            , validation_frame = valid_x
                            , x = X
                            , y = Y) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,RMSE = h2o.perf@metrics$RMSE
      ,MAE = h2o.perf@metrics$mae
      ,MSE = h2o.perf@metrics$MSE
      ,R2 = h2o.perf@metrics$r2
    )
  out_list <- list(vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}


# 3 GBM
run_gbm_regression_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "SALE PRICE"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.gbm(training_frame=train_x
                   , validation_frame = valid_x
                   , x = X
                   , y = Y) 
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,RMSE = h2o.perf@metrics$RMSE
      ,MAE = h2o.perf@metrics$mae
      ,MSE = h2o.perf@metrics$MSE
      ,R2 = h2o.perf@metrics$r2
    )
  out_list <- list(vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}



# 4 FF ML ANN
run_dl_regression_models <- function(data, model_name = NULL){
  # data <- zip_input
  # rm(data)
  suppressWarnings(h2o.init())
  
  train_x = as.h2o(data$train)
  test_x = as.h2o(data$test)
  valid_x = as.h2o(data$validate) 
  
  
  # Set predictor and response variables
  Y = "SALE PRICE"
  X = setdiff(names(train_x), Y)
  
  # Define the data for the model and display the results
  model <- h2o.deeplearning(training_frame=train_x
                            , validation_frame = valid_x
                            , x = X
                            , y = Y
                            , activation = "RectifierWithDropout"
                            , hidden = c(1024,1024)
                            , epochs = 100
                            , l1 = 1e-5
                            , input_dropout_ratio = 0.2
                            , train_samples_per_iteration = -1
                            , classification_stop = -1
                            #, l1=1e-5
  )
  
  # Predict using GLM model
  h2o.perf = h2o.performance(model,  newdata = test_x)
  
  # Look at summary of predictions: probability of TRUE class (p1)
  vars_imp <- h2o.varimp(model)
  
  final_results <- 
    tibble(
      model = model_name
      ,RMSE = h2o.perf@metrics$RMSE
      ,MAE = h2o.perf@metrics$mae
      ,MSE = h2o.perf@metrics$MSE
      ,R2 = h2o.perf@metrics$r2
    )
  out_list <- list(vars_imp, final_results) 
  names(out_list) <- c(paste0(model_name," var imp")
                       ,paste0(model_name," results"))
  h2o.removeAll()
  return(out_list)
  
}





