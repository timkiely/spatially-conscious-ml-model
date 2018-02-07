


# PROBABILITY MODEL EVALUATION --------------------------------------------
evaluate_probability_models <- function(base_data_infile = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds"
                                        , outfile = "data/processing steps/p15_prob_model_evaluations.rds") {

  if(file.exists(base_data_infile)){
    message("\n Evaluating base PROBABILITY model...")
    base_model <- read_rds(base_data_infile)
    
    base_model <- 
      base_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(AUC = map2_dbl(.x = y_hat, .y = test.Y, .f = function(x, y) pROC::auc(predictor = as.numeric(x>0.5), response = y)))
    
    base_model$data_type <- "Base"
    
    # base_model1 <- base_model[2,]
    # table(as.numeric(base_model1$y_hat[[1]]>0.5) == as.numeric(base_model1$test.Y[[1]]))
    # h2o::h2o.varimp(base_model1$modelFits[[1]])
    
  } else message(base_data_infile, " Not yet avaulable")
  
  
  # zip models --------------------------------------------------------------
  
  if(file.exists(zip_data_infile)){
      message("\n Evaluating zipcode PROBABILITY model...")
      zip_model <- read_rds(zip_data_infile)
      
      zip_model <- 
        zip_model %>% 
        mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
        mutate(AUC = map2_dbl(.x = y_hat, .y = test.Y, .f = function(x, y) pROC::auc(predictor = as.numeric(x>0.5), response = y)))
      
      zip_model$data_type <- "Zip"
      
      # zip_model1 <- zip_model[1,]
      # table(as.numeric(zip_model1$y_hat[[1]]>0.5) == as.numeric(zip_model1$test.Y[[1]]))
      # h2o::h2o.varimp(zip_model1$modelFits[[1]])
    
  } else message(zip_data_infile, " Not yet avaulable")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
    message("\n Evaluating radii PROBABILITY model...")
    radii_model <- read_rds(radii_data_infile)
    
    radii_model <- 
      radii_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(AUC = map2_dbl(.x = y_hat, .y = test.Y, .f = function(x, y) pROC::auc(predictor = as.numeric(x>0.5), response = y)))
    
    radii_model$data_type <- "Radii"
    
    # radii_model1 <- radii_model[1,]
    # table(as.numeric(radii_model1$y_hat[[1]]>0.5) == as.numeric(radii_model1$test.Y[[1]]))
    # h2o::h2o.varimp(radii_model1$modelFits[[1]])
    
  } else message(radii_data_infile, " Not yet avaulable")
  
  
  
 message("\nWriting probability model evaluations to ",outfile)
 all_evals <- bind_rows(base_model,zip_model,radii_model)
 write_rds(all_evals, outfile)
 
 best_model <- all_evals %>% arrange(-AUC) %>% head(1) %>% select(modelName, data_type, data_id, AUC)
 message(paste("=========> Best AUC: ", paste(best_model$modelName, best_model$data_type, best_model$data_id, round(best_model$AUC, 3))))

 }





# SALES MODEL EVALUATION --------------------------------------------------
evalutate_sales_models <- function(base_data_infile = "data/processing steps/p12_sale_price_model_base.rds"
                                   , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                                   , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds"
                                   , outfile = "data/processing steps/p16_sales_model_evaluations.rds") {
  
  if(file.exists(base_data_infile)){
    message("\n Evaluating base SALES model...")
    base_model <- read_rds(base_data_infile)
    
    base_model <- 
      base_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
      ) %>% 
      mutate(Test_Sales_Ratios = map2(.x = y_hat, .y = test.Y, .f = ~as.numeric(.x /.y))  
             , Test_Average_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(mean(.x)))
             , Test_SD_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(sd(.x))) 
             , Test_N_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(length(.x))) 
             , Test_ERR_Sales_Ratio = qt(0.975, df=Test_N_Sales_Ratio-1)*Test_SD_Sales_Ratio/sqrt(Test_N_Sales_Ratio) 
             , Test_SR_CI_Low = Test_Average_Sales_Ratio-Test_ERR_Sales_Ratio
             , Test_SR_CI_Hi = Test_Average_Sales_Ratio+Test_ERR_Sales_Ratio
      ) %>% 
      mutate(Test_Median_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(median(.x)))
             , COD_Step1 = map2(.x = Test_Sales_Ratios, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-Test_Percent_Error, -Test_Sales_Ratios, -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    base_model$data_type <- "Base"
    
    # base_model1 <- base_model[1,]
    # h2o::h2o.varimp(base_model1$modelFits[[1]])
    
  } else message(base_data_infile, " Not yet available")
  
  
  # zip models --------------------------------------------------------------
  if(file.exists(zip_data_infile)){
    
    message("\n Evaluating zip SALES model...")
    zip_model <- read_rds(zip_data_infile)
    
    zip_model <- 
      zip_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
      ) %>% 
      mutate(Test_Sales_Ratios = map2(.x = y_hat, .y = test.Y, .f = ~as.numeric(.x /.y))  
             , Test_Average_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(mean(.x)))
             , Test_SD_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(sd(.x))) 
             , Test_N_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(length(.x))) 
             , Test_ERR_Sales_Ratio = qt(0.975, df=Test_N_Sales_Ratio-1)*Test_SD_Sales_Ratio/sqrt(Test_N_Sales_Ratio) 
             , Test_SR_CI_Low = Test_Average_Sales_Ratio-Test_ERR_Sales_Ratio
             , Test_SR_CI_Hi = Test_Average_Sales_Ratio+Test_ERR_Sales_Ratio
      ) %>% 
      mutate(Test_Median_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(median(.x)))
             , COD_Step1 = map2(.x = Test_Sales_Ratios, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-Test_Percent_Error, -Test_Sales_Ratios, -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    zip_model$data_type <- "Zip"
    
    # zip_model1 <- zip_model[1,]
    # h2o::h2o.varimp(zip_model1$modelFits[[1]])
    
  } else message(zip_data_infile, " Not yet available")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
    message("\n Evaluating radii SALES model...")
    radii_model <- read_rds(radii_data_infile)
    
    radii_model <- 
      radii_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
      ) %>% 
      mutate(Test_Sales_Ratios = map2(.x = y_hat, .y = test.Y, .f = ~as.numeric(.x /.y))  
             , Test_Average_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(mean(.x)))
             , Test_SD_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(sd(.x))) 
             , Test_N_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(length(.x))) 
             , Test_ERR_Sales_Ratio = qt(0.975, df=Test_N_Sales_Ratio-1)*Test_SD_Sales_Ratio/sqrt(Test_N_Sales_Ratio) 
             , Test_SR_CI_Low = Test_Average_Sales_Ratio-Test_ERR_Sales_Ratio
             , Test_SR_CI_Hi = Test_Average_Sales_Ratio+Test_ERR_Sales_Ratio
      ) %>% 
      mutate(Test_Median_Sales_Ratio = map_dbl(.x = Test_Sales_Ratios, .f = ~as.numeric(median(.x)))
             , COD_Step1 = map2(.x = Test_Sales_Ratios, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-Test_Percent_Error, -Test_Sales_Ratios, -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    radii_model$data_type <- "Radii"
    
    # radii_model1 <- radii_model[1,]
    # h2o::h2o.varimp(radii_model1$modelFits[[1]])
    
  } else message(radii_data_infile, " Not yet available")
  
  
  
  message("\nWriting sales model evaluations to ", outfile)
  all_evals <- bind_rows(base_model,zip_model,radii_model)
  write_rds(all_evals, outfile)
  
  best_model <- all_evals %>% arrange(Test_RMSE) %>% head(1) %>% select(modelName, data_type, data_id, Test_RMSE)
  message(paste("=========> Best RMSE: ", paste(best_model$modelName, best_model$data_type, best_model$data_id, round(best_model$Test_RMSE, 3))))
  
}


