


# PROBABILITY MODEL EVALUATION --------------------------------------------
evaluate_probability_models <- function(base_data_inflie = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds"
                                        , outfile = "data/processing steps/p15_prob_model_evaluations.rds") {

  if(file.exists(base_data_inflie)){
    message("Evaluating base PROBABILITY model...")
    base_model <- read_rds(base_data_inflie)
    
    base_model <- 
      base_model %>% 
      mutate(y_hat = map(y_hat, ~.x$predict)) %>% 
      mutate(AUC = map2_dbl(.x = y_hat, .y = test.Y, .f = function(x, y) pROC::auc(predictor = as.numeric(x>0.5), response = y)))
    
    # base_model1 <- base_model[2,]
    # table(as.numeric(base_model1$y_hat[[1]]>0.5) == as.numeric(base_model1$test.Y[[1]]))
    # h2o::h2o.varimp(base_model1$modelFits[[1]])
    
  } else message(base_data_inflie, " Not yet avaulable")
  
  
  # zip models --------------------------------------------------------------
  if(file.exists(zip_data_infile)){
    
  } else message(zip_data_infile, " Not yet avaulable")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
  } else message(radii_data_infile, " Not yet avaulable")
  
  
  
 message("Writing probability model evaluations to ",outfile)
 write_rds(base_model, outfile)
 
 best_model <- base_model %>% arrange(-AUC) %>% head(1) %>% select(modelName, AUC)
 message("=========> Best AUC: ", paste(best_model$modelName, round(best_model$AUC, 3)))

 }





# SALES MODEL EVALUATION --------------------------------------------------
evalutate_sales_models <- function(base_data_inflie = "data/processing steps/p12_sale_price_model_base.rds"
                                   , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                                   , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds"
                                   , outfile = "data/processing steps/p16_sales_model_evaluations.rds") {
  
  if(file.exists(base_data_inflie)){
    message("Evaluating base SALES model...")
    base_model <- read_rds(base_data_inflie)
    
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
    
    
    # base_model1 <- base_model[1,]
    # h2o::h2o.varimp(base_model1$modelFits[[1]])
    
  } else message(base_data_inflie, " Not yet available")
  
  
  # zip models --------------------------------------------------------------
  if(file.exists(zip_data_infile)){
    
  } else message(zip_data_infile, " Not yet available")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
  } else message(radii_data_infile, " Not yet available")
  
  
  
  message("Writing sales model evaluations to ", outfile)
  write_rds(base_model, outfile)
  
  best_model <- base_model %>% arrange(-Test_Rsq) %>% head(1) %>% select(modelName, Test_Rsq)
  message("=========> Best Test Rsq: ", paste(best_model$modelName, round(best_model$Test_Rsq, 3)))
  
}


