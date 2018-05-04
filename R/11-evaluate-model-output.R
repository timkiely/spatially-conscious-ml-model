
# TWO functions: one to evaluate the SALES model and one for the PROB model

# PROBABILITY MODEL EVALUATION --------------------------------------------
evaluate_probability_models <- function(base_data_infile = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds"
                                        , outfile = "data/processing steps/p15_prob_model_evaluations.rds") {
  
  if(file.exists(base_data_infile)){
    message("\n Evaluating base PROBABILITY model...")
    base_model <- read_rds(base_data_infile)
    act_pred <- data_frame(actual = base_model$actual, pred = base_model$probs)
    base_model_AUC <- pROC::auc(predictor = act_pred$pred, response = act_pred$actual)[[1]]
    
  } else message(base_data_infile, " Not yet avaulable")
  
  
  # zip models --------------------------------------------------------------
  
  if(file.exists(zip_data_infile)){
    message("\n Evaluating zipcode PROBABILITY model...")
    zip_model <- read_rds(zip_data_infile)
    
    act_pred <- data_frame(actual = zip_model$actual, pred = zip_model$probs)
    zip_model_AUC <- pROC::auc(predictor = act_pred$pred, response = act_pred$actual)[[1]]
    
    
  } else message(zip_data_infile, " Not yet avaulable")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
    message("\n Evaluating radii PROBABILITY model...")
    radii_model <- read_rds(radii_data_infile)
    
    act_pred <- data_frame(actual = radii_model$actual, pred = radii_model$probs)
    radii_model_AUC <- pROC::auc(predictor = act_pred$pred, response = act_pred$actual)[[1]]
    
    
  } else message(radii_data_infile, " Not yet avaulable")
  
  
  
  message("\n Writing probability model evaluations to ",outfile)
  ( all_evals <- data_frame(type = "Prob", base = base_model_AUC, Zip = zip_model_AUC, Radii = radii_model_AUC) )
  write_rds(all_evals, outfile)
  
  
  print(all_evals)
  
}





# SALES MODEL EVALUATION --------------------------------------------------
evalutate_sales_models <- function(base_data_infile = "data/processing steps/p12_sale_price_model_base.rds"
                                   , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                                   , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds"
                                   , outfile = "data/processing steps/p16_sales_model_evaluations.rds") {
  
  if(file.exists(base_data_infile)){
    message("\n Evaluating base SALES model...")
    base_model <- read_rds(base_data_infile)
    
    base_eval <- 
      data_frame(pred = base_model$pred, actual = base_model$actual) %>% 
      nest() %>% 
      mutate(y_hat = map(data, ~.x$pred)
             , test.Y = map(data, ~.x$actual)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
             , Test_MEAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(median(abs(unlist(.x)))))
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
             , COD_Step1 = map2(.x = Test_Average_Sales_Ratio, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-data, -y_hat, -test.Y, -Test_Errors, -Test_Percent_Error, -Test_Sales_Ratios
             , -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    base_eval$data_type <- "Base"
    
  } else message(base_data_infile, " Not yet available")
  
  
  # zip models --------------------------------------------------------------
  if(file.exists(zip_data_infile)){
    
    message("\n Evaluating zip SALES model...")
    zip_model <- read_rds(zip_data_infile)
    
    zip_eval <- 
      data_frame(pred = zip_model$pred, actual = zip_model$actual) %>% 
      nest() %>% 
      mutate(y_hat = map(data, ~.x$pred)
             , test.Y = map(data, ~.x$actual)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
             , Test_MEAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(median(abs(unlist(.x)))))
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
             , COD_Step1 = map2(.x = Test_Average_Sales_Ratio, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-data, -y_hat, -test.Y, -Test_Errors, -Test_Percent_Error, -Test_Sales_Ratios
             , -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    zip_eval$data_type <- "Zip"
    
  } else message(zip_data_infile, " Not yet available")
  
  
  
  
  # radii models ------------------------------------------------------------
  
  if(file.exists(radii_data_infile)){
    
    message("\n Evaluating radii SALES model...")
    radii_model <- read_rds(radii_data_infile)
    
    radii_eval <- 
      data_frame(pred = radii_model$pred, actual = radii_model$actual) %>% 
      nest() %>% 
      mutate(y_hat = map(data, ~.x$pred)
             , test.Y = map(data, ~.x$actual)) %>% 
      mutate(Test_Errors = map2(.x = y_hat, .y = test.Y, .f = ~.y-.x))  %>% 
      mutate(Test_RMSE = map_dbl(.x = Test_Errors, .f = ~as.numeric(sqrt(mean(.x^2))))
             , Test_Rsq = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "pearson")))
             , Test_Spearman = map2_dbl(.x = y_hat, .y = test.Y, .f = ~as.numeric(cor(.x,.y, method = "spearman")))
             , Test_Percent_Error = map2(Test_Errors, test.Y, ~unlist(.x)/.y)
             , Test_MAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(mean(abs(unlist(.x)))))
             , Test_MEAPE = map_dbl(.x = Test_Percent_Error, .f = ~as.numeric(median(abs(unlist(.x)))))
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
             , COD_Step1 = map2(.x = Test_Average_Sales_Ratio, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
             , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
             , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
             , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
             , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
             , Test_COD = COD_Step5*100
      ) %>% 
      select(-data, -y_hat, -test.Y, -Test_Errors, -Test_Percent_Error, -Test_Sales_Ratios, -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
      arrange(-Test_Rsq)
    
    radii_eval$data_type <- "Radii"
    
  } else message(radii_data_infile, " Not yet available")
  
  
  
  message("\nWriting sales model evaluations to ", outfile)
  all_evals <- bind_rows(base_eval, zip_eval, radii_eval) %>% select(data_type, names(base_eval)[!names(base_eval)%in%c("data_type")])
  write_rds(all_evals, outfile)
}


