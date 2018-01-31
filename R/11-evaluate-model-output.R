
evaluate_probability_models <- function(base_data_inflie = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds"
                                        , outfile = "data/processing steps/p15_prob_model_evaluations.rds") {
  
  if(file.exists(base_data_inflie)){
    message("Evaluating base probability model...")
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


evalutate_sales_models <- function(base_data_inflie = "data/processing steps/p12_sale_price_model_base.rds"
                                   , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                                   , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds"
                                   , outfile = "data/processing steps/p16_sales_model_evaluations.rds") {
  message("TODO: CREATE EVAL FUNCTION FOR SALES MODEL")
}


