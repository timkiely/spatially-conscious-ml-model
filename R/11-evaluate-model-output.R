
evaluate_probability_models <- function(base_data_inflie = "data/processing steps/p09_prob_of_sale_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds") {
  
  if(file.exists(base_data_inflie)){
    base_model <- read_rds(base_data_inflie)
  
    base_model1 <- base_model[1,]
    pROC::auc(predictor = as.numeric(base_model1$y_hat[[1]]$predict>0.5), response = as.numeric(base_model1$test.Y[[1]]))
    table(as.numeric(base_model1$y_hat[[1]]$predict>0.5) == as.numeric(base_model1$test.Y[[1]]))
    h2o::h2o.varimp(base_model1$modelFits[[1]])
    base_model1$modelFits[[1]] %>% plot()
    
  } else message(base_data_inflie, " Not yet avaulable")
  
  if(file.exists(zip_data_infile)){
  } else message(zip_data_infile, " Not yet avaulable")
  
  
  
  
  if(file.exists(radii_data_infile)){
  } else message(radii_data_infile, " Not yet avaulable")
    message("TODO: CREATE EVAL FUNCTION FOR PROB MODEL")
}


evalutate_sales_models <- function(base_data_inflie = "data/processing steps/p12_sale_price_model_base.rds"
                                        , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                                        , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds") {
  message("TODO: CREATE EVAL FUNCTION FOR SALES MODEL")
}