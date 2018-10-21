
global_time_start <- Sys.time()
## run additional models on refined dataset
source("R/helper/load-packages.R")
source("R/helper/source-files.R")
source("R/helper/run-h2o-models.R")
sample_percent <- 1


# 3) ----------------------------------------------------------------------
# step 3: create test and train model lists

base_subset <- read_rds("data/processing steps/p18_base_model_data_subset.rds") %>% 
  select(-CondoNo)  %>% 
  select(Sold, `SALE PRICE`, everything()) %>%  
  select(-TOTAL_SALES, -SALE_DATE, -SALE_YEAR, -Annual_Sales) %>% 
  select(-BoroCode, -OwnerType, -`BUILDING CLASS AT PRESENT`, -Borough
         , -BldgClass, -`BUILDING CLASS CATEGORY`, -`TAX CLASS AT PRESENT`
         , -`TAX CLASS AT TIME OF SALE`, -`BUILDING CLASS AT TIME OF SALE`
         , -Building_Type,-Percent_Factory, -FactryArea)

zip_subset <- read_rds("data/processing steps/p19_zipcode_model_data_subset.rds")%>% 
  select(-CondoNo)  %>% 
  select(Sold, `SALE PRICE`, everything()) %>%  
  select(-TOTAL_SALES, -SALE_DATE, -SALE_YEAR, -Annual_Sales)%>% 
  select(-BoroCode, -OwnerType, -`BUILDING CLASS AT PRESENT`, -Borough
         , -BldgClass, -`BUILDING CLASS CATEGORY`, -`TAX CLASS AT PRESENT`
         , -`TAX CLASS AT TIME OF SALE`, -`BUILDING CLASS AT TIME OF SALE`
         , -Building_Type,-Percent_Factory, -FactryArea)

radii_subset <- read_rds("data/processing steps/p20_radii_model_data_subset.rds")%>%  
  select(-CondoNo)  %>% 
  select(Sold, `SALE PRICE`, everything()) %>%  
  select(-TOTAL_SALES, -SALE_DATE, -SALE_YEAR, -Annual_Sales)%>% 
  select(-BoroCode, -OwnerType, -`BUILDING CLASS AT PRESENT`, -Borough
         , -BldgClass, -`BUILDING CLASS CATEGORY`, -`TAX CLASS AT PRESENT`
         , -`TAX CLASS AT TIME OF SALE`, -`BUILDING CLASS AT TIME OF SALE`
         , -Building_Type,-Percent_Factory, -FactryArea)


split_train_test <- function(data, name = "NA"){
  set.seed(1)
  train <- data %>% filter(!Year%in%c(2016,2017))
  validate <- data %>% filter(Year%in%c(2016))
  test <- data %>% filter(Year%in%c(2017))
  out_list <- list("train" = train
                   ,"validate" = validate
                   , "test" = test)
  names(out_list) <- paste0(name, ' ',names(out_list))
  out_list
}

base_train_test <- split_train_test(base_subset, name = "Base")
zip_train_test <- split_train_test(zip_subset, name = "Zip")
radii_train_test <- split_train_test(radii_subset, name = "Radii")


# 4)  ---------------------------------------------------------------------
# modeling data
# for dev purposes
set.seed(1987)
model_data_list <- c(base_train_test, zip_train_test, radii_train_test)
model_data_list <- map(model_data_list, .f = ~{sample_frac(.x,sample_percent)})



# base
input_data <- list("train" = model_data_list$`Base train`
                   ,"test" = model_data_list$`Base test`
                   ,"validate" = model_data_list$`Base validate`)  %>% 
  map(~.x %>% 
        select(-`SALE PRICE`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(Sold = as.factor(Sold))
  )



# zip 
zip_input <- list("train" = model_data_list$`Zip train`
                  ,"test" = model_data_list$`Zip test`
                  , "validate" = model_data_list$`Zip validate`) %>% 
  map(~.x %>% 
        select(-`SALE PRICE`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(Sold = as.factor(Sold))
  )



# radii
radii_input <- list("train" = model_data_list$`Radii train`
                    ,"test" = model_data_list$`Radii test`
                    ,"validate" = model_data_list$`Radii validate`) %>% 
  map(~.x %>% 
        select(-`SALE PRICE`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(Sold = as.factor(Sold))
  )



rm(base_glm, zip_glm, radii_glm, base_rf, zip_rf, radii_rf)


# GLM
base_glm <- run_glm_models(input_data, model_name = "Base Prob GLM")
zip_glm <- run_glm_models(zip_input, model_name = "Zip Prob GLM")
radii_glm <- run_glm_models(radii_input, model_name = "Radii Prob GLM")


## Random Forrest
base_rf <- run_rf_models(input_data, model_name = "Base Prob RF")
zip_rf <- run_rf_models(zip_input, model_name = "Zip Prob RF")
radii_rf <- run_rf_models(radii_input, model_name = "Radii Prob RF")

## GBM
base_gbm <- run_gbm_models(input_data, model_name = "Base Prob GBM")
zip_gbm <- run_gbm_models(zip_input, model_name = "Zip Prob GBM")
radii_gbm <- run_gbm_models(radii_input, model_name = "Radii Prob GBM")


# ANN
base_ann <- run_dl_models(input_data, model_name = "Base Prob ANN")
zip_ann <- run_dl_models(zip_input, model_name = "Zip Prob ANN")
radii_ann <- run_dl_models(radii_input, model_name = "Radii Prob ANN")

# Naive Bayes
classification_results <- 
  bind_rows(
    base_glm$`Base Prob GLM results`
    , zip_glm$`Zip Prob GLM results`
    , radii_glm$`Radii Prob GLM results`
    , base_rf$`Base Prob RF results`
    , zip_rf$`Zip Prob RF results`
    , radii_rf$`Radii Prob RF results`
    , base_gbm$`Base Prob GBM results`
    , zip_gbm$`Zip Prob GBM results`
    , radii_gbm$`Radii Prob GBM results`
    , base_ann$`Base Prob ANN results`
    , zip_ann$`Zip Prob ANN results`
    , radii_ann$`Radii Prob ANN results`
  )

classification_results_list <- 
  list(
    base_glm
    , zip_glm
    , radii_glm
    , base_rf
    , zip_rf
    , radii_rf
    , base_gbm
    , zip_gbm
    , radii_gbm
    , base_ann
    , zip_ann
    , radii_ann
  )
write_rds(classification_results, 'data/aux data/many-models-classification-results.rds')
write_rds(classification_results_list, 'data/aux data/many-models-classification-results-list.rds')


# regression models -------------------------------------------------------
# Regression models

# modeling data
# for dev purposes
set.seed(1987)
model_data_list <- c(base_train_test, zip_train_test, radii_train_test)
model_data_list <- map(model_data_list, .f = ~{sample_frac(.x,sample_percent)})



# base
input_data <- list("train" = model_data_list$`Base train`
                   ,"test" = model_data_list$`Base test`
                   ,"validate" = model_data_list$`Base validate`) 

input_data <- input_data %>% 
  map(~.x %>% 
        select(-`Sold`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(`SALE PRICE` = as.numeric(`SALE PRICE`))
  )



# zip 
zip_input <- list("train" = model_data_list$`Zip train`
                  ,"test" = model_data_list$`Zip test`
                  , "validate" = model_data_list$`Zip validate`) 

zip_input <- zip_input %>% 
  map(~.x %>% 
        select(-`Sold`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(`SALE PRICE` = as.numeric(`SALE PRICE`))
  )



# radii
radii_input <- list("train" = model_data_list$`Radii train`
                    ,"test" = model_data_list$`Radii test`
                    ,"validate" = model_data_list$`Radii validate`)


radii_input <- radii_input %>% 
  map(~.x %>% 
        select(-`Sold`,-Address
               ,-bbl, -OwnerName, -ZoneDist1
               ,-ZoneDist2) %>% 
        mutate(`SALE PRICE` = as.numeric(`SALE PRICE`))
  )



rm(reg_base_glm, reg_zip_glm, reg_radii_glm, reg_base_rf, reg_zip_rf, reg_radii_rf)

# GLM
reg_base_glm <- run_glm_regression_models(input_data, model_name = "Base Price GLM")
reg_zip_glm <- run_glm_regression_models(zip_input, model_name = "Zip Price GLM")
reg_radii_glm <- run_glm_regression_models(radii_input, model_name = "Radii Price GLM")


## Random Forrest
reg_base_rf <- run_rf_regression_models(input_data, model_name = "Base Price RF")
reg_zip_rf <- run_rf_regression_models(zip_input, model_name = "Zip Price RF")
reg_radii_rf <- run_rf_regression_models(radii_input, model_name = "Radii Price RF")

## GBM
reg_base_gbm <- run_gbm_regression_models(input_data, model_name = "Base Price GBM")
reg_zip_gbm <- run_gbm_regression_models(zip_input, model_name = "Zip Price GBM")
reg_radii_gbm <- run_gbm_regression_models(radii_input, model_name = "Radii Price GBM")


# ANN
reg_base_ann <- run_dl_regression_models(input_data, model_name = "Base Price ANN")
reg_zip_ann <- run_dl_regression_models(zip_input, model_name = "Zip Price ANN")
reg_radii_ann <- run_dl_regression_models(radii_input, model_name = "Radii Price ANN")



regression_results <- 
  bind_rows(
    reg_base_glm$`Base Price GLM results`
    , reg_zip_glm$`Zip Price GLM results`
    , reg_radii_glm$`Radii Price GLM results`
    , reg_base_rf$`Base Price RF results`
    , reg_zip_rf$`Zip Price RF results`
    , reg_radii_rf$`Radii Price RF results`
    , reg_base_gbm$`Base Price GBM results`
    , reg_zip_gbm$`Zip Price GBM results`
    , reg_radii_gbm$`Radii Price GBM results`
    , reg_base_ann$`Base Price ANN results`
    , reg_zip_ann$`Zip Price ANN results`
    , reg_radii_ann$`Radii Price ANN results`
  )

regression_results_list <- 
  list(reg_base_glm
       , reg_zip_glm
       , reg_radii_glm
       , reg_base_rf
       , reg_zip_rf
       , reg_radii_rf
       , reg_base_gbm
       , reg_zip_gbm
       , reg_radii_gbm
       , reg_base_ann
       , reg_zip_ann
       , reg_radii_ann)

write_rds(regression_results_list , 'data/aux data/many-models-regression-results-list.rds')
write_rds(regression_results, 'data/aux data/many-models-regression-results.rds')


global_time_end <- Sys.time()

# 1% = ~5 mins
# 5% = ~13 mins
# 10% = ~20 mins
# 100% = ~1..6 hours with 60GiB and 36 cores
message("Total run time: ", round(difftime(global_time_end, global_time_start),2)," ",units(difftime(global_time_end, global_time_start)))


## AUTOM ML MODELS

# train_x = as.h2o(input_data$train)
# test_x = as.h2o(input_data$test)
# 
# # Set predictor and response variables
# Y = "Sold"
# X = setdiff(names(train_x), Y)
# 
# # Define the data for the model and display the results
# automl_model <- h2o.automl(training_frame=train_x
#                          , validation_frame = test_x
#                          , x = X
#                          , y = Y)

















