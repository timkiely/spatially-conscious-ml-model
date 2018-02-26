
# for testing things out



# pluto1 <- read_rds("data/processing steps/p01_pluto_raw.rds")
# pad1 <- read_rds("data/processing steps/p02_pad_raw.rds")
# sales1 <- read_rds("data/processing steps/p03_sales_raw.rds")
# sales2 <- read_rds("data/processing steps/p04_sales_and_pad.rds")
# pluto2 <- read_rds("data/processing steps/p05_pluto_with_sales.rds")

# base1 <- read_rds("data/processing steps/p06_base_model_data.rds")
# "data/processing steps/p07_zipcode_model_data.rds"
# radii <- read_rds("data/processing steps/p08_radii_model_data.rds")

# "data/processing steps/p09_prob_of_sale_model_base.rds"
# "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
# "data/processing steps/p11_prob_of_sale_model_radii.rds"

# "data/processing steps/p12_sale_price_model_base.rds"
# "data/processing steps/p13_sale_price_model_zipcode.rds"
# "data/processing steps/p14_sale_price_model_radii.rds"

# "data/processing steps/p15_prob_model_evaluations.rds"
# "data/processing steps/p16_sales_model_evaluations.rds"

source("R/helper/load-packages.R")
source("R/helper/source-files.R")

# base1 <- read_rds("data/processing steps/p06_base_model_data.rds")
# base_samp <- base1 %>% filter(BoroCode==1)
# write_rds(base_samp, "data/aux data/sample_p06_base_model_data.rds")
base1 <- read_rds("data/aux data/sample_p06_base_model_data.rds")


base1 %>% glimpse()
base1 %>% summary()


message("Initiating h2o clusters...")
suppressMessages({
  suppressWarnings({
    sink(".sink-output")
    h2o.init(nthreads = -1) #Number of threads -1 means use all cores on your machine
    options("h2o.use.data.table"=TRUE)
    h2o.no_progress()
    h2o.removeAll()
    sink(NULL)
  })
})

train <- base1 %>% filter(Year!=2017) %>% filter(OfficeArea>0)
test <- base1 %>% filter(Year==2017) %>% filter(OfficeArea>0)

X <- train %>% select(-Sold, -`SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_names <- names(X)
Y <- train %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
names(Y) <- Y_names <- "Sold"
training_frame <- as.h2o(bind_cols(X,Y))

X_val <- test %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_val_names <- names(X_val)
Y_val <- test %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
names(Y_val) <- Y_val_names <- "Sold"
validation_frame <- as.h2o(bind_cols(X_val,Y_val))



start_time <- Sys.time()-14400
bst <- h2o.randomForest(x = X_names,
                        y = Y_names,
                        training_frame = training_frame,
                        validation_frame = validation_frame, 
                        model_id = "h2o_rf_fit",
                        ntrees = 200,
                        stopping_rounds = 10,
                        stopping_metric = "AUC",
                        seed = 1)
end_time <- Sys.time()-14400
end_time-start_time
print(bst)
h2o.varimp(bst)

# h2o.gainsLift(bst, valid = F, xval = T)

validation_frame_bak <- validation_frame
validation_frame_bak$preds <- predict(bst, newdata = validation_frame_bak, type = 'probs')$predict

actual <- as.numeric(as.data.frame(validation_frame_bak$Sold)$Sold)
pred <- as.numeric(as.data.frame(validation_frame_bak$preds)$preds)
(roc <- pROC::roc(actual, pred))

validation_frame_bak %>% 
  as_tibble() %>% 
  group_by(Building_Type) %>% 
  nest() %>% 
  mutate(roc = map_dbl(data, ~as.numeric(pROC::roc(as.numeric(.x$Sold), as.numeric(.x$preds))$auc)))





