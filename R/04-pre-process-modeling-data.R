


rm(list=ls()[!ls()%in%c("sale_augmented")])
source("R/00aa-load-packages.R")
source("R/helper-functions.R")


if(!exists("sale_augmented")){
  sale_augmented <- tbl_df(read_rds("data/sales_augmented.rds"))
}

sale_modeling <- 
  sale_augmented %>% 
  PROCESS_SALES_DATA() %>% 
  select(BOROUGH:BUILDING.CLASS.AT.PRESENT,-ADDRESS,-BBL_derive
         , ZIP.CODE:SALE_YEAR,condoflag, lat, lon, Building_Type
         , LotArea:ExemptTotal
  ) %>% 
  mutate(Id = 1:n())


# Train / Test datasets ---------------------------------------------------
set.seed(1)
train <- sample_frac(sale_modeling, 0.7)
train_label <- train$SALE.PRICE
test <- anti_join(sale_modeling,train, by = 'Id')
test_label <- test$SALE.PRICE
train$id <- NULL
test$id <- NULL


# ================== PROCESSING =======================
processing_time <- Sys.time()


# Numeric Columns ---------------------------------------------------------
num_train <- train %>% select_if(.predicate = is.numeric)
num_test <- test %>% select_if(.predicate = is.numeric)

pprocess_num_train <- preProcess(num_train %>% select(-SALE.PRICE), method = c("corr", "nzv","medianImpute"))

num_train <- predict(pprocess_num_train, num_train)
num_test <- predict(pprocess_num_train, num_test)


# Processed numeric columns -----------------------------------------------
num_train_processed <- train %>% select_if(.predicate = is.numeric) %>% as.data.frame()
num_test_processed <- test %>% select_if(.predicate = is.numeric) %>% as.data.frame()

pprocess_num_train_processed <- preProcess(num_train_processed %>% select(-SALE.PRICE), method = c("BoxCox", "center", "scale", "bagImpute", "corr", "nzv"))

num_train_processed <- predict(pprocess_num_train_processed, num_train_processed)
num_test_processed <- predict(pprocess_num_train_processed, num_test_processed)


# PCA, ICA or Spatial Sign -----------------------------------------------
num_train_component <- train %>% select_if(.predicate = is.numeric) %>%  as.data.frame()
num_test_component <- test %>% select_if(.predicate = is.numeric) %>% as.data.frame()

pprocess_num_train_component <- preProcess(num_train_component %>% select(-SALE.PRICE), method = c("ica"), n.comp = 10)

num_train_component <- predict(pprocess_num_train_component, num_train_component) %>% filter(!is.na(ICA1))
num_test_component <- predict(pprocess_num_train_component, num_test_component) %>% filter(!is.na(ICA1))


# VTREAT ------------------------------------------------------------------
# vtreat: http://www.win-vector.com/blog/2017/04/encoding-categorical-variables-one-hot-and-beyond/

cores <- detectCores()
clust <- makeCluster(cores-1)

dTrainN <- train
dTestN <- test
treat_cols <- colnames(dTrainN)[!colnames(dTrainN)%in%c("Id")]

# treatment
treatmentsN = designTreatmentsN(dframe = dTrainN, varlist = treat_cols, outcomename = 'SALE.PRICE', parallelCluster = clust)
stopCluster(clust)

# treated train 
dTrainNTreated <- prepare(treatmentsN,dTrainN,
                          pruneSig=c(),scale=TRUE)
# treated test 
dTestNTreatedS <- prepare(treatmentsN,dTestN,
                          pruneSig=c(),scale=TRUE)

# remove NZV and correlated columns
vtreat_post_process <- preProcess(dTrainNTreated %>% select(-SALE.PRICE), method = c("corr", "nzv"))
dTrainNTreated <- predict(vtreat_post_process, dTrainNTreated)
dTestNTreatedS <- predict(vtreat_post_process, dTestNTreatedS)



# Process time
processing_time_end <- Sys.time()
total_process_time <- difftime(processing_time_end,processing_time)

# write to disk -----------------------------------------------------------
write_rds(
  list('train_vtreated' = tbl_df(dTrainNTreated)
       , "test_vtreated" = tbl_df(dTestNTreatedS)
       , "train_numeric_only" = tbl_df(num_train)
       , "test_numeric_only" = tbl_df(num_test)
       , "train_num_processed" = tbl_df(num_train_processed)
       , "test_num_processed" = tbl_df(num_test_processed)
       , "train_num_component" = tbl_df(num_train_component)
       , "test_num_component" = tbl_df(num_test_component)
  )
  , "data/processed-modeling-data.rds", compress = 'gz'
)

# previous processing time: 8.2 mins
message("Total processing time ", round(total_process_time,3)," ",units(total_process_time))


