

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


radii_index <- read_rds("data/aux data/radii-index.rds")
radii_data <- radii_index %>% st_set_geometry(NULL)
radii_data$lon <- st_coordinates(radii_index)[,1]
radii_data$lat <- st_coordinates(radii_index)[,2]

inverted_normalized_distance <- function(x){
  x <- if_else(x!=0,1/x,0)
  x <- x/sum(x, na.rm = TRUE)
}

distance_weighted_mean <- function(x, w) weighted.mean(x, w, na.rm = T)
radii_mean <- function(x) mean(x, na.rm = T)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

nb_weights <- 
  radii_data %>% 
  #sample_frac(.01) %>% 
  #filter(bbl == '1_10_14') %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
           , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
           , "Neighbor_SF" = BldgArea)
    , by = c('neighbors'='bbl')
    ) %>% 
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  mutate(absolute_sf_difference = abs(BldgArea-Neighbor_SF)) %>% 
  
  
  # TEST: drop unlike buildings weights to zero. 
  # Run 1 AUC: 0.8397
  # Run 2 AUC: 0.8454 (drop unlike building class to 0 weight)
  
                  #   pred
      # actual     0     1
              # 0 18480  5742
              # 1   535  2208
  
# test 1 0.01 data AUC: 0.85
# test 2 0.01 data AUC 0.8512 no change
# test 3 0.01 data going back to just inverted distance (no sf consideration) 0.8518

  mutate(Euc_distance = ifelse(Building_Type == Nieghbor_BT, Euc_distance, 0)) %>% 
  mutate(absolute_sf_difference = ifelse(Building_Type == Nieghbor_BT, absolute_sf_difference, NA)) %>% 
  
  group_by(bbl) %>% 
  mutate(sf_weight = inverted_normalized_distance(absolute_sf_difference)) %>% 
  mutate(euc_weight = inverted_normalized_distance(Euc_distance)) %>% 
  mutate(weights = euc_weight) %>% arrange(sf_distance_rank) %>% 
  ungroup() %>% 

  select(bbl, neighbors, weights) %>% 
  left_join(select(base1, Year, bbl, Years_Since_Last_Sale:Percent_Change_EMA_5), by = c('neighbors'='bbl')) %>% 
  group_by(bbl, Year) %>%
  summarise_at(vars(Years_Since_Last_Sale:Percent_Change_EMA_5), funs(distance_weighted_mean(., weights), radii_mean))
  
base1 <- base1 %>% left_join(nb_weights, by = c("bbl", "Year"))

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

train <- base1 %>% filter(!Year%in%c(2016, 2017))
validate <- base1 %>% filter(Year==2016)
test <- base1 %>% filter(Year==2017)

X <- train %>% select(-Sold, -`SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_names <- names(X)
Y <- train %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
names(Y) <- Y_names <- "Sold"
training_frame <- as.h2o(bind_cols(X,Y))

X_val <- validate %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_val_names <- names(X_val)
Y_val <- validate %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
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


X_test <- test %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_test_names <- names(X_test)
Y_test <- test %>% select(Sold) %>% mutate(Sold = as.factor(Sold))
names(Y_test) <- Y_test_names <- "Sold"
test_frame <- as.h2o(bind_cols(X_test,Y_test))
test_frame$preds <- predict(bst, newdata = test_frame, type = "probs")$predict
probs <- as.numeric(as.data.frame(predict(bst, newdata = test_frame, type = "probs")$p1)$p1)

actual <- recode(as.numeric(as.data.frame(test_frame)$Sold),0,1)
pred <- recode(as.numeric(as.data.frame(test_frame)$preds), 0,1)
(roc <- pROC::roc(actual, probs))
confusionMatrix(table(actual, pred), positive = "1")

test_frame %>% 
  as_tibble() %>% 
  group_by(Building_Type) %>% 
  nest() %>% 
  mutate(actual = map(data, ~as.numeric(.x$Sold))
         , preds = map(data, ~as.numeric(.x$preds))
  ) %>% 
  mutate(auc = map2_dbl(actual, preds, ~pROC::roc(.x, .y)$auc)
         , sensitivity = map2_dbl(actual, preds, ~pROC::roc(.x, .y)$sensitivities[2])
         , specificity = map2_dbl(actual, preds, ~pROC::roc(.x, .y)$specificities[2])
  )





