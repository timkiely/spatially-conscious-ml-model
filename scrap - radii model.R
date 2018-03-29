# for testing things out
source("R/helper/load-packages.R")
source("R/helper/source-files.R")
base1 <- read_rds("data/aux data/sample_p06_base_model_data.rds")

radii_index <- read_rds("data/aux data/radii-index.rds")
radii_data <- radii_index %>% st_set_geometry(NULL)
radii_data$lon <- st_coordinates(radii_index)[,1]
radii_data$lat <- st_coordinates(radii_index)[,2]


# UDFs
inverted_normalized_distance <- function(x){
  x <- if_else(x!=0,1/x,0)
  x <- x/sum(x, na.rm = TRUE)
  x <- if_else(is.nan(x),0,x)
}

distance_weighted_mean <- function(x, w) weighted.mean(x, w, na.rm = T)
radii_mean <- function(x) mean(x, na.rm = T)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

set.seed(1988)
sample_bbls <- sample_n(distinct(radii_data, bbl, lat, lon),100)
sample_bbls %>% st_as_sf(coords = c("lon","lat"), crs = 32618) %>% st_transform(4326) %>% st_geometry() %>% 
  leaflet() %>% addTiles() %>% addCircleMarkers()

nb_weights <- 
  radii_data %>% 
  #sample_frac(.01) %>% 
  # filter(bbl == '1_829_16') %>% 
  filter(bbl %in% sample_bbls$bbl) %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
           , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
           , "Neighbor_SF" = BldgArea)
    , by = c('neighbors'='bbl')
    ) %>% 
  # distance metrics. Currently using absolute euclidean distance, and Square Footage
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  
  # drop non-like buildings to 0 weight
  mutate(Euc_distance = ifelse(Building_Type == Nieghbor_BT, Euc_distance, max(Euc_distance))) %>% 
  
  # inverting and normalizing. Closer observations are more alike
  group_by(bbl) %>% 
  mutate(euc_weight = max(Euc_distance)-Euc_distance) %>% 
  
  # creating weight vectors. Taking geometric mean of scaled data
  mutate(dist_weight = euc_weight) %>% ungroup() %>% 

  # joining to the radii index and creating distance-weighted mean objects
  select(bbl, neighbors, dist_weight) %>% 
  left_join(select(base1, Year, bbl, Sold, SALE_DATE, SALE_YEAR, Annual_Sales, Years_Since_Last_Sale), by = c('neighbors'='bbl')) %>% 
  group_by(bbl, Year) %>%
  summarise_at(vars(Years_Since_Last_Sale:Percent_Change_EMA_5)
               , funs(dist = distance_weighted_mean(., dist_weight)
                      , sf_dist = distance_weighted_mean(., dist_sf_geometric_weight)
                      , sf = distance_weighted_mean(., sf_weight)
                      , combined = distance_weighted_mean(., combined_weight)
                      , radii_mean)
               )
  
base1 <- base1 %>% left_join(nb_weights, by = c("bbl", "Year"))

base1 %>% filter(grepl("31 WEST 27 STREET", Address)) %>% glimpse()


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
confusionMatrix(table(actual, pred), positive = "1")
(roc <- pROC::roc(actual, probs))



