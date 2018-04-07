# for testing things out

# 5,000 bbls
# BASE: 

source("R/helper/load-packages.R")
source("R/helper/source-files.R")

base1 <- read_rds("data/aux data/sample_p06_base_model_data.rds")
radii_index <- read_rds("data/aux data/radii-index.rds")
radii_data <- radii_index %>% st_set_geometry(NULL)
radii_data$lon <- st_coordinates(radii_index)[,1]
radii_data$lat <- st_coordinates(radii_index)[,2]

inverted_normalized_distance <- function(x){
  x <- if_else(x!=0,1/x,0)
  x <- x/sum(x, na.rm = TRUE)
  x <- if_else(is.nan(x),0,x)
}

distance_weighted_mean <- function(x, w) weighted.mean(x, w, na.rm = T)
radii_mean <- function(x) mean(x, na.rm = T)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
percent_change = function(x) (x-lag(x,1))/lag(x,1)
two_year_sum = function(x) x+lag(x,1)

set.seed(1988)
sample_bbls <- sample_n(distinct(radii_data, bbl, lat, lon), 5000) #reset to 3000
sample_bbls <- bind_rows(sample_bbls, data_frame(bbl = "1_829_16", lat = 40.74504, lon = -73.98949))
# sample_bbls %>% st_as_sf(coords = c("lon","lat"), crs = 32618) %>% st_transform(4326) %>% st_geometry() %>% 
#   leaflet() %>% addTiles() %>% addCircleMarkers()

sold_features <- 
  radii_data %>% 
  # sample_frac(.01) %>% 
  # filter(bbl == '1_829_16') %>% 
  filter(bbl %in% sample_bbls$bbl) %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
                   , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
                   , "Neighbor_SF" = BldgArea)
            , by = c('neighbors'='bbl')
  ) %>% 
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  
  # inverting and normalizing. Closer observations are more alike
  group_by(bbl) %>% 
  mutate(euc_weight = max(Euc_distance)-Euc_distance) %>% 
  
  # creating weight vectors. Taking geometric mean of scaled data
  mutate(dist_weight = euc_weight) %>% 
  
  # joining to the radii index and creating distance-weighted mean objects
  ungroup() %>% 
  select(bbl, neighbors, dist_weight) %>% 
  left_join(select(base1, Year, bbl, Sold, SALE_DATE, SALE_YEAR, Annual_Sales
                   , Years_Since_Last_Sale, BldgArea, UnitsTotal, UnitsRes)
            , by = c('neighbors'='bbl')) %>% 
  filter(Sold == 1) %>% 
  group_by(bbl, Year) %>%
  summarise(Radius_Sold_In_Year = sum(Annual_Sales, na.rm = T)
            , Radius_Years_Since_Last_Sale = radii_mean(Years_Since_Last_Sale)
            , Radius_Res_Units_Sold_In_Year = sum(UnitsRes, na.rm = T)
            , Radius_Units_Sold_In_Year = sum(UnitsTotal, na.rm = T)
            , Radius_SF_Sold_In_Year = sum(BldgArea, na.rm = T)
  ) %>% 
  mutate_at(vars(Radius_Sold_In_Year:Radius_SF_Sold_In_Year), funs(Two_year = two_year_sum)) %>%
  mutate_at(vars(Radius_Sold_In_Year:Radius_SF_Sold_In_Year_Two_year), funs(perc_change = percent_change))


build_features <- 
  radii_data %>% 
  filter(bbl %in% sample_bbls$bbl) %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
                   , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
                   , "Neighbor_SF" = BldgArea)
            , by = c('neighbors'='bbl')
  ) %>% 
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  
  # inverting and normalizing. Closer observations are more alike
  group_by(bbl) %>% 
  mutate(euc_weight = max(Euc_distance)-Euc_distance) %>% 
  
  # creating weight vectors. Taking geometric mean of scaled data
  mutate(dist_weight = euc_weight) %>% 
  
  # joining to the radii index and creating distance-weighted mean objects
  ungroup() %>% 
  select(bbl, neighbors, dist_weight) %>% 
  left_join(select(base1, bbl, Year, Percent_Com:Percent_Other)
            , by = c('neighbors'='bbl')) %>% 
  mutate_at(vars(Percent_Com:Percent_Other), function(x) ifelse(is.na(x),0,x)) %>% 
  group_by(bbl, Year) %>%
  summarise_at(vars(Percent_Com:Percent_Other), funs(dist = distance_weighted_mean(. , dist_weight)
                                                     , basic_mean = radii_mean)
  ) %>% 
  mutate_at(vars(Percent_Com_dist:Percent_Other_dist), funs(perc_change = percent_change))


MA_features <- 
  radii_data %>% 
  # sample_frac(.01) %>% 
  # filter(bbl == '1_829_16') %>% 
  filter(bbl %in% sample_bbls$bbl) %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
                   , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
                   , "Neighbor_SF" = BldgArea)
            , by = c('neighbors'='bbl')
  ) %>% 
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  
  # inverting and normalizing. Closer observations are more alike
  group_by(bbl) %>% 
  mutate(euc_weight = max(Euc_distance)-Euc_distance) %>% 
  
  # creating weight vectors. Taking geometric mean of scaled data
  mutate(dist_weight = euc_weight) %>% 
  
  # joining to the radii index and creating distance-weighted mean objects
  ungroup() %>% 
  select(bbl, neighbors, dist_weight) %>% 
  left_join(select(base1, bbl, Year, SMA_Price_2_year:Percent_Change_EMA_5)
            , by = c('neighbors'='bbl')) %>% 
  group_by(bbl, Year) %>%
  summarise_at(vars(SMA_Price_2_year:Percent_Change_EMA_5), funs(dist = distance_weighted_mean(. , dist_weight)
                                                                 , basic_mean = radii_mean)
  ) %>% 
  mutate_at(vars(SMA_Price_2_year_dist:Percent_Change_EMA_5_basic_mean), funs(perc_change = percent_change))


Intensity_features <- 
  radii_data %>% 
  # sample_frac(.01) %>% 
  # filter(bbl == '1_829_16') %>% 
  filter(bbl %in% sample_bbls$bbl) %>% 
  select(bbl, lat, lon, Building_Type, BldgArea, neighbors) %>% 
  unnest() %>% 
  left_join(select(radii_data, bbl, "lat_neighbor" = lat
                   , "lon_neighbor" = lon,"Nieghbor_BT" = Building_Type
                   , "Neighbor_SF" = BldgArea)
            , by = c('neighbors'='bbl')
  ) %>% 
  mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)) %>%
  
  # inverting and normalizing. Closer observations are more alike
  group_by(bbl) %>% 
  mutate(euc_weight = max(Euc_distance)-Euc_distance) %>% 
  
  # creating weight vectors. Taking geometric mean of scaled data
  mutate(dist_weight = euc_weight) %>% 
  
  # joining to the radii index and creating distance-weighted mean objects
  ungroup() %>% 
  select(bbl, neighbors, dist_weight) %>% 
  left_join(select(base1, bbl, Year, Sold)
            , by = c('neighbors'='bbl')) %>% 
  group_by(bbl, Year) %>%
  summarise(Total_neighbors = n()
            , Total_Neighbors_Sold = sum(Sold, na.rm = T)
            , Percent_Neighbords_Sold = Total_Neighbors_Sold/Total_neighbors) %>% 
  mutate_at(vars(Total_neighbors:Percent_Neighbords_Sold), funs(SMA_2_year = SMA(., n = 2))
            ,vars(Total_neighbors:Percent_Neighbords_Sold), funs(SMA_3_year = SMA(., n = 3))
  ) %>% 
  mutate_at(vars(Total_neighbors:Percent_Neighbords_Sold_SMA_2_year), funs(percent_change = percent_change))





base_filt <- base1 %>% filter(bbl %in% sample_bbls$bbl) %>% filter(!is.na(`SALE PRICE`))
base2 <- base_filt %>% 
  left_join(MA_features, by = c("bbl", "Year")) %>% 
  left_join(sold_features, by = c("bbl", "Year")) %>% 
  left_join(build_features, by = c("bbl", "Year")) %>% 
  left_join(Intensity_features, by = c("bbl", "Year")) %>% 
  select(Sold, `SALE PRICE`,`BUILDING CLASS CATEGORY`:Annual_Sales, Year, best_vars$variable)


# base2 %>% filter(grepl("31 WEST 27 STREET", Address)) %>% glimpse()


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

################################# INPUT #################################
input <- base2
########################################################################

train <- input %>% filter(!Year%in%c(2016, 2017))
validate <- input %>% filter(Year==2016)
test <- input %>% filter(Year==2017)

X <- train %>% select(-Sold, -`SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_names <- names(X)
Y <- train %>% select(`SALE PRICE`) %>% mutate(`SALE PRICE` = as.numeric(`SALE PRICE`))
names(Y) <- Y_names <- "SALE PRICE"
training_frame <- as.h2o(bind_cols(X,Y))

X_val <- validate %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_val_names <- names(X_val)
Y_val <- validate %>% select(`SALE PRICE`) %>% mutate(Sold = as.numeric(`SALE PRICE`))
names(Y_val) <- Y_val_names <- "SALE PRICE"
validation_frame <- as.h2o(bind_cols(X_val,Y_val))



start_time <- Sys.time()-14400
bst <- h2o.randomForest(x = X_names,
                        y = Y_names,
                        training_frame = training_frame,
                        validation_frame = validation_frame, 
                        model_id = "h2o_rf_fit",
                        ntrees = 200,
                        stopping_rounds = 10,
                        seed = 1)
end_time <- Sys.time()-14400
end_time-start_time
print(bst)
h2o.varimp(bst)


X_test <- test %>% select(-Sold, `SALE PRICE`,-c(`BUILDING CLASS CATEGORY`:Annual_Sales))
X_test_names <- names(X_test)
Y_test <- test %>% select(`SALE PRICE`) %>% mutate(`SALE PRICE` = as.numeric(`SALE PRICE`))
names(Y_test) <- Y_test_names <- "SALE PRICE"
test_frame <- as.h2o(bind_cols(X_test, Y_test))

test_frame$preds <- predict(bst, newdata = test_frame)$predict
preds <- as.numeric(as.data.frame(predict(bst, newdata = test_frame)$predict)$predict)

actual <- as.numeric(as.data.frame(test_frame)$`SALE PRICE`)
pred <- as.numeric(as.data.frame(test_frame)$preds)

h2o.varimp(bst) %>% head(20)

eval_model <- 
  data_frame(actual, pred) %>% 
  nest() %>% 
  mutate(y_hat = map(data, ~.x$pred)) %>% 
  mutate(test.Y = map(data, ~.x$actual)) %>% 
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
         , COD_Step1 = map2(.x = Test_Average_Sales_Ratio, .y = Test_Median_Sales_Ratio, .f = ~as.numeric(.x-.y))
         , COD_Step2 = map(.x = COD_Step1, .f = ~abs(.x))
         , COD_Step3 = map_dbl(.x = COD_Step2, .f = ~sum(.x))
         , COD_Step4 = COD_Step3/Test_N_Sales_Ratio
         , COD_Step5 = COD_Step4/Test_Median_Sales_Ratio
         , Test_COD = COD_Step5*100
  ) %>% 
  select(-data, -y_hat, -test.Y, -Test_Errors, -Test_Percent_Error, -Test_Sales_Ratios, -Test_SD_Sales_Ratio, -Test_N_Sales_Ratio, -Test_ERR_Sales_Ratio, -contains("Step")) %>% 
  arrange(-Test_Rsq)

glimpse(eval_model)

eval_model$Model <- "All Feats top vars"
# keep_frame <- bind_rows(keep_frame, eval_model)
# glimpse(keep_frame)

# extract the variables that account for 80% of the importance
best_vars <- 
  h2o.varimp(bst) %>% mutate(cumulative = cumsum(percentage)) %>% 
  filter(cumulative<=.80) %>% 
  select(variable)


beepr::beep(4)
#    View(h2o.varimp(bst))


