create_radii_features <- function(pluto_model, radii_index) {
  
  message("     Starting radii feature creation...")
  radii_feature_start <- Sys.time()
  
  radii_data <- radii_index %>% st_set_geometry(NULL)
  radii_data$lon <- st_coordinates(radii_index)[,1]
  radii_data$lat <- st_coordinates(radii_index)[,2]
  
  

# UDFs --------------------------------------------------------------------

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


# create features ---------------------------------------------------------
  message("     Bulding sales features...")
  sold_features <- radii_data %>% 
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
    left_join(select(pluto_model, Year, bbl, Sold, SALE_DATE, SALE_YEAR, Annual_Sales
                     , Years_Since_Last_Sale, BldgArea, UnitsTotal, UnitsRes)
              , by = c('neighbors'='bbl')) %>% 
    filter(Sold == 1) %>% 
    group_by(bbl, Year) %>%
    summarise(Radius_Total_Sold_In_Year = sum(Annual_Sales, na.rm = T)
              , Radius_Average_Years_Since_Last_Sale = radii_mean(Years_Since_Last_Sale)
              , Radius_Res_Units_Sold_In_Year = sum(UnitsRes, na.rm = T)
              , Radius_All_Units_Sold_In_Year = sum(UnitsTotal, na.rm = T)
              , Radius_SF_Sold_In_Year = sum(BldgArea, na.rm = T)
    ) %>% 
    mutate_at(vars(Radius_Total_Sold_In_Year:Radius_SF_Sold_In_Year), funs(sum_over_2_years = two_year_sum)) %>%
    mutate_at(vars(Radius_Total_Sold_In_Year:Radius_SF_Sold_In_Year_sum_over_2_years), funs(percent_change = percent_change))
  
  message("     Bulding building features...")
  building_features <- radii_data %>% 
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
    left_join(select(pluto_model, bbl, Year, Percent_Com:Percent_Other)
              , by = c('neighbors'='bbl')) %>% 
    mutate_at(vars(Percent_Com:Percent_Other), function(x) ifelse(is.na(x),0,x)) %>% 
    group_by(bbl, Year) %>%
    summarise_at(vars(Percent_Com:Percent_Other), funs(dist = distance_weighted_mean(. , dist_weight)
                                                       , basic_mean = radii_mean)) %>% 
    mutate_at(vars(Percent_Com_dist:Percent_Other_dist), funs(perc_change = percent_change))
  
  message("     Bulding moving average features...")
  MA_features <- radii_data %>% 
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
    left_join(select(pluto_model, bbl, Year, SMA_Price_2_year:Percent_Change_EMA_5)
              , by = c('neighbors'='bbl')) %>% 
    mutate_at(vars(SMA_Price_2_year:Percent_Change_EMA_5), function(x) ifelse(is.nan(x), NA, x)) %>% 
    group_by(bbl, Year) %>%
    summarise_at(vars(SMA_Price_2_year:Percent_Change_EMA_5), funs(dist = distance_weighted_mean(. , dist_weight)
                                                                   , basic_mean = radii_mean)) %>% 
    mutate_at(vars(SMA_Price_2_year_dist:Percent_Change_EMA_5_basic_mean), funs(perc_change = percent_change))
  
  

# join to base data -------------------------------------------------------
  message("     Join new features to original data...")
  pluto_model <- pluto_model %>% 
    left_join(sold_features, by = c("bbl", "Year")) %>% 
    left_join(building_features, by = c("bbl", "Year")) %>% 
    left_join(MA_features, by = c("bbl", "Year"))
  
  radii_feature_end <- Sys.time()

  message("Radii feature creation time: ", round(radii_feature_end-radii_feature_start, 2)
          ," ",units(radii_feature_end-radii_feature_start)
          )
  
  pluto_model
}

