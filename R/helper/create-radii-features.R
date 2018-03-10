create_radii_features <- function(pluto_model, radii_index) {
  
  # pluto_model_bak <- pluto_model
  # pluto_model <- pluto_model %>% filter(Borough=="MN")
  
  pluto <- pluto_model
  radii_data <- radii_index %>% st_set_geometry(NULL)
  radii_data$lon <- st_coordinates(radii_index)[,1]
  radii_data$lat <- st_coordinates(radii_index)[,2]
  
  

# UDFs --------------------------------------------------------------------

  inverted_normalized_distance <- function(x){
    x <- if_else(x!=0, 1/x ,0) # take inverse only if denominator is not zero
    x <- x/sum(x, na.rm = TRUE) # normalize 0 to 1
    x <- if_else(is.nan(x),0,x) # anything with NaN convert to zero
  }
  weighted_mean <- function(x, w) weighted.mean(x, w, na.rm = T)
  radii_mean <- function(x) mean(x, na.rm = T)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  

# create features ---------------------------------------------------------

  nb_weights <- 
    radii_data %>% 
    # sample_frac(.01) %>% 
    # filter(bbl == '1_10_14') %>% 
    select(bbl, lat, lon, Building_Type, BldgArea, Years_Since_Latest, neighbors) %>% 
    unnest() %>% 
    
    # join back to original data to add properties of the neighbors
    left_join(select(radii_data
                     , bbl
                     , "lat_neighbor" = lat
                     , "lon_neighbor" = lon
                     , "Nieghbor_BT" = Building_Type
                     , "Neighbor_SF" = BldgArea
                     , "Neighbor_age" = Years_Since_Latest
                     )
              , by = c('neighbors' = 'bbl')
              ) %>% 
    
    # create distance metrics (using both euclidean spatial distance and absolute difference in Gross Square Footage)
    # COULD USE: WEIGHTS LIST TO INCLUDE BUILDING SIZE, CHARACTERISTICS, AGE, DISTANCE etc
    mutate(Euc_distance = sqrt((lat_neighbor-lat)^2+(lon_neighbor - lon)^2)
           , sf_difference = abs(BldgArea-Neighbor_SF)
           , Age_difference = abs(Years_Since_Latest-Neighbor_age)
           ) %>% 
    
    # Weights only apply to buildings of the same Building_Type, otherwise weight drops to 0
    mutate_at(vars(Euc_distance, sf_difference, Age_difference), funs(ifelse(Building_Type == Nieghbor_BT, ., NA))) %>% 
    
    # Scaled distances (for combining)
    mutate_at(vars(Euc_distance, sf_difference, Age_difference), funs(scaled = scale(., center = FALSE))) %>% 
    
    # geometric average of the the scaled weights
    mutate(combined_distance = (Euc_distance_scaled * sf_difference_scaled * Age_difference_scaled)^(1/3)) %>% 
    
    # inverting and normalizing so that closer observations are more alike
    group_by(bbl) %>% 
    mutate_at(vars(Euc_distance, sf_difference, Age_difference, combined_distance
                  , Euc_distance_scaled, sf_difference_scaled,  Age_difference_scaled)
              , funs(weight = inverted_normalized_distance)) %>% 
    
    # name the weight vectors. 
    mutate(dist_weight = Euc_distance_weight
           , SF_weight = sf_difference_weight
           , age_weight = Age_difference_weight
           , combined_weight = combined_distance_weight
      
           # alternative geometric mean weight. geometric mean of scaled weights
           , geometric_weight = (Euc_distance_scaled_weight*sf_difference_scaled_weight*Age_difference_scaled_weight)^(1/3)
           ) %>% 
    
    ungroup() %>% 
    
    # joining weights back to to the original radii data and creating distance-weighted means
    # several different weight calculations
    select(bbl, neighbors, dist_weight, SF_weight, age_weight, combined_weight, geometric_weight) %>% 
    left_join(select(pluto, Year, bbl, Years_Since_Last_Sale:Percent_Change_EMA_5), by = c('neighbors'='bbl')) %>% 
    group_by(bbl, Year) %>%
    summarise_at(vars(Years_Since_Last_Sale:Percent_Change_EMA_5)
                 , funs(       dist = weighted_mean(., dist_weight)
                        ,      sqft = weighted_mean(., SF_weight)
                        ,       age = weighted_mean(., age_weight)
                        ,  combined = weighted_mean(., combined_weight)
                        ,      geom = weighted_mean(., geometric_weight)
                        ,    simple = radii_mean)
    )
  
  pluto <- pluto %>% left_join(nb_weights, by = c("bbl", "Year")) 
  pluto
}

