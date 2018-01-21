
library(tidyverse)
library(sf)
source("R/helper-functions.R")

if(!exists("comp_ids")){
  comp_ids <- read_rds("data/comp_ids_full_v_0.01.rds")
  # comp_ids <- read_rds("data/comp_ids_full_v1.0.rds")
}

if(!exists("sale_augmented")){
  sale_augmented <- tbl_df(read_rds("data/sales_augmented.rds")) %>% mutate(UID = 1:nrow(.)) 
}

# haversine distance
haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}

comps_of_interest <- comp_ids[[1]]

address <- as.numeric(distinct(comps_of_interest,Address))
address_info <- sale_augmented %>% filter(UID%in%address)



address_info %>% 
  st_as_sf(coords = c("lon","lat")) %>% 
  st_geometry() %>% 
  plot()


# Multivariate exponential weighted moving average taking into account: Age, Distance and Square Footage 
comp_set_metrics <- 
  sale_augmented %>% 
  filter(UID%in%comps_of_interest$UID) %>% 
  filter(SALE_DATE < address_info$SALE_DATE) %>% 
  filter(!UID%in%address) %>% 
  PROCESS_SALES_DATA() %>% 
  mutate(abs_distance = haversine(address_info$lat, address_info$lon, lat, lon)
         , abs_distance_w = 2/(abs_distance+1)
         , Age = lubridate::year((Sys.Date())) - SALE_YEAR
         , Age_w = 2/(Age+1)
         , SF_diff = abs(address_info$GROSS.SQUARE.FEET - GROSS.SQUARE.FEET)
         , SF_diff_w = 2/(SF_diff+1)
         ) %>% 
  rowwise() %>% mutate(WEIGHT = prod(abs_distance_w, Age_w, SF_diff_w, na.rm = T)) %>% ungroup() %>% 
  select(-abs_distance, -abs_distance_w, -Age, -Age_w, -SF_diff, -SF_diff_w) %>% 
  summarise(WEIGHTED_SALE_PRICE = weighted.mean(SALE.PRICE, WEIGHT, na.rm = TRUE)
            , MEAN_SALE_PRICE = mean(SALE.PRICE, na.rm = T)
            , Count_of_comps = n())
  
  



return_most_common_val <- function(vrs) {
  vrs %>% 
    as_data_frame() %>% 
    count(value) %>% 
    slice(which.max(n)) %>% 
    select(-n) %>% 
    as.character()
  }


comp_set


if(nrow(comp_set)<1) stop("no comps available")
comp_chars <- comp_set %>% summarise_if(.predicate = is.character, .funs = return_most_common_val)
comp_nums <- comp_set %>% summarise_if(.predicate = is.numeric, .funs = funs(mean, median))

Average_year_built = mean(YEAR.BUILT, na.rm = T)
    , 
  )



