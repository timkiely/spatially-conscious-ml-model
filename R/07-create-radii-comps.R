

library(tidyverse)
library(sf)

source("R/00aa-load-packages.R")
source("R/tune-model-objects.R")

if(!exists("sale_augmented")){
  sale_augmented <- tbl_df(read_rds("data/sales_augmented.rds")) %>% mutate(UID = 1:nrow(.))
}

set.seed(1987)
sales <- sale_augmented %>% filter(!is.na(lat)) %>% filter(lon>-76) #%>% sample_frac(0.01)
sales_sf <- st_as_sf(sales, coords = c("lon","lat"), remove = FALSE, crs = 4326, na.fail = FALSE) 


pb <- progress::progress_bar$new(total = nrow(sales_sf))
sales_sf_index <- sales_sf %>% select(UID, geometry) %>% st_transform(crs = 32618)
sales_index <- sales %>% select(UID)
out_frame <- data_frame()
opts <- list(progress = function(n) pb$tick(token = list("current" = n,"what" = ii)))


message("Starting at ", Sys.time())
run_start <- Sys.time()



num_cores <- parallel::detectCores()-2
cl <- makeSOCKcluster(num_cores)
registerDoSNOW(cl)
Comps_uids <- foreach(ii = 1:nrow(sales_sf)
                          , .verbose = FALSE
                          , .errorhandling = "pass"
                          , .options.snow = opts ) %dopar% {
                            
                            source("R/00aa-load-packages.R")
                            
                            row_interest <- sales_sf_index %>% filter(row_number()==ii)
                            buffer <- row_interest %>% st_buffer(dist = 804.672)
                            
                            comps_idx <- suppressMessages(st_intersects(buffer, sales_sf_index))[[1]]
                            comps <- sales_index %>% filter(row_number()%in%comps_idx)
                            
                            if(nrow(comps)>0) {
                              comps$Address <- row_interest$UID
                            } else {
                              comps <- data_frame("UID" = NA_integer_, "Address" = row_interest$UID)
                              comps$UID <- as.integer(comps$UID)
                            }
                            
                            
                            pb$tick()
                            return(comps)
                          }; closeAllConnections();stopImplicitCluster()

run_end <- Sys.time()



boros <- read_sf("data/nyc_boroughs_shapefile.geojson.txt") %>% st_transform(crs = 32618)
plot(boros$geometry)
plot(sales_sf$geometry, add = F, col = "blue")
plot(row_interest$geometry, add = T, col = "green", size = 10)
plot(buffer, add = TRUE, col = "red")

library(leaflet)
sales_sf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = boros %>% st_transform(crs = 4326)) %>% 
  addCircles(color = "green") %>% 
  addPolygons(data = buffer %>% st_transform(crs = 4326), color = "red") %>% 
  addCircleMarkers(data = comps %>% st_transform(crs = 4326), color = "orange")

