

create_radii_comps <- function(pluto_model) {
  
  unique_bbls <- distinct(pluto_model, bbl, lat, lon) %>% filter(!is.na(lat))
  
  unique_bbls_sf <- 
    unique_bbls %>% 
    st_as_sf(coords = c('lon','lat')
             , na.fail = FALSE
             , remove = FALSE
             , crs = 4326) %>% 
    st_transform(32618)
  
  pb <- progress::progress_bar$new(total = nrow(unique_bbls_sf))
  opts <- list(progress = function(n) pb$tick(token = list("current" = n)))
  
  message("Starting radii indexing at ", Sys.time())
  run_start <- Sys.time()
  
  num_cores <- parallel::detectCores()-2
  cl <- makeSOCKcluster(num_cores)
  registerDoSNOW(cl)
  
  Comps_uids <- foreach(ii = 1:nrow(unique_bbls_sf)
                        , .verbose = FALSE
                        , .errorhandling = "pass"
                        , .options.snow = opts ) %dopar% {
                          
                          pacman::p_load(sf, dplyr, magrittr)
                          l = 500 # 500 meters
                          row_interest <- filter(unique_bbls_sf, row_number()==ii)
                          buffer <- row_interest %>% st_buffer(dist = l)
                          
                          comps_idx <- suppressMessages(st_intersects(buffer, unique_bbls_sf))[[1]]
                          comps <- unique_bbls_sf %>% filter(row_number() %in% comps_idx)
                          comps <- comps %>% st_set_geometry(NULL)
                          
                          if(nrow(comps)>0) {
                            comps$Origin_Key <- row_interest$bbl
                          } else {
                            comps <- data_frame("lat" = NA_integer_,"lon" = NA_integer_, "bbl" = row_interest$bbl)
                            comps$Origin_Key <- row_interest$bbl
                          }
                          
                          
                          pb$tick()
                          return(comps)
                        }; closeAllConnections();stopImplicitCluster()
  
  
  message("     ...Finished RADII indexing at ", Sys.time())
  Comps_uids
}

