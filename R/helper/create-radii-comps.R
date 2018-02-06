

create_radii_comps <- function(pluto_model) {
  
  # lat/lons differ slightly from one year to next. Round lat/lons to compensate
  
  pluto_model <- 
    pluto_model %>% 
    mutate(round_lat = round(lat, 3)
           , round_lon = round(lon, 3)
           )
  
  # pluto_model_bak <- pluto_model
  # pluto_model <- pluto_model_bak
  
  # NOTE: round lat/ lon not implemented, unique bbl couts are innacurate here
  # 1,000 rows = 333 uniqe bbls = 4.39secs
  # 10,000 rows = 2,905 uniqe bbls = 21.79secs
  # 100,000 rows = 28,841 unique bbls = 8.38mins
  # 1m rows = 284,542 unique bbls = 10.6hours
  #
  # All rows 8.1MM = 2,536,241 unique bbls = XXmins?
  pluto_model <- pluto_model %>% filter(Borough=="MN")
  unique_bbls <- distinct(pluto_model, bbl, round_lat, round_lon) %>% filter(!is.na(lat))
  
  
  unique_bbls_sf <- 
    unique_bbls %>% 
    st_as_sf(coords = c('round_lon','round_lat')
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
  on.exit(closeAllConnections())
  on.exit(stopImplicitCluster())
  
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
  
  end_time <- Sys.time()
  tot_time <- end_time-run_start
  
  message("     ...Finished RADII indexing at ", Sys.time())
  message("     ...Total indexing time: ", round(tot_time,2),units(tot_time))
  Comps_uids
}

