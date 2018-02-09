

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
  
  unique_bbls <- 
    pluto_model %>% 
    distinct(bbl, round_lat, round_lon, Borough) %>% 
    filter(!is.na(round_lat)) %>% 
    distinct(bbl, .keep_all = TRUE)
  
  unique_bbls_sf <- 
    unique_bbls %>% 
    st_as_sf(coords = c('round_lon','round_lat')
             , na.fail = FALSE
             , remove = FALSE
             , crs = 4326) %>% 
    st_transform(32618)
  
  

  ## TESTING ALTERNATIVE APPROACH FOR FASTER INDEXING:
  # Baseline: Total indexing time: 3.25hours on aws w/ MN, BK
  # MK+BK 1.39hours using dnearneigh technique. Could split up by boro to improve speed?
  coords <- st_coordinates(unique_bbls_sf)
  nbb_time <- Sys.time()
  nnb <- dnearneigh(coords, 0, 500)
  end_nbb_time <- Sys.time()
  nbb_tot <- end_nbb_time-nbb_time
  message("Total nbb time: ",round(nbb_tot,2),units(nbb_tot))
  neighbors_list <- nb2listw(nnb)
  
  message("     ...Finished RADII indexing at ", Sys.time())
  message("     ...Total indexing time: ", round(nbb_tot,2),units(nbb_tot))
  neighbors_list
}

