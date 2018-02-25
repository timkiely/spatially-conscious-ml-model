# This function creates the RADII modeling data

create_radii_data <- function(base_model_data = "data/processing steps/p06_base_model_data.rds"
                              , outfile = "data/processing steps/p08_radii_model_data.rds"
                              , run_radii = FALSE) {
  
  message("## Creating RADII Modeling Data")
  
  # create radii index set --------------------------------------------------
  # we create an index of all PLUTO observations within 500 meters of every other
  # PLUTO observation. We later use this to create radii metrics
  
  if(run_radii==TRUE){
    message("Loading BASE model data...")
    pluto_model <- read_rds(base_model_data)
    
    message("Creating radii comps...")
    source("R/helper/get-spatial-neighbor-points.R")
    radii_time <- Sys.time()
    
    radii_index <- 
      pluto_model %>% 
      filter(Year == max(unique(pluto_model$Year), na.rm = T)) %>% 
      distinct(bbl, .keep_all = T) %>% 
      st_as_sf(coords = c("lon","lat"), na.fail=F, crs = 4326) %>% 
      st_transform(crs = 32618) %>% 
      get_spatial_neighbor_points(id_col = "bbl"
                                  , max_distance = 500
                                  , n_cuts = max(c(1,floor(sqrt(parallel::detectCores())))-1)
                                  , allow_parralell = TRUE
                                  , num_clusters = parallel::detectCores()-2)
    
    radii_time_end <- Sys.time()
    message("     ...done. Total indexing time: ", round(radii_time_end-radii_time), " ",units(radii_time_end-radii_time))
    
    message("Writing radii index to disk...")
    
    write_rds(radii_index, "data/aux data/radii-index.rds", compress = "gz")
    message("     ...done")
    
  } else {
    message("Bypassing radii index calculation, loading from disk...")
    if(!file.exists("data/aux data/radii-index.rds")) stop("file data/aux data/radii-comps.rds missing. Run create_radii_data() at least once to completion first")
    radii_index <- read_rds("data/aux data/radii-index.rds")
  }
  
  
  # create radii features from the index ------------------------------------
  message("Creating radii features...")
  
  # if(run_radii==TRUE){
  #   message("Running RADII feature creation")
  #   source("R/helper/create-radii-features.R")
  #   pluto_radii <- create_radii_features(pluto_model, radii_comps)
  #   message("     ...done")
  #   
  #   
  #   message("Writing radii features to disk...")
  #   write_time <- Sys.time()
  #   write_rds(pluto_radii, "data/aux data/radii-features.rds", compress = "gz")
  #   write_time_end <- Sys.time()
  #   tot_write_time <- write_time_end-write_time
  #   message("     ...done. Writing took ", round(tot_write_time,2),units(tot_write_time))
  #   
  #   
  # } else {
  #   message("Bypassing radii features calculation, loading from disk...")
  #   if(!file.exists("data/aux data/radii-features.rds")) stop("file data/aux data/radii-comps.rds missing. Run create_radii_features() at least once to completion first")
  #   pluto_radii <- read_rds("data/aux data/radii-features.rds")
  # }
  
  # message("     ...Engineering done. Input ", length(pluto_model)," variables and output ", length(pluto_radii), " variables")
  
  message("Writing RADII modeling data to disk...")
  # write_rds(pluto_radii, outfile, compress = "gz")
  message("     ...done. RADII modeling data written to ", outfile)
  
}


