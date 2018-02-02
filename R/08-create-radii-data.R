# This function creates the RADII modeling data

create_radii_data <- function(base_model_data = "data/processing steps/p06_base_model_data.rds"
                              , outfile = "data/processing steps/p08_radii_model_data.rds"
                              , run_radii = "N") {
  
  message("Creating RADII Modeling Data")
  message("Loading BASE model data...")
  pluto_model <- read_rds(base_model_data)
  
  
  # create radii index set --------------------------------------------------
  # we create an index of all PLUTO observations within 0.25 miles of every other
  # PLUTO observation. We later use this to create radii metrics
  
  if(run_radii=="run-radii"){
    message("Creating radii comps...")
    source("R/helper/create-radii-comps.R")
    radii_comps <- create_radii_comps(pluto_model)
    message("     ...done")
    
    message("Writing radii comps to disk...")
    write_rds(radii_comps, "data/aux data/radii-comps.rds")
    message("     ...done")
    
  } else {
    message("Bypassing radii comp calculation, loading from disk...")
    radii_comps <- read_rds("data/aux data/radii-comps.rds")
    if(is.na(radii_comps)) stop("Radii comps file missing")
  }
  
  

# create radii features from the index ------------------------------------
  message("Creating radii features...")
  source("R/helper/create-radii-features.R")
  
  pluto_radii <- create_radii_features(pluto_model, radii_comps)
  
  message("     ...Engineering done. Input ", length(pluto_model)," variables and output ", length(pluto_radii), " variables")
  
  message("Writing RADII modeling data to disk...")
  write_rds(pluto_radii, outfile, compress = "gz")
  message("     ...done. RADII modeling data written to ", outfile)
  
}