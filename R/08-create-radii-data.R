# This function creates the RADII modeling data

create_radii_data <- function(base_model_data = "data/processing steps/p06_base_model_data.rds"
                              , outfile = "data/processing steps/p08_radii_model_data.rds"
                              , run_radii = FALSE) {
  
  message("Creating RADII Modeling Data")
  
  
  # create radii index set --------------------------------------------------
  # we create an index of all PLUTO observations within 0.25 miles of every other
  # PLUTO observation. We later use this to create radii metrics
  
  if(run_radii==TRUE){
    message("Loading BASE model data...")
    pluto_model <- read_rds(base_model_data)
    message("Creating radii comps...")
    source("R/helper/create-radii-comps.R")
    radii_comps <- create_radii_comps(pluto_model)
    message("     ...done")
    
    message("Writing radii index to disk...")
    write_rds(radii_comps, "data/aux data/radii-index.rds")
    message("     ...done")
    
  } else {
    message("Bypassing radii index calculation, loading from disk...")
    if(!file.exists("data/aux data/radii-index.rds")) stop("file data/aux data/radii-comps.rds missing. Run create_radii_data() at least once to completion first")
    radii_comps <- read_rds("data/aux data/radii-index.rds")
  }
  
  
# create radii features from the index ------------------------------------
  message("Creating radii features...")
  
  if(run_radii==TRUE){
    message("Running RADII feature creation")
    source("R/helper/create-radii-features.R")
    pluto_radii <- create_radii_features(pluto_model, radii_comps)
    message("     ...done")
    
    
    message("Writing radii features to disk...")
    write_rds(pluto_radii, "data/aux data/radii-features.rds")
    message("     ...done")
    
    
  } else {
    message("Bypassing radii features calculation, loading from disk...")
    if(!file.exists("data/aux data/radii-features.rds")) stop("file data/aux data/radii-comps.rds missing. Run create_radii_features() at least once to completion first")
    pluto_radii <- read_rds("data/aux data/radii-features.rds")
  }
  
  message("     ...Engineering done. Input ", length(pluto_model)," variables and output ", length(pluto_radii), " variables")
  
  message("Writing RADII modeling data to disk...")
  write_rds(pluto_radii, outfile, compress = "gz")
  message("     ...done. RADII modeling data written to ", outfile)
  
}


