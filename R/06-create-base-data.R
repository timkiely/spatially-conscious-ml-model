# This function creates the BASE modeling data

create_base_data <- function(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                             , outfile = "data/processing steps/p06_base_model_data.rds"
                             , limit_boros = FALSE) {
  
  message("## Creating Base Modeling Data")
  message("Loading PLUTO...")
  pluto <- read_rds(pluto_with_sales_infile)
  
  # for dev purposes, if TRUE, filter the data for only specified boros
  if(limit_boros==TRUE){
    
    boro_lim <- c("MN") ### <- change this if you want to include more borough in the data
    
    message("Since limit_boros=TRUE, filtering data for the following boroughs only: ", paste(boro_lim, collapse = ","))
    pluto <- pluto %>% filter(Borough%in%boro_lim)
  }
  
# varibale seelction and some feature engineering -------------------------

  message("Partitioning PLUTO...")
  
  pluto_with_sales <- 
    pluto %>% 
    # filters out bbls that have no recorded sale in the dataset:
    group_by(bbl) %>% mutate(sold_sum = sum(Sold, na.rm = T)) %>% 
    ungroup() %>% filter(sold_sum>0) %>% select(-sold_sum)
  
  pluto_without_sales <- 
    pluto %>% 
    group_by(bbl) %>% mutate(sold_sum = sum(Sold, na.rm = T)) %>% 
    ungroup() %>% filter(sold_sum==0) %>% select(-sold_sum)
  
  message("Executing base feature engineering...")
  source("R/helper/engineer-base-features.R")
  
  pluto_model <- 
    pluto_with_sales %>% 
    engineer_base_features() %>% 
    ungroup()
  
  message("     ...done. Input ", length(pluto_with_sales)," variables and output ", length(pluto_model), " variables")
  
  
  message("Re-combining PLUTO...")
  final_data <- bind_rows(pluto_model, pluto_without_sales) %>% ungroup()
  
  message("Writing base modeling data to disk...")
  write_rds(final_data, outfile, compress = "gz")
  message("     ...done. Base modeling data written to ", outfile)
  
}