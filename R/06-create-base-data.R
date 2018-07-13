# This function creates the BASE modeling data

create_base_data <- function(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                             , outfile = "data/processing steps/p06_base_model_data.rds"
                             , limit_boros = FALSE) {
  
  message("## Creating Base Modeling Data")
  message("Loading PLUTO...")
  pluto <- read_rds(pluto_with_sales_infile)
  
  # If TRUE, filter the data for only specified boros. Dramatically speeds up modeling
  if(limit_boros==TRUE){
    
    boro_lim <- c("MN") ### <- change this if you want to include more boroughs in the sample data
    
    message("Since limit_boros=TRUE, filtering data for the following boroughs only: ", paste(boro_lim, collapse = ","))
    pluto <- pluto %>% filter(Borough %in% boro_lim)
  }
  
  # add age features to the data
  pluto <- 
    pluto %>% 
    mutate(YearBuilt = if_else(YearBuilt==0, as.integer(NA), YearBuilt)
           , Building_Age = Year-YearBuilt
           , Years_Since_Latest = Year - pmax(YearBuilt, YearAlter1, YearAlter2, na.rm = TRUE)
           , Has_alteration = if_else(YearAlter1+YearAlter2>0, 1, 0)
    )
  
  
  #  data cleansing -----------------------------------------------
  message("Applying data cleansing...")
  source("R/helper/cleanse-base-data.R")
  pluto_cleansed <- cleanse_base_data(pluto)

  
  # varibale selection and some feature engineering -------------------------
  message("Partitioning PLUTO...")
  
  # For features based on sale data, only use bbls that have recorded sales
  pluto_with_sales <- 
    pluto_cleansed %>% 
    group_by(bbl) %>% mutate(sold_sum = sum(Sold, na.rm = T)) %>% 
    ungroup() %>% filter(sold_sum>0) %>% select(-sold_sum)
  
  pluto_without_sales <- 
    pluto_cleansed %>% 
    group_by(bbl) %>% mutate(sold_sum = sum(Sold, na.rm = T)) %>% 
    ungroup() %>% filter(sold_sum==0) %>% select(-sold_sum)
  
  message("Executing base feature engineering...")
  source("R/helper/engineer-base-features.R")
  
  pluto_model <- 
    pluto_with_sales %>% 
    engineer_base_features() %>% 
    ungroup()
  
  message("     ...done. Input ", length(pluto_with_sales)," variables and output ", length(pluto_model), " variables.  Total rows: ", scales::comma(nrow(pluto_model)+nrow(pluto_without_sales)))
  
  
  message("Re-combining PLUTO...")
  final_data <- bind_rows(pluto_model, pluto_without_sales) %>% ungroup()
  
  message("Writing base modeling data to disk...")
  write_rds(final_data, outfile, compress = "gz")
  message("     ...done. Base modeling data written to ", outfile)
  
}