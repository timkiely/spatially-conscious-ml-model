# This function creates the ZIPCODE modeling data

create_zipcode_data <- function(base_model_data = "data/processing steps/p06_base_model_data.rds"
                                , outfile = "data/processing steps/p07_zipcode_model_data.rds") {
  
  message("## Creating Zipcode Modeling Data")
  message("Loading BASE modeling data...")
  pluto_model <- read_rds(base_model_data)
  
  # varibale selection and some feature engineering -------------------------
  # zip code level features:
  message("Executing ZIP feature engineering...")
  
  identity <- function(x) mean(x) # for naming purposes, will be removed
  zip_code_average <- function(x) mean(x, na.rm  = T) # for naming purposes, a simple average
  
  pluto_zip_features <- 
    pluto_model %>% 
    left_join(
      pluto_model %>% 
        select(-Block, -Lot, -Easements, -BoroCode, -NumBldgs, -ProxCode) %>% 
        group_by(ZipCode, Year) %>% 
        summarise(Last_Year_Zip_Sold = sum(Sold, na.rm = T)) %>% 
        mutate(Last_Year_Zip_Sold = lag(Last_Year_Zip_Sold, 1)
               , Last_Year_Zip_Sold_Percent_Ch = (Last_Year_Zip_Sold - lag(Last_Year_Zip_Sold,1))/lag(Last_Year_Zip_Sold,1)) %>%
        left_join(
          pluto_model %>% 
            select(-Block, -Lot, -Easements, -BoroCode, -NumBldgs, -ProxCode) %>% 
            group_by(ZipCode, Year) %>% 
            summarise_at(.vars = vars(Last_Sale_Price:Percent_Change_EMA_5), .funs = funs(identity, zip_code_average)) %>% 
            select(ZipCode, Year,contains("zip_code_average"))
          , by = c("Year", "ZipCode"))
      , by = c("Year", "ZipCode"))
  
  message("     ...Engineering done. Input ", length(pluto_model)," variables and output ", length(pluto_zip_features), " variables")

  message("Writing ZIPCODE modeling data to disk...")
  write_rds(pluto_zip_features, outfile, compress = "gz")
  message("     ...done. ZIPCODE modeling data written to ", outfile)

}