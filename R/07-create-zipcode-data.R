# This function creates the ZIPCODE modeling data

create_zipcode_data <- function(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                                , outfile = "data/processing steps/p07_zipcode_model_data.rds") {
  
  message("Creating Zipcode Modeling Data")
  message("Loading PLUTO...")
  pluto <- read_rds(pluto_with_sales_infile)
  
  # varibale selection and some feature engineering -------------------------
  
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
  # base features:
  pluto_model <- 
    pluto_with_sales %>% 
    create_base_features() %>% 
    ungroup()
  message("     ...done.")
  
  
  # zip code level features:
  message("Executing ZIP feature engineering...")
  identity <- function(x) mean(x)
  zip_code_average <- function(x) mean(x, na.rm  = T)
  
  pluto_model <- 
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
  
  message("     ...done.")
  
  message("     ...Engineering done. Input ", length(pluto_with_sales)," variables and output ", length(pluto_model), " variables")

  message("Re-combining PLUTO...")
  final_data <- bind_rows(pluto_model, pluto_without_sales) %>% ungroup()
  
  message("Writing base modeling data to disk...")
  write_rds(final_data, outfile, compress = "gz")
  message("     ...done. ZIPCODE modeling data written to ", outfile)

}