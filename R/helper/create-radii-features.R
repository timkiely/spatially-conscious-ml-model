create_radii_features <- function(pluto_model, radii_comps) {
  # pluto_model_bak <- pluto_model
  # pluto_model <- head(pluto_model,10000)
  pluto_model
  radii_comps
  
  pluto_model
  
  
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
  
  message("FUNCTION NOT DONE")
  return(NA)
}