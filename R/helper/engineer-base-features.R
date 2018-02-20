

# function to create a standardized base feature creation.

engineer_base_features <- function(pluto_with_sales){
  
  pluto_with_sales <- 
    pluto_with_sales %>% 
    group_by(bbl) %>%
    arrange(bbl, Year) %>% 
    
    # building class variables
    mutate(
      has_building_area = ifelse(BldgArea>0,1,0)
      , BldgArea2 = ifelse(BldgArea==0,1,BldgArea)
      , Percent_Com = ComArea/(BldgArea2)
      , Percent_Res = ResArea/(BldgArea2)
      , Percent_Office = OfficeArea/(BldgArea2)
      , Percent_Retail = RetailArea/(BldgArea2)
      , Percent_Garage = GarageArea/(BldgArea2)
      , Percent_Storage = StrgeArea/(BldgArea2)
      , Percent_Factory = FactryArea/(BldgArea2)
      , Percent_Other = OtherArea/(BldgArea2)
    ) %>%
    select(-BldgArea2) %>%
    
    # creating a running tally for moving average calculations
    mutate(Last_Sale_Price = na.locf(lag(`SALE PRICE`,1), na.rm = FALSE)
           , Last_Sale_Price_Total = na.locf(lag(TOTAL_SALES,1), na.rm = FALSE)
           , SALE_YEAR = if_else(is.na(`SALE PRICE`), NA_real_, SALE_YEAR) 
           , SALE_DATE = if_else(is.na(`SALE PRICE`), as.Date(NA), SALE_DATE)
           , Last_Sale_Date = na.locf(lag(SALE_DATE, 1), na.rm = FALSE)
           , Years_Since_Last_Sale = Year-lubridate::year(Last_Sale_Date)
    ) %>% 
    
    # SMA VARS:
    mutate(SMA_Price_2_year = roll_meanr(lag(Last_Sale_Price,1), n = 2, na.rm = T, fill = NaN)
           , SMA_Price_3_year = roll_meanr(lag(Last_Sale_Price,1), n = 3, na.rm = T, fill = NaN)
           , SMA_Price_5_year = roll_meanr(lag(Last_Sale_Price,1), n = 5, na.rm = T, fill = NaN)
    ) %>% 
    mutate(Percent_Change_SMA_2 = (SMA_Price_2_year-lag(SMA_Price_2_year,1))/lag(SMA_Price_2_year,1)
           ,Percent_Change_SMA_5 = (SMA_Price_5_year-lag(SMA_Price_5_year,1))/lag(SMA_Price_5_year,1)) %>% 
    
    # EMA VARS
    mutate(EMA_Price_2_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 2, na.rm = T, fill = NaN)
           , EMA_Price_3_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 3, na.rm = T, fill = NaN)
           , EMA_Price_5_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 5, na.rm = T, fill = NaN)
    ) %>% 
    mutate(Percent_Change_EMA_2 = (EMA_Price_2_year-lag(EMA_Price_2_year,1))/lag(EMA_Price_2_year,1)
           ,Percent_Change_EMA_5 = (EMA_Price_5_year-lag(EMA_Price_5_year,1))/lag(EMA_Price_5_year,1)
    )
  return(pluto_with_sales)
}