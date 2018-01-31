

# function to create a standardized base feature creation.

create_base_features <- function(data){
  data <- 
  data %>% 
    group_by(bbl) %>%
    arrange(bbl, Year) %>% 
    
    # creating a running tally for moving average calculations
    mutate(Last_Sale_Price = na.locf(`SALE PRICE`, na.rm = FALSE)
           , Last_Sale_Price_Total = na.locf(TOTAL_SALES, na.rm = FALSE)
           , Last_Sale_Date = na.locf(SALE_DATE, na.rm = FALSE)
           , Years_Since_Last_Sale = Year-lubridate::year(lag(Last_Sale_Date,1))
    ) %>% 
    
    # SMA VARS:
    mutate(SMA_Price_2_year = roll_meanr(lag(Last_Sale_Price,1), n = 2, na.rm = T, fill = NaN)
           , SMA_Price_3_year = roll_meanr(lag(Last_Sale_Price,1), n = 3, na.rm = T, fill = NaN)
           , SMA_Price_5_year = roll_meanr(lag(Last_Sale_Price,1), n = 5, na.rm = T, fill = NaN)
           , SMA_Price_10_year = roll_meanr(lag(Last_Sale_Price,1), n = 10, na.rm = T, fill = NaN)
           , SMA_Price_2_year_total = roll_meanr(lag(Last_Sale_Price_Total,1), n = 2, na.rm = T, fill = NaN)
           , SMA_Price_3_year_total = roll_meanr(lag(Last_Sale_Price_Total,1), n = 3, na.rm = T, fill = NaN)
           , SMA_Price_5_year_total = roll_meanr(lag(Last_Sale_Price_Total,1), n = 5, na.rm = T, fill = NaN)
           , SMA_Price_10_year_total = roll_meanr(lag(Last_Sale_Price_Total,1), n = 10, na.rm = T, fill = NaN)
    ) %>% 
    mutate(Percent_Change_SMA_2 = (SMA_Price_2_year-lag(SMA_Price_2_year,1))/lag(SMA_Price_2_year,1)
           ,Percent_Change_SMA_5 = (SMA_Price_5_year-lag(SMA_Price_5_year,1))/lag(SMA_Price_5_year,1)) %>% 
    
    # EMA VARS
    mutate(EMA_Price_2_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 2, na.rm = T, fill = NaN)
           , EMA_Price_3_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 3, na.rm = T, fill = NaN)
           , EMA_Price_5_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 5, na.rm = T, fill = NaN)
           , EMA_Price_10_year = exp_roll_meanr(lag(Last_Sale_Price,1), n = 10, na.rm = T, fill = NaN)
           , EMA_Price_2_year_total = exp_roll_meanr(lag(Last_Sale_Price_Total,1), n = 2, na.rm = T, fill = NaN)
           , EMA_Price_3_year_total = exp_roll_meanr(lag(Last_Sale_Price_Total,1), n = 3, na.rm = T, fill = NaN)
           , EMA_Price_5_year_total = exp_roll_meanr(lag(Last_Sale_Price_Total,1), n = 5, na.rm = T, fill = NaN)
           , EMA_Price_10_year_total = exp_roll_meanr(lag(Last_Sale_Price_Total,1), n = 10, na.rm = T, fill = NaN)
    ) %>% 
    mutate(Percent_Change_EMA_2 = (EMA_Price_2_year-lag(EMA_Price_2_year,1))/lag(EMA_Price_2_year,1)
           ,Percent_Change_EMA_5 = (EMA_Price_5_year-lag(EMA_Price_5_year,1))/lag(EMA_Price_5_year,1)
    )
  return(data)
}