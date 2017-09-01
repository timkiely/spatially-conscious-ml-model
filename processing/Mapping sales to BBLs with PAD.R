

# Demonstrates how to link NYC Rolling Sales Data, PLUTO and PAD
# Using this methodology, one should be able to geo-locate all sales in 
# NYC, including Condo sales

library(tidyverse)

# sales data --------------------------------------------------------------
nyc_sales_raw <- read_rds('data/nyc-sales-data-raw.rds') %>% mutate_if(.predicate = is.factor,.funs = as.character)

nyc_sales_clean_1 <- 
  nyc_sales_raw %>% 
  mutate_if(.predicate = is.character, .funs = trimws) %>% 
  mutate(SALE.DATE1 =  lubridate::ymd(SALE.DATE, quiet = T)
         ,SALE.DATE2 =  lubridate::mdy(SALE.DATE, quiet = T)
         ,SALE_DATE = SALE.DATE1
         ,SALE_DATE = if_else(is.na(SALE_DATE),SALE.DATE2,SALE_DATE)
         ,SALE_YEAR = lubridate::year(SALE_DATE)
  ) %>% 
  select(-SALE.DATE,-SALE.DATE1,-SALE.DATE2,-SALE.YEAR) %>% 
  mutate(BOROUGH = as.integer(BOROUGH))



# combine with PLUTO ------------------------------------------------------

pluto_lean <- read_rds("data/pluto_lean.rds")

# Note: Only about half of BBL's map correctly. Majority of non-maps are condos
sales_not_in_pluto <-
  anti_join(nyc_sales_clean_1,pluto_lean
            , by = c(
              "BOROUGH" = "BOROUGH"
              , "BLOCK" = "Block"
              , "LOT" = "Lot" 
              , "SALE_YEAR" = "Year"
            )
  )



# combine with PAD --------------------------------------------------------

# in order to map the majority of the remainder, we have to look to the PAD file
pad_bbl_expanded <- read_rds("data/PAD_Condo_BBLs_expanded_v002.rds")


sales_not_in_pluto_or_pad <-
  anti_join(sales_not_in_pluto %>% mutate(bbl = paste(BOROUGH,as.numeric(BLOCK),as.numeric(LOT),sep="_"))
            ,pad_bbl_expanded 
            , by = c("bbl"="new_bbl"))



# the remaining non-mappable records (less than 1% of sales) could be due to the fact that we are using the 2017 version of PAD only
message("All sales in database: ",scales::comma(nrow(nyc_sales_clean_1)))
message("Sales that cannot be mapped to PLUTO BBL: ",scales::comma(nrow(sales_not_in_pluto)))
message("Sales that cannot be mapped to PLUTO or PAD: ",scales::comma(nrow(sales_not_in_pluto_or_pad)))
message(scales::comma(nrow(sales_not_in_pluto_or_pad))
        ," out of "
        , scales::comma(nrow(nyc_sales_clean_1))
        ," or "
        ,scales::percent(nrow(sales_not_in_pluto_or_pad)/nrow(nyc_sales_clean_1))
        , " of records could not be matched")


