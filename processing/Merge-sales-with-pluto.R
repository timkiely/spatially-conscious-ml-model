
rm(list = ls()[!ls()%in%c("pad_bbl_expanded","pluto_lean")])

library(tidyverse)
# This script takes NYC's 2017 Property Address Directory (PAD) file and
# maps each address to PLUTO.


lookat <- function(boro= 1,blck = 829,lt = 16,data = pluto_lean) data %>% filter(BOROUGH == boro, Block == as.numeric(blck), Lot == as.numeric(lt)) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16,data = pluto_lean) data %>% filter(BOROUGH == boro, Block == blck, Lot == lt)


# pad_bbl_expanded was created by adding a unique
# row for every row in PAD that had a range of lots for
# a single BBL. These were mostly condos

# Load the data -----------------------------------------------------------
if(!exists("pad_bbl_expanded")){
  pad_bbl_expanded <- 
    read_rds("data/PAD_Condo_BBLs_expanded_v002.rds") %>% 
    mutate(billing_bbl = paste(billboro,as.numeric(billblock),as.numeric(billlot),sep="_"))
}

if(!exists("pluto_lean")){
  pluto_lean <- 
    read_rds("data/pluto_lean.rds") %>% 
    mutate(bbl = paste(BOROUGH,as.numeric(Block),as.numeric(Lot),sep="_"))
}

if(!exists("nyc_sales_clean_1")){
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
    mutate(BOROUGH = as.integer(BOROUGH)) %>% 
    mutate(bbl = paste(BOROUGH,as.numeric(BLOCK),as.numeric(LOT),sep="_"))
}


# map lat and lon to sales transactions ----------------------------------------------------

# all bbls in pluto with lat/lon coordinates attached
pluto_distinct <- 
  pluto_lean %>% 
  filter(Year%in%c(2016)) %>% 
  distinct(bbl,lat,lon, .keep_all = T) %>% 
  mutate(PLUTO_DISTINCT_FLAG = 1)


# non-condos can be mapped to pluto with "new bbl"
pad_with_latlon_1 <- 
  left_join(pad_bbl_expanded
            , pluto_distinct
            , by = c("new_bbl"="bbl")) %>% 
  filter(!is.na(PLUTO_DISTINCT_FLAG))

# condos can be mapped to pluto with "billing bbl"
pad_with_latlon_2 <- 
  left_join(pad_bbl_expanded
            , pluto_distinct
            , by = c("billing_bbl"="bbl"))%>% 
  filter(!is.na(PLUTO_DISTINCT_FLAG))

# combined the mapped files
pad_with_latlon <- bind_rows(pad_with_latlon_1,pad_with_latlon_2)

# join sales to location data
sales_with_location <-
  left_join(nyc_sales_clean_1
            , pad_with_latlon
            , by = c("bbl"="new_bbl")
  )

# duplicates? close to a rounding error
nrow(sales_with_location)/nrow(nyc_sales_clean_1)

# how many sales mapped unsuccessfully?
scales::percent(nrow(filter(sales_with_location,is.na(PLUTO_DISTINCT_FLAG)))/nrow(sales_with_location))

sales_with_location_write <- 
  sales_with_location %>% 
  select(-BOROUGH.y, -PLUTO_DISTINCT_FLAG
         ,-loboro,-loblock,-lolot,-lobblscc,-hiboro,-hiblock,-hilot,-hibblscc
         ,-row_Id, -contains("new_"), -contains("billing_")
         ) %>% 
  rename("BOROUGH" = BOROUGH.x)


write_rds(sales_with_location_write, 'data/sales_with_pluto.rds', compress = 'gz')




