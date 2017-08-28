

rm(list=ls())
library(tidyverse)
library(lubridate)


# function to quickly glimpse BBL
lookat <- function(boro= 1,blck = 829,lt = 16,data = nyc_sales_clean_1) data %>% filter(BOROUGH == boro, BLOCK == as.numeric(blck), LOT == as.numeric(lt)) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16,data = nyc_sales_clean_1) data %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt)



nyc_sales_raw <- read_rds("data/nyc-sales-data-raw.rds")
nyc_sales_raw <- nyc_sales_raw %>% mutate_if(.predicate = is.factor,.funs = as.character)

nyc_sales_clean_1 <- 
  nyc_sales_raw %>% 
  mutate_if(.predicate = is.character, .funs = trimws) %>% 
  mutate(SALE.DATE1 = ymd(SALE.DATE, quiet = T)
         ,SALE.DATE2 = mdy(SALE.DATE, quiet = T)
         ,SALE_DATE = SALE.DATE1
         ,SALE_DATE = if_else(is.na(SALE_DATE),SALE.DATE2,SALE_DATE)
         ,SALE_YEAR = lubridate::year(SALE_DATE)
         ) %>% 
  select(-SALE.DATE,-SALE.DATE1,-SALE.DATE2,-SALE.YEAR) %>% 
  mutate(BOROUGH = as.integer(BOROUGH))

nyc_sales_clean_1$SALE_YEAR %>% table()


# WHY ARE THERE SO MANY TRANSACTIONS ON THE SAME DAY FOR 1 BBL???
# ANSWER: THese tansactions are "Timeshare Deeds" related to hotels. 
# Solution: map in ACRIS data and filter out "timeshare deeds"
nyc_sales_clean_1 %>% 
  #filter(!BUILDING.CLASS.AT.PRESENT%in%c('H3')) %>% 
  group_by_at(vars(BOROUGH:BUILDING.CLASS.AT.TIME.OF.SALE,SALE_DATE,SALE_YEAR)) %>%
  summarise(count = n()
            ,top_class = head(BUILDING.CLASS.AT.PRESENT,1)) %>% 
  ungroup() %>% 
  select(BOROUGH,BLOCK,LOT,count,top_class) %>% 
  arrange(-count) 

# here's a hotel with almost 300 transactions on the same day in 2016:

# a hotel with timeshare deeds:
lookat(1,1009,37)

# condo hotels
lookat(1,1006,1303)






# Combine sales data with PLUTO -------------------------------------------
pluto_lean <- read_rds("data/pluto_lean.rds")



# Note: Only about half of BBL's map correctly. Majority of non-maps are condos
sale_augmented <-
  left_join(nyc_sales_clean_1,pluto_lean, by = c(
  "BOROUGH"="BOROUGH"
  ,"BLOCK"="Block"
  ,"LOT"="Lot"
  ,"SALE_YEAR" = "Year"
))

sale_augmented$Address %>% is.na() %>% table()


acris <- read_csv("/Users/timkiely/Downloads/ACRIS_-_Real_Property_Master.csv")

sale_augmented2 <-
  left_join(sale_augmented,acris, by = c(
    "BOROUGH"="BOROUGH"
    ,"BLOCK"="Block"
    ,"LOT"="Lot"
    ,"SALE_YEAR" = "Year"
  ))

sale_augmented$Address %>% is.na() %>% table()






write_rds(sale_augmented, "data/sales_augmented.rds")




