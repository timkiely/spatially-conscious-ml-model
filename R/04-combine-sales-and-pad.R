
# This function joins PAD and PLUTO BBL's to the sales data, allowing us to join all sales to PLUTO in the next step

combine_sales_and_pad <- function(sales_infile = "data/processing steps/p03_sales_raw.rds"
                                  , pad_infile = "data/processing steps/p02_pad_raw.rds"
                                  , pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                                  , outfile = "data/processing steps/p04_sales_and_pad.rds") {
  
  
  # Load the data -----------------------------------------------------------
  
  message("Loading and processing PAD data...")
  pad_bbl_expanded <- 
      read_rds(pad_infile) %>% 
      mutate(billing_bbl = paste(billboro,as.numeric(billblock),as.numeric(billlot),sep="_")
             , new_bbl = paste(loboro,as.numeric(loblock),as.numeric(new_lot),sep="_"))
  message("     ...done")
  
  message("Loading and processing PLUTO data...")
  pluto_raw <- read_rds(pluto_infile) %>% 
    mutate(bbl = paste(BoroCode,as.numeric(Block),as.numeric(Lot),sep="_"))
  message("     ...done")
  
  message("Loading and processing SALES data...")
  nyc_sales_raw <- read_rds(sales_infile) %>% 
    mutate_if(.predicate = is.factor,.funs = as.character) %>% 
      mutate_if(.predicate = is.character, .funs = trimws) %>% 
      mutate(SALE.DATE1 =  lubridate::ymd(`SALE DATE`, quiet = T)
             , SALE.DATE2 =  lubridate::mdy(`SALE DATE`, quiet = T)
             , SALE_DATE = SALE.DATE1
             , SALE_DATE = if_else(is.na(SALE_DATE),SALE.DATE2,SALE_DATE)
             , SALE_YEAR = lubridate::year(SALE_DATE)
             ) %>% 
      select(-`SALE DATE`,-SALE.DATE1,-SALE.DATE2) %>% 
      mutate(BOROUGH = as.integer(BOROUGH)) %>% 
      mutate(bbl = paste(BOROUGH,as.numeric(BLOCK),as.numeric(LOT),sep="_"))
  message("     ...done")
  
  
  

# Create exhaustive BBL lists ---------------------------------------------

  pluto_distinct <- 
    pluto_raw %>% 
    filter(Year%in%c(2017)) %>% 
    distinct(bbl,lat,lon, .keep_all = F) %>% 
    mutate(PLUTO_DISTINCT_FLAG = 1)

  #=========================>>> LEFT OFF
  # TODO: new_lot in the PAD file is all NA, making a mtach impossible
    
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
  
  pad_with_latlon_1
  pad_with_latlon_2
  
  
    message("TODO: function to combine SALES and PAD data")
}
