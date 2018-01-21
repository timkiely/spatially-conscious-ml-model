
# This function joins PAD and PLUTO BBL's to the sales data, allowing us to join all sales to PLUTO in the next step

combine_sales_and_pad <- function(sales_infile = "data/processing steps/p03_sales_raw.rds"
                                  , pad_infile = "data/processing steps/p02_pad_raw.rds"
                                  , pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                                  , outfile = "data/processing steps/p04_sales_and_pad.rds") {
  
  
  # Load the data -----------------------------------------------------------
  
  message("Loading and processing PAD data...")
  pad_bbl_expanded <- 
    read_rds(pad_infile) %>% 
    mutate(new_lot = ifelse(is.na(new_lot), as.numeric(lolot), new_lot)) %>% 
    mutate(new_bbl = paste(loboro,as.numeric(loblock),as.numeric(new_lot),sep="_")) %>% 
    mutate(pluto_bbl = paste(as.numeric(billboro), as.numeric(billblock), as.numeric(billlot), sep = "_")) %>% 
    mutate(pluto_bbl = ifelse(is.na(billboro), new_bbl, pluto_bbl))
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
      mutate(bbl = paste(BOROUGH,as.numeric(BLOCK),as.numeric(LOT),sep="_")) %>% 
    mutate(pluto_bbl = bbl)
  message("     ...done")
  
  
  # we split the SALES data into records which can and cannot be mapped to PLUTO
  # for those that cannot be mapped, we modify the pluto_bbl field by brining in PAD data
  # overall mapping error rate drops to about 0.1%
  sales_in_pluto <- semi_join(nyc_sales_raw, pluto_raw, by = c("pluto_bbl"="bbl"))
  sales_not_in_pluto <- anti_join(nyc_sales_raw, pluto_raw, by = c("pluto_bbl"="bbl"))
  first_error <- scales::percent( nrow(sales_not_in_pluto)/nrow(nyc_sales_raw))

  # map PAD to the sales which cannot be mapped to PLUTO
  pad_map_key <- pad_bbl_expanded %>% select(new_bbl, pluto_bbl)
  sales_not_in_pluto <- sales_not_in_pluto %>% select(-pluto_bbl)
  sales_not_in_pluto <- left_join(sales_not_in_pluto, pad_map_key, by = c("bbl"="new_bbl"))
  
  # measure the error rate reduction:
  sales_not_in_pluto_new <- anti_join(sales_not_in_pluto, pluto_raw, by = c("pluto_bbl"="bbl"))
  second_error <- scales::percent( nrow(sales_not_in_pluto_new)/nrow(nyc_sales_raw))
  
  # re-combine the sales data
  sales_new <- bind_rows(sales_in_pluto, sales_not_in_pluto)
  sales_new_not_in_pluto <- anti_join(sales_new, pluto_raw, by = c("pluto_bbl"="bbl"))
  overall_error <- scales::percent( nrow(sales_new_not_in_pluto)/nrow(nyc_sales_raw))
  
  message("Processing done. Overall mapping error rates by steps:")
  message("     ...Sales maping error rate directly to PLUTO: ", first_error)
  message("     ...Sales with PAD BBLs added mapping error rate: ", second_error)
  message("     ...Final overall mapping error rate: ", overall_error)
  
  write_rds(sales_new, outfile)
  message("Done. Sales data combined with PAD. Outfile written to ", outfile)
}







