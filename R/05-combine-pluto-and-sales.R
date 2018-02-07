
# This function combines PLUTO with the sales&pad data

combine_pluto_with_sales <- function(pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                                     , sales_pad_infile = "data/processing steps/p04_sales_and_pad.rds"
                                     , outfile = "data/processing steps/p05_pluto_with_sales.rds") {
  
  message("## Combining PLUTO with Sales data")
  message("Loading PLUTO, SALES+PAD data...")  
  pluto_raw <- read_rds(pluto_infile) %>% mutate(bbl = paste(BoroCode,as.numeric(Block),as.numeric(Lot),sep="_"))
  sales_pad_raw <- read_rds(sales_pad_infile)
  
  # normalize sales that take place more than 1 per year:
  message("Normalizing the sales data...")  
  
  merge_time <- Sys.time()
  sales <- 
    sales_pad_raw %>% 
    group_by(pluto_bbl, `SALE YEAR`) %>% 
    summarise(`BUILDING CLASS CATEGORY` = first(`BUILDING CLASS CATEGORY`)
              ,`TAX CLASS AT PRESENT` = first(`TAX CLASS AT PRESENT`)
              , `BUILDING CLASS AT PRESENT` = first(`BUILDING CLASS AT PRESENT`)
              , `ZIP CODE` = first(`ZIP CODE`)
              , `RESIDENTIAL UNITS` = mean(`RESIDENTIAL UNITS`, na.rm = TRUE)
              , `COMMERCIAL UNITS` = mean(`COMMERCIAL UNITS`, na.rm = TRUE)
              , `TOTAL UNITS` = mean(`TOTAL UNITS`, na.rm = TRUE)
              , `LAND SQUARE FEET` = mean(`LAND SQUARE FEET`, na.rm = TRUE)
              , `GROSS SQUARE FEET` = mean(`GROSS SQUARE FEET`, na.rm = TRUE)
              , `YEAR BUILT` = first(`YEAR BUILT`)
              , `TAX CLASS AT TIME OF SALE` = first(`TAX CLASS AT TIME OF SALE`)
              , `BUILDING CLASS AT TIME OF SALE` = first(`BUILDING CLASS AT TIME OF SALE`)
              , `SALE PRICE` = mean(`SALE PRICE`, na.rm = T)
              , TOTAL_SALES = sum(`SALE PRICE`, na.rm = T)
              , Year = first(Year)
              , SALE_DATE = mean(SALE_DATE, na.rm = T)
              , SALE_YEAR = first(SALE_YEAR)
              , bbl = first(bbl)
              , Annual_Sales = n()
    ) %>% 
    mutate(Sold = 1)
  
  message("Merging sales data with PLUTO...")  
  pluto_with_sales <- 
    left_join(pluto_raw, sales, by = c("bbl"="pluto_bbl", "Year"="SALE YEAR")) %>% 
    select(-contains(".y")) %>% mutate(Sold = if_else(is.na(Sold),0,Sold)) %>% 
    
    # removing select variables which are redundant
    select(-BBL, -XCoord, -YCoord, -file, -`ZIP CODE`, -`RESIDENTIAL UNITS`, -`COMMERCIAL UNITS`
           ,-`TOTAL UNITS`,-`LAND SQUARE FEET`, -`YEAR BUILT`, -Ext) %>% 
    
    # additional data munging
    mutate(AssessTotal = as.numeric(AssessTotal), ExemptTotal = as.numeric(ExemptTotal)
           , AssessTot = ifelse(is.na(AssessTot), AssessTotal, AssessTot)
           ,  ExemptTot = ifelse(is.na(ExemptTot), ExemptTotal, ExemptTot)
    ) %>% 
    select(-AssessTotal, -ExemptTotal) %>% 
    mutate(CornerLot = as.numeric(ifelse(CornerLot=="Y",1,0))
           , FAR = as.numeric(FAR)
           , IrrLotCode = as.numeric(ifelse(IrrLotCode=="Y",1,0))
           , MaxAllwFAR = suppressWarnings(as.numeric(MaxAllwFAR))
           , `GROSS SQUARE FEET` = as.numeric(`GROSS SQUARE FEET`)) %>% 
    
    # there are a number of micro-sales of 0.5 and 1. Removing those
    mutate_at(vars(`SALE PRICE`, TOTAL_SALES), .funs = function(x) if_else(x<2, NA_real_, x)) %>% 
    mutate(`SALE PRICE` = `SALE PRICE`/`GROSS SQUARE FEET`) %>% 
    mutate(`SALE PRICE` = ifelse(is.nan(`SALE PRICE`), NA, `SALE PRICE`)) %>% 
    mutate_at(vars(BldgArea:BldgDepth), function(x) ifelse(is.na(x),0,x))
  
  # GLOBAL FILTERING
  message("Applying global filtering to data...")
  pluto_with_sales <-
    pluto_with_sales %>%
    
    # remove problematic building classes, like hotel Time Share deeds
    filter(Building_Type%in%c("A","B","C","D","F","L","O")) %>% # eliminates ~2 million records
    
    # remove any tax lots with >1 building (e.g, World Trade Center)
    filter(NumBldgs==1) %>% # eliminates ~2 million records
    
    # remove micro transactions
    # filter(`SALE PRICE`>1|is.na(`SALE PRICE`)) %>% # eliminates small number of records
    
    # remove GSF of less than 500
    filter(`GROSS SQUARE FEET`>=50|is.na(`GROSS SQUARE FEET`)) # eliminates a small number of records
    
  
  merge_end <- Sys.time()
  message("     ...done")  
  message("Merge time: ", round(merge_end-merge_time, 2), units(merge_end-merge_time))
  
  message("Writing PLUTO with sales to disk...")  
  write_rds(pluto_with_sales, outfile)
  message("Done. Pluto combined with sales and written to ", outfile)  
}
