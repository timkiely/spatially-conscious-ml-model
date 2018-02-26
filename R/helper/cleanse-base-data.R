

cleanse_base_data <- function(pluto){
  pluto <- 
    pluto %>% 
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
  pluto <-
    pluto %>%
    
    # remove problematic building classes, like hotel Time Share deeds
    filter(Building_Type%in%c("A","B","C","D","F","G","L","O")) %>% # eliminates ~2 million records
    
    # remove any tax lots with >1 building (e.g, World Trade Center)
    filter(NumBldgs<2) %>% # eliminates ~2 million records
    
    # remove GSF of less than 500
    filter(`GROSS SQUARE FEET`>=50|is.na(`GROSS SQUARE FEET`)) # eliminates a small number of records
  
  
  # certain variables may be missing or change slightly over time. standardize variables across bbls
  standard_combos <- 
    pluto %>% 
    arrange(-Year) %>% 
    distinct(bbl, ZipCode, Address, lat, lon) %>% 
    filter(!is.na(ZipCode), !is.na(Address)
           , !is.na(lat), !is.na(lon)
    ) %>% 
    distinct(bbl, .keep_all = TRUE) %>% 
    arrange(bbl)
  
  pluto <- 
    pluto %>% 
    select(-ZipCode, -Address, -lat, -lon) %>% 
    left_join(standard_combos, by = "bbl")
  
  # removing any variables still missing lat/lon
  pluto <- pluto %>% filter(!is.na(lat),!is.na(lon))
  
  pluto
}