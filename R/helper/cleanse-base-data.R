

cleanse_base_data <- function(pluto){
  
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
  
  
}