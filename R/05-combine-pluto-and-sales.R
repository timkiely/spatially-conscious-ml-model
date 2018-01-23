
# This function combines PLUTO with the sales&pad data

combine_pluto_with_sales <- function(pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                                     , sales_pad_infile = "data/processing steps/p04_sales_and_pad.rds"
                                     , outfile = "data/processing steps/p05_pluto_with_sales.rds") {
  
  message("Loading PLUTO and SALES data...")  
  pluto_raw <- read_rds(pluto_infile) %>% mutate(bbl = paste(BoroCode,as.numeric(Block),as.numeric(Lot),sep="_"))
  sales_pad_raw <- read_rds(sales_pad_infile)

  # functions for calculating various sales metrics:
  mean_na <- function(x) mean(x, na.rm = TRUE)
  min_na <- function(x) min(x, na.rm = TRUE)
  max_na <- function(x) max(x, na.rm = TRUE)
  median_na <- function(x) median(x, na.rm = TRUE)
  Q3_na <- function(x) as.numeric(quantile(x, na.rm = TRUE, probs = 0.75))
  
  
  pluto <- pluto_raw 
  
  sales_more_than_one_per_year <- 
    sales_pad_raw %>% 
    group_by(pluto_bbl, `SALE YEAR`) %>% 
    mutate(annual_sales = n()) %>% 
    filter(count>1)
  
  sales_more_than_one_per_year2 <- 
    sales_more_than_one_per_year %>% 
    ungroup() %>% 
    group_by_if(.predicate = is.character) %>% 
    summarise_all(.funs = mean, na.rm = T)
  
  ## LEFT OFF: HOW TO COMBINE SALES WITH PLUTO EFFECTIVELY?
  
  sales_more_than_one_per_year %>% filter(pluto_bbl == "1_1274_7504") %>% glimpse()

  
  sales_one_per_year <- 
    sales_pad_raw %>% 
    group_by(pluto_bbl, `SALE YEAR`) %>% 
    mutate(annual_sales = n()) %>% 
    filter(annual_sales==1)
  
  sales_more_than_one_per_year
  
  message("Mergin sales data with PLUTO...")  
  pluto_with_sales <- left_join(pluto, sales, by = c("bbl"="pluto_bbl", "Year"="SALE YEAR")) 
    
  
  message("TODO: function to combine PLUTO with SALES&PAD data")
}
