
# This function downloads all of NYC Rolling SALES data

download_nyc_sales <- function(save_file = "data/processing steps/p03_sales_raw.rds") {
  
  # create a dest directory if not already exists 
  if(!dir.exists("data/aux data/sales data raw")){
    dir.create("data/aux data/sales data raw")
  }
  if(!dir.exists("data/aux data/sales data csv")){
    dir.create("data/aux data/sales data csv")
  }
  
  # use rvest to scrape all download links from the page
  # source: http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page
  if(url.exists("http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page")){
    links <-
      read_html("http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page") %>% 
      html_nodes("td~ td+ td a") %>% html_attr('href') %>% 
      map_chr(function(x) paste0("http://www1.nyc.gov",x)) %>% 
      discard(function(x) str_detect(x, "citywide|sales_statistics|sale_statistics|sales_prices"))
  } else stop("Rolling Sales URL not reachable: http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page")
  
  # add 12 month rolling sales data:
  if(url.exists("http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page")){
    twelve_month <-
      read_html("http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page") %>% 
      html_nodes(".rolling_sales a") %>% html_attr('href') %>% 
      map_chr(function(x) paste0("http://www1.nyc.gov",x)) %>% 
      keep(function(x) str_detect(x, "[.]xls"))
  } else stop("Rolling Sales URL not reachable: http://www1.nyc.gov/site/finance/taxes/property-rolling-sales-data.page")
  
  links <- c(links, twelve_month)
  yet_to_download <- links[!basename(links) %in% dir("data/aux data/sales data raw")]
  
  # for testing:
  #yet_to_download <- yet_to_download[1:10]
  writeLines(yet_to_download, "data/aux data/raw_sales_data_urls.txt")
  
  
  # download files in parallel using xargs (Unix only)
  message("Downloading sales data files in parallel using xargs at ",Sys.time())
  system(paste0("cat 'data/aux data/raw_sales_data_urls.txt' | xargs -n 1 -P ", ceiling(parallel::detectCores()/2)," wget -c -P 'data/aux data/sales data raw'")
         , ignore.stdout = TRUE
         , ignore.stderr = TRUE
         , wait = TRUE)
  
  message("     ...xls files downloaded")
  
  # clean up the cache index
  unlink("data/aux data/raw_sales_data_urls.txt")
  
  our_files <- dir("data/aux data/sales data raw")
  sales_df_out <- vector("list", length(our_files))
  for(j in 1:length(our_files)){ 
    
    file_name <- our_files[j]
    year <- 
      file_name %>% str_extract_all("[0-9]") %>%
      unlist() %>% paste0(collapse = "") %>% 
      map_dbl(function(x) ifelse(nchar(x)==2, as.numeric(paste0("20",x)),as.numeric(x)))
    
    # only re-download the rolling sales data once per month:
    if(is.na(year)){
      year <- "rolling"
      file_name_csv <- str_replace(file_name, "xlsx|xls","csv")
      file_name_csv <- paste0(lubridate::year(Sys.Date()),sprintf("%02.0f",lubridate::month(Sys.Date())), "_",file_name_csv)
    } else {
      file_name_csv <- str_replace(file_name, "xlsx|xls","csv")
    }
    
    
    message("Working on ", file_name)
    
    if(file_name_csv %in% dir("data/aux data/sales data csv")){
      message("      ....already downloaded, reading from disk")
      sales_df_out[[j]] <- 
        suppressWarnings(suppressMessages(
          read_csv(paste0("data/aux data/sales data csv/",file_name_csv)
                   , col_types = cols(.default = "c")
                   , progress = FALSE)
        ))
      message("      ....done")
    } else {
      
      
      # gdata has the flexible and useful "pattern" argument. Slower but able to handle varying formats
      message("      ....extracting Excel file to csv")
      xl_file <- tbl_df(gdata::read.xls(paste0("data/aux data/sales data raw/",file_name), sheet=1, pattern="BOROUGH",as.is=T))
      xl_file$from_file <- file_name_csv
      xl_file$Year <- year
      xl_file <- xl_file %>% mutate_all(as.character)
      
      message("      ....writing to disk")
      write_csv(xl_file, paste0("data/aux data/sales data csv/",file_name_csv))
      sales_df_out[[j]] <- xl_file
      message("      ....done with: ", file_name_csv)
    }
  }; message("Sales data downloaded and read into memory")
  
  
  ## making column names consistent
  message("Final processing of sales data...")
  nms <- 
    names(sales_df_out[[1]]) %>% 
    str_replace("[.]$","") %>% 
    str_replace_all("[.]"," ")
  
  sales_df_out <- 
    sales_df_out %>% map(function(x) {
      names(x) <- nms
      x <- x %>% mutate_all(as.character)
      x <- x %>% mutate_all(trimws)
      return(x)
    }
    )
  
  ## putting list into single dataframe
  final_object <- bind_rows(sales_df_out)
  # de-dupe the list, in case rolling sales were included in past trasactions
  final_object <- 
    final_object %>% 
    distinct(BOROUGH, BLOCK, LOT, `EASE MENT`
             , ADDRESS, `SALE DATE`, `SALE PRICE` 
             , `GROSS SQUARE FEET`, .keep_all = TRUE)
  
  final_object <-
    final_object %>%
    mutate(
      `BUILDING CLASS CATEGORY` = trimws(`BUILDING CLASS CATEGORY`)
      , `ZIP CODE` = as.integer(`ZIP CODE`)
      , SALE_DATE = as.Date(`SALE DATE`, format = "%Y-%m-%d")
    ) %>% 
    mutate(`SALE PRICE` = as.numeric(str_replace_all(`SALE PRICE`, "[$]|[,]|[-]",""))) %>% 
    mutate_at(vars(`RESIDENTIAL UNITS`:`GROSS SQUARE FEET`), str_replace_all, "[$]|[,]|[-]","") %>% 
    mutate_at(vars(`RESIDENTIAL UNITS`:`YEAR BUILT` ), as.numeric) %>% 
    mutate(`SALE YEAR` = lubridate::year(SALE_DATE))
  
  message("Writing sales data to disk...")
  write_rds(final_object, save_file)
  message("Complete sales data written to ",save_file)
}

