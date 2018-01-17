
# This function downloads all of NYC Rolling SALES data

download_nyc_sales <- function(save_file = "data/processing steps/p03_sales_raw.rds") {
  
  # TODO: paralellize? 
 
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
  
  yet_to_download <- links[!basename(links) %in% dir("data/aux data/sales data")]
  yet_to_download <- yet_to_download[1:3]
  writeLines(yet_to_download, "data/aux data/raw_sales_data_urls.txt")
  
  
  # download files in parallel using xargs (Unix only)
  message("Downloading files in parallel")
  system(paste0("cat 'data/aux data/raw_sales_data_urls.txt' | xargs -n 1 -P ", ceiling(parallel::detectCores()/2)," wget -c -P 'data/aux data/sales data'")
         , ignore.stdout = TRUE
         , ignore.stderr = TRUE
         , wait = TRUE)

  sales_df_out <- vector("list", length(links))
  for(j in length(sales_df_out):1){ 
    
    dl_url <- links[j]
    file_name <- basename(dl_url)
    file_name_csv <- str_replace(file_name, "xlsx|xls","csv")
    year <- 
      file_name %>% str_extract_all("[0-9]") %>%
      unlist() %>% paste0(collapse = "") %>% 
      map_dbl(function(x) ifelse(nchar(x)==2, as.numeric(paste0("20",x)),as.numeric(x)))
    
    message("Working on ", dl_url)
    
    if(file_name_csv %in% dir("data/aux data/sales data")){
      message("      ....already downloaded, reading from disk")
      sales_df_out[[j]] <- try(suppressWarnings(suppressMessages(read_csv(paste0("data/aux data/sales data/",file_name_csv)))), silent = TRUE)
      message("      ....done")
    } else {
      
      # otherwise download it
      message("      ....downloading file")
      check_url <- url.exists(dl_url,.header=T)
      if(check_url["status"] != "200") next("Warning: download failed for ", dl_url)
      
      tmp <- tempfile()
      download.file(dl_url, destfile = tmp, method="curl", quiet=TRUE)
      # gdata has the flexible and useful "pattern" argument. Slower but able to handle varying formats
      message("      ....extracting from Excel file")
      xl_file <- tbl_df(gdata::read.xls(tmp, sheet=1, pattern="BOROUGH",as.is=T))
      unlink(tmp)
      xl_file$from_file <- file_name_csv
      xl_file$Year <- year
      message("      ....writing to disk")
      write_csv(xl_file, paste0("data/aux data/sales data/",file_name_csv))
      sales_df_out[[j]] <- xl_file
      message("      ....file downloaded: ", file_name_csv)
    }
  }; message("Data downloaded and read into memory")
  
  
  ## making column names consistent
  sales_df_out_2 <- lapply(sales_df_out, function(x) {
    colnames(x) <- colnames(df_list[[1]])
    return(x)
  })
  
  ## Changing factors to character to avoid errors downstream
  df_list <- lapply(df_list, function(df.tmp){
    change.cols <- as.numeric(
      which(
        sapply(df.tmp, class)!="character"
      )
    )
    df.tmp[,change.cols] <- sapply(df.tmp[,change.cols], as.character)
    return(df.tmp)
  }
  )
  
  ## putting list into single dataframe
  propsale.df <- bind_rows(df_list)
  
  write_rds(propsale.df, save_file)
}

