
# This function downloads all of NYC PAD data

download_nyc_pad <- function(download_path = "https://data.cityofnewyork.us/download/bc8t-ecyu/application%2Fzip"
                             , save_file = "data/processing steps/p02_pad_raw.rds"){
  
  # parallel create an expanded list of Condo BBL's from PAD's bbl directory
  # Basic idea is that PAD gives you a low BBL and a high BBL, so in order to 
  # map to PLUTO and/or sales data, you need to expand the list to include all
  # BBL's in that range
  
  # Note: run time on 10 cores was approximately 55 minutes
  
  if(!file.exists("data/aux data/pad17b/bobabbl.txt")){
    tmp <- tempfile()
    download.file(download_path, destfile = tmp)
    unzip(tmp, exdir = "data/aux data/pad17b")
    unlink(tmp)
  }
  
  bbl_dat <- suppressMessages(read_csv("data/aux data/pad17b/bobabbl.txt"))
  
  # There are no block or lot ranges, only lot ranges
  test_ranges <- 
    bbl_dat %>% 
    select(loboro, loblock, lolot, hiboro, hiblock, hilot) %>% 
    mutate(boro_range = as.numeric(hiboro)-as.numeric(loboro)
           ,block_range = as.numeric(hiblock)-as.numeric(loblock)
           ,lot_range = as.numeric(hilot)-as.numeric(lolot)) %>% 
    summary()
  
  bbl_ranges <- 
    bbl_dat %>% 
    mutate(lot_range = as.numeric(hilot)-as.numeric(lolot)) %>% 
    filter(lot_range>0)
  
  bbl_not_ranges <- 
    bbl_dat %>% 
    mutate(lot_range = as.numeric(hilot)-as.numeric(lolot)) %>% 
    filter(lot_range==0)
  
  # we should end up with 1,092,917 rows at the end
  # (bbl_ranges$lot_range %>% sum())+(nrow(bbl_dat)-nrow(bbl_ranges))
  
  
  # function to expand a range:
  unpack_rows <- function(low,hi) {
    a_seq <- paste0(seq(as.numeric(low),as.numeric(hi)),collapse = ",")
    a_seq[1]
  }
  # unpack_rows(1001,1052)
  
  ## Regular for loop:
  # bbl_expanded <- data_frame()
  # for(i in 1:nrow(bbl_dat)){
  #   cat(i,"of",nrow(bbl_dat),"\n")
  #   row <- bbl_dat %>% filter(row_number()==i)
  #   row$Id <- i 
  #   lo <- row$lolot
  #   hi <- row$hilot
  #   a_seq <- seq(as.numeric(lo),as.numeric(hi))
  #   df <- tibble("new_lot"=a_seq)
  #   df <- df %>% mutate(Id=row$Id)
  #   df_out <- left_join(df,row, by = "Id")
  #   bbl_expanded <- bind_rows(bbl_expanded,df_out)
  # }
  
  
  ## Parallel loop:
  message("Initiating parallel cluster for BBL range expansion at ", Sys.time())
  cl <- makeCluster(detectCores()-2)
  registerDoParallel(cl)
  
  expander_start_time <- Sys.time()
  bbl_expanded <- 
    foreach(i = 1:nrow(bbl_ranges), .combine=bind_rows) %dopar% {
      
      library(tidyverse) # push packages to the individual clusters
      row <- bbl_dat %>% filter(row_number()==i)
      row$Id <- i 
      lo <- row$lolot
      hi <- row$hilot
      a_seq <- seq(as.numeric(lo),as.numeric(hi))
      df <- tibble("new_lot"=a_seq)
      df <- df %>% mutate(Id=row$Id)
      df_out <- left_join(df,row, by = "Id")
      df_out
    }
  
  expander_end_time <- Sys.time()
  (tot_time <- expander_end_time - expander_start_time)
  stopCluster(cl); rm(cl)
  message("Cluster Expansion completed. ",nrow(bbl_ranges), " rows expanded to ", nrow(bbl_expanded))
  
  message("Writing to disk...")
  write_rds(bbl_expanded, save_file, compress = "gz")
  message("PAD file downloaded and expanded. Processed file saved to: ", save_file)
}


