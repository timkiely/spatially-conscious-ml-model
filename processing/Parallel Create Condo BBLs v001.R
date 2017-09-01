

# parallel create an expanded list of Condo BBL's from PAD's bbl directory
# Basic idea is that PAD gives you a low BBL and a high BBL, so in order to 
# map to PLUTO and/or sales data, you need to expand the list to include all
# BBL's in that range

# Notes: run time on 10 cores was approximately 55 minutes

library(tidyverse)
library(foreach)
library(doParallel)


bbl_dat <- read_csv("data/pad17b/bobabbl.txt")



# There are no block or lot ranges, only lot ranges
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
(bbl_ranges$lot_range %>% sum())+(nrow(bbl_dat)-nrow(bbl_ranges))


low <- 1001
hi <- 1052
unpack_rows <- function(low,hi) {
  a_seq <- paste0(seq(as.numeric(low),as.numeric(hi)),collapse = ",")
  a_seq[1]
}

unpack_rows(low,hi)

bbl_ranges %>% 
  mutate(row_id = 1:n()) %>% 
  select(row_id,lolot,hilot) %>% 
  mutate(unpacked = map2_chr(lolot,hilot,unpack_rows)) %>% glimpse()


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
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)

rm(bbl_expanded)
start_time <- Sys.time()  

# 1,000 rows 21 seconds
# 10,000 rows 84 seconds



bbl_expanded <- 
  foreach(i = 1:nrow(bbl_ranges), .combine=bind_rows) %dopar% {
    
  library(tidyverse)
  row <- bbl_dat %>% filter(row_number()==i)
  row$Id <- i 
  lo <- row$lolot
  hi <- row$hilot
  a_seq <- seq(as.numeric(lo),as.numeric(hi))
  df <- tibble("new_lot"=a_seq)
  df <- df %>% mutate(Id=row$Id)
  df_out <- left_join(df,row, by = "Id")
  df_out
}; beepr::beep(4)

end_time <- Sys.time()
(tot_time <- end_time - start_time)


stopCluster(cl)



write_rds(bbl_expanded,"data/PAD_Condo_BBLs_expanded_par.rds",compress = "gz")



