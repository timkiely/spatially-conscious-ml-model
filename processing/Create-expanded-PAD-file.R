
library(tidyverse)
# This script takes NYC's 2017 Property Address Directory (PAD) file and
# maps each address to PLUTO.

# pad_bbl_expanded was created by adding a unique
# row for every row in PAD that had a range of lots for
# a single BBL. These were mostly condos
pad_bbl_expanded <- read_rds("data/PAD_Condo_BBLs_expanded_v002.rds")
pluto_lean <- read_rds("data/pluto_lean.rds")


lookat <- function(boro= 1,blck = 829,lt = 16,data = pluto_lean) data %>% filter(BOROUGH == boro, Block == as.numeric(blck), Lot == as.numeric(lt)) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16,data = pluto_lean) data %>% filter(BOROUGH == boro, Block == blck, Lot == lt)


# join the PAD file to PLUTO using the 'billing bbl' fields
rm(pad_expanded_with_pluto)
pad_expanded_with_pluto <-
  anti_join(pad_bbl_expanded %>% mutate(billing_bbl = paste(new_boro,as.numeric(new_block),as.numeric(new_lot),sep="_"))
            , pluto_lean %>% mutate(bbl = paste(BOROUGH,as.numeric(Block),as.numeric(Lot),sep="_"))
            , by = c("billing_bbl"="bbl")
            ); beepr::beep(4)


pad_expanded_with_pluto %>% arrange(boro) %>% head() %>% glimpse()

lookat(5,27000,0523)
