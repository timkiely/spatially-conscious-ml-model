

source("R/helper/load-packages.R")
source("R/helper/source-files.R")


data_path <- "C:/Users/tkiely/Dropbox/MSPA/Thesis/Analysis/full-data"

## all base data:
base_data <- read_rds(paste0(data_path,"/","p05_pluto_with_sales.rds"))



# 1) ----------------------------------------------------------------------
# step 1: create subset of base data with bldg typ C, D and Manhattan, BK and BX
base_data_subset <- base_data %>% 
  filter(Building_Type %in% c("D","C")) %>% 
  filter(Borough %in% c("MN","BK","BX"))

# write subset data to new file
write_rds(base_data_subset, "data/processing steps/p17_pluto_with_sales_subset.rds")



# 2) ----------------------------------------------------------------------
# step 2: re-run feature generation on subset of data

# base data
create_base_data(pluto_with_sales_infile = "data/processing steps/p17_pluto_with_sales_subset.rds"
                 , outfile = "data/processing steps/p18_base_model_data_subset.rds"
                 , limit_boros = FALSE)

# zipcode data
create_zipcode_data(base_model_data = "data/processing steps/p18_base_model_data_subset.rds"
                    , outfile = "data/processing steps/p19_zipcode_model_data_subset.rds")

# radii data. Note: extremely time intensive. Last full data run was 3.3 hours
create_radii_data(base_model_data = "data/processing steps/p18_base_model_data_subset.rds"
                  , outfile = "data/processing steps/p20_radii_model_data_subset.rds"
                  #  to run, explicity supply the "--run-radii" argument or modify the funtion argument run_radii below
                  , run_radii = FALSE)


