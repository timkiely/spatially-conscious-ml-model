
# run the entire analysis from this script file

# source all the necessary functions:
source("R/helper/source-files.R")


# data --------------------------------------------------------------------
download_nyc_pluto( save_file = "data/processing steps/p01_pluto_raw.rds")
download_nyc_pad(   save_file = "data/processing steps/p02_pad_raw.rds")
download_nyc_sales( save_file = "data/processing steps/p03_sales_raw.rds")


# processing --------------------------------------------------------------
combine_sales_and_pad(sales_infile = "data/processing steps/p03_sales_raw.rds"
                      , pad_infile = "data/processing steps/p02_pad_raw.rds"
                      , outfile = "data/processing steps/p04_sales_and_pad.rds")

combine_pluto_with_sales(pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                         , sales_pad_infile = "data/processing steps/p04_sales_and_pad.rds"
                         , outfile = "data/processing steps/p05_pluto_with_sales.rds")

# features and partitions -------------------------------------------------

# base data
create_base_data(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                 , outfile = "data/processing steps/p06_base_model_data.rds")

# zipcode data
create_zipcode_data(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                    , outfile = "data/processing steps/p07_zipcode_model_data.rds")

# radii data
create_radii_data(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                  , outfile = "data/processing steps/p08_radii_model_data.rds")


# Prob of sale model ------------------------------------------------------

# base data
run_probability_model(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                      , outfile = "data/processing steps/p09_prob_of_sale_model_base.rds")

# zipcode data
run_probability_model(model_data_infile = "data/processing steps/p07_zipcode_model_data.rds"
                      , outfile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds")

# radii data
run_probability_model(model_data_infile = "data/processing steps/p08_radii_model_data.rds"
                      , outfile = "data/processing steps/p11_prob_of_sale_model_radii.rds")



# sale price model --------------------------------------------------------

# base data
run_sales_model(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                      , outfile = "data/processing steps/p12_sale_price_model_base.rds")

# zipcode data
run_sales_model(model_data_infile = "data/processing steps/p07_zipcode_model_data.rds"
                      , outfile = "data/processing steps/p13_sale_price_model_zipcode.rds")

# radii data
run_sales_model(model_data_infile = "data/processing steps/p08_radii_model_data.rds"
                      , outfile = "data/processing steps/p14_sale_price_model_radii.rds")















