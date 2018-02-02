## run the entire analysis from this script file
message("Starting script at ", as.POSIXct(Sys.time(), tz = "EST"))
script_start <- Sys.time()

# script arguments:
args <- commandArgs(TRUE)
DL <- as.character(args[1]) # 'skip-dl' bypasses the download steps
PP <- as.character(args[2]) # 'skip-pp' bypasses the pre-processing steps
dev <- as.character(args[3]) # 'run-dev' train the models with sample data, to go much faster
run_radii <- as.character(args[4]) # 'run-radii' re-runs the full radii indexing operation. Else, loads from disk


# load packages and source the necessary scripting functions:
source("R/helper/load-packages.R")
source("R/helper/source-files.R")


# data --------------------------------------------------------------------
# approx. 5 minutes
if(is.na(DL)) DL <- "N"
if(tolower(DL) == "skip-dl"){
  message("=====> Bypassing download function")
} else {
  download_nyc_pluto( save_file = "data/processing steps/p01_pluto_raw.rds")
  download_nyc_pad(   save_file = "data/processing steps/p02_pad_raw.rds") # steps 1 and 2 take 13.5 minutes
  download_nyc_sales( save_file = "data/processing steps/p03_sales_raw.rds") # steps 1, 2 and 3 take 55 mins (from scratch)
}  


# processing --------------------------------------------------------------
# approx. 30 minutes
if(is.na(PP)) PP <- "N"
if(tolower(PP) == "skip-pp") {
  message("=====> Bypassing preprocessing function")
} else {
  
  combine_sales_and_pad(sales_infile = "data/processing steps/p03_sales_raw.rds"
                        , pad_infile = "data/processing steps/p02_pad_raw.rds"
                        , pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                        , outfile = "data/processing steps/p04_sales_and_pad.rds")
  
  
  combine_pluto_with_sales(pluto_infile = "data/processing steps/p01_pluto_raw.rds"
                           , sales_pad_infile = "data/processing steps/p04_sales_and_pad.rds"
                           , outfile = "data/processing steps/p05_pluto_with_sales.rds")
  
  
  # features and partitions -------------------------------------------------
  
  
  # base data
  create_base_data(pluto_with_sales_infile = "data/processing steps/p05_pluto_with_sales.rds"
                   , outfile = "data/processing steps/p06_base_model_data.rds")
  
  # zipcode data
  create_zipcode_data(base_model_data = "data/processing steps/p06_base_model_data.rds"
                      , outfile = "data/processing steps/p07_zipcode_model_data.rds")
  
}

if(is.na(run_radii)) run_radii <- "N"
# radii data
create_radii_data(base_model_data = "data/processing steps/p06_base_model_data.rds"
                  , outfile = "data/processing steps/p08_radii_model_data.rds"
                  , run_radii = run_radii)


# Prob of sale model ------------------------------------------------------

# base data
run_probability_model(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                      , outfile = "data/processing steps/p09_prob_of_sale_model_base.rds"
                      , dev = dev)

# zipcode data
run_probability_model(model_data_infile = "data/processing steps/p07_zipcode_model_data.rds"
                      , outfile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds")

# radii data
run_probability_model(model_data_infile = "data/processing steps/p08_radii_model_data.rds"
                      , outfile = "data/processing steps/p11_prob_of_sale_model_radii.rds")



# sale price model --------------------------------------------------------

# base data
run_sales_model(model_data_infile = "data/processing steps/p06_base_model_data.rds"
                , outfile = "data/processing steps/p12_sale_price_model_base.rds"
                , dev = dev)

# zipcode data
run_sales_model(model_data_infile = "data/processing steps/p07_zipcode_model_data.rds"
                , outfile = "data/processing steps/p13_sale_price_model_zipcode.rds")

# radii data
run_sales_model(model_data_infile = "data/processing steps/p08_radii_model_data.rds"
                , outfile = "data/processing steps/p14_sale_price_model_radii.rds")



# Evaluate model output ---------------------------------------------------
evaluate_probability_models(base_data_inflie = "data/processing steps/p09_prob_of_sale_model_base.rds"
                            , zip_data_infile = "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
                            , radii_data_infile = "data/processing steps/p11_prob_of_sale_model_radii.rds"
                            , outfile = "data/processing steps/p15_prob_model_evaluations.rds")

evalutate_sales_models(base_data_inflie = "data/processing steps/p12_sale_price_model_base.rds"
                       , zip_data_infile = "data/processing steps/p13_sale_price_model_zipcode.rds"
                       , radii_data_infile = "data/processing steps/p14_sale_price_model_radii.rds"
                       , outfile = "data/processing steps/p16_sales_model_evaluations.rds")


script_end <- Sys.time()
message("Program Total Run Time: ", round(script_end - script_start,2), units(script_end - script_start))













