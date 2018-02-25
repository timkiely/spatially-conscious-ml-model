

message("checking if packages are installed...")
# check if pacman is loaded. if not, prompt user for fresh install
if(!"pacman"%in%installed.packages()){
  message("\n\nWARNING: This script needs to install several dependencies. For a complete list, see the `load-packages` portion of this program
          \n")
  message("Do you wish to install all dependencies (this can take several minutes)?")
  input <- readline(prompt = "Y/n: ")
  if(input=="Y"){
    install.packages("pacman")
  } else {
      message("terminated")
    }
}

message("loading packages...")

# all the packages we need for this script:
pacman::p_load(
  gdata, caret, plyr 
  , magrittr, stringr
  , purrr, modelr, sf
  , lubridate, Matrix, doMC
  , parallel, foreach, doSNOW
  , doParallel
  , xgboost, randomForest, elasticnet
  , knitr, h2o, data.table
  , htmlTable, pander, rPython, kknn
  , proj4, httr, RCurl, rvest, TTR, zoo, RcppRoll
  , e1071, progress, pROC, optparse, spdep, tidyverse
  , data.table, tibble, dplyr
)


# dplyr verbs sometimes get masked unexpectedly, so making sure that doesn't happen:
select <- dplyr::select
summarise <- dplyr::summarise

message("packages loaded successfully")