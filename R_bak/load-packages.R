

# check if pacman is loaded. if not, prompt user for fresh install
if("pacman"%in%installed.packages()){
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


# all the packages we need for this script:
pacman::p_load(
  caret, plyr, dplyr, tidyverse
  , magrittr, lubridate, stringr
  , purrr, modelr, sf
  , lubridate, Matrix, doMC
  , parallel, foreach, doSNOW
  , doParallel, vtreat
  , xgboost, randomForest, elasticnet
  , progress, knitr, h2o, data.table
  , htmlTable, pander, rPython, kknn
)