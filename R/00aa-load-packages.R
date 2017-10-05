
if(!"ponyexpress"%in%installed.packages()){
  stop("You must install ponyexpress with devtools::install_github('ropenscilabs/ponyexpress')")
}

# load packages
if(!"pacman"%in%installed.packages()){
  install.packages("pacman")
}

library(pacman)

p_load(
  caret
  , plyr
  , dplyr
  , tidyverse
  , magrittr
  , lubridate
  , stringr
  , purrr
  , modelr
  , sf
  , lubridate
  , Matrix
  , doMC
  , parallel
  , foreach
  , doSNOW
  , doParallel
  , vtreat
  , xgboost
  , randomForest
  , progress
  , knitr
  , ponyexpress
  , htmlTable
  , pander
)
