

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
)
