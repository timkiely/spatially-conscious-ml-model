

######################################################
## Helper Functions
######################################################

## tidy up fields with dollar signs
dollar.fix <- function(var){
  var <- gsub("[^[:^punct:].]","",var,perl=T)
  var <- as.numeric(as.character(var))
  return(var)
}

## remove commas
comma.rem <- function(x){
  if(class(x)=="character"){
    x <- gsub(",","",x)
  }
  return(x)
}



# checks for project directory structure and creates it if it doesn't exist
project_makefile <- function() {
  if(!"data" %in% dir()){
    dir.create("data")
  }
  
  if(!"aux data" %in% dir("data")){
    dir.create("data/aux data")
  }
  
  if(!"processing steps" %in% dir("data")){
    dir.create("data/processing steps")
  }
  
}


# calculate exponential moving average with RcppRoll::roll_mean()
exp_roll_meanr <- function(x, exp = 0.9, n = 2, na.rm = TRUE, fill = NaN) {
  roll_meanr(x, n = n, weights = (exp^(0:n)), na.rm = na.rm, fill = fill)
} 
