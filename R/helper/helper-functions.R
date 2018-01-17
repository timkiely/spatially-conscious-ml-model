

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


# process sales data
PROCESS_SALES_DATA <- function(sales_data){
  sales_data %>% 
    filter(SALE.PRICE>=10000) %>% 
    filter(GROSS.SQUARE.FEET>500) %>% 
    filter(Building_Type%in%c("A","B","C","D","F","L","O")) %>% 
    mutate(SALE.PRICE = SALE.PRICE/GROSS.SQUARE.FEET)
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
  
  if(!"log" %in% dir()){
    dir.create("log")
  }
  
}
