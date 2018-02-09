

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


# set of standard filters
PROCESS_SALES_DATA <- function(sales_data){
  sales_data %>% 
    filter(SALE.PRICE>=10000) %>% 
    filter(GROSS.SQUARE.FEET>500) %>% 
    filter(Building_Type%in%c("A","B","C","D","F","L","O")) %>% 
    mutate(SALE.PRICE = SALE.PRICE/GROSS.SQUARE.FEET)
}

# calculate exponential moving average with RcppRoll::roll_mean()
exp_roll_meanr <- function(x, exp = 0.9, n = 2, na.rm = TRUE, fill = NaN) {
  roll_meanr(x, n = n, weights = (exp^(0:n)), na.rm = na.rm, fill = fill)
} 
