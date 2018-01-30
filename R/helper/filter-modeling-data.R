
filter_modleing_data <- function(data){
  data <- 
    data %>% 
    filter(NumBldgs==1) %>% 
    filter(SALE_PRICE>=10000) %>% 
    filter(GROSS_SQUARE_FEET>500)
}