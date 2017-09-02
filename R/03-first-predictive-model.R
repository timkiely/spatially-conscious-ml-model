
rm(list=ls()[!ls()%in%c("sale_augmented")])
library(tidyverse)


# sales data ------------------------------------------------------------------
if(!exists("sale_augmented")){
  sale_augmented <- read_rds("data/sales_augmented.rds")
}


sale_modeling <- 
  sale_augmented %>% 
  filter(SALE.PRICE>0) %>% 
  filter(!BUILDING.CLASS.AT.TIME.OF.SALE%in%c("H3","H1")) %>% 
  select(BOROUGH:BUILDING.CLASS.AT.PRESENT,-ADDRESS,-BBL_derive
  )

# frequency of transactions -----------------------------------------------
# all_sales <- 
#   sale_augmented %>% 
#   filter(SALE.PRICE>0) %>% 
#   mutate(sold = ifelse(!is.na(SALE.PRICE),1,0))
# 
# all_sales %>% group_by(sold) %>% count() %>% mutate(perc = n/sum(n)) %>% 
#   ggplot()+aes(x = sold, y = n) + geom_col()




# sale price --------------------------------------------------------------
library(xgboost)

num_vars <- names(sapply(all_sales,is.numeric))

f1 <- as.formula(SALE.PRICE~.)



f1_glm <- glm(f1, data  = all_sales %>% select_if(.predicate=is.numeric))
summary(f1_glm)

