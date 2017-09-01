

library(tidyverse)

sale_augmented <- read_rds("data/sales_with_pluto.rds")
pluto_lean <- read_rds("data/pluto_lean.rds")

pluto_lean$Year %>% table()
sale_augmented$SALE_YEAR %>% table()


all_sales <-
  left_join(pluto_lean,sale_augmented
            ,by = c(
              'Year'='SALE_YEAR'
              ,'BOROUGH'='BOROUGH'
              ,'Block'='BLOCK'
              ,'Lot'='LOT')
            )

all_sales <- 
  all_sales %>% 
  mutate(sold = ifelse(!is.na(SALE_DATE),1,0))

all_sales %>% group_by(sold) %>% count() %>% mutate(perc = n/sum(n))
  ggplot()+aes(x = sold, y = n) + geom_col()


library(xgboost)
f1 <- as.formula(sold~.)

f1_glm <- glm(f1, data  = all_sales)
summary(f1_glm)

