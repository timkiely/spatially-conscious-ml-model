
rm(list=ls())
library(tidyverse)
library(sf)
library(stringr)





Boro <- 1  
Blck <- 829
lot <- '16' 
sale_augmented %>% filter(BOROUGH == Boro, BLOCK == Blck, LOT == lot) %>% glimpse()

pluto_lean %>% filter(BOROUGH == Boro, Block == Blck, Lot == lot) %>% select(Year) %>% table()


# sales data ------------------------------------------------------------------
sale_augmented <- read_rds("data/sales_augmented.rds")

# function to quickly glimpse BBL
lookat <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt)


sale_augmented %>% filter(BOROUGH == 1, BLOCK == 829, LOT == 16) %>% 
  select(ADDRESS,SALE_DATE, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal)




sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  filter(AssessTotal<500000000) %>% 
  #filter(SALE.PRICE>5e+07) %>% 
  ggplot()+
  aes(x = SALE.PRICE, y=AssessTotal, group = Building_Type, color = Building_Type)+
  geom_point()+
  geom_smooth(se=F, method = "lm")+
  scale_y_continuous(labels=scales::comma)


library(modelr)

f1 <- lm(AssessTotal~SALE.PRICE, data = sale_augmented)

group_model <- function(df) {
  lm(AssessTotal ~ SALE.PRICE, data = df)
}

extract_coef <- function(model) coef(model)["SALE.PRICE"]

extract_coef(f1)

by_group<-
sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  mutate(AssessSD = sd(AssessTotal), AssessZscore = scale(AssessTotal)) %>% 
  #filter(abs(AssessZscore)<3) %>% 
  group_by(Building_Type) %>% 
  nest() %>% 
  mutate(Number_of_Sales = map_dbl(data,nrow)) %>% 
  mutate(model = map(data, group_model)) %>% 
  mutate(Sale_Coef = map_dbl(model,extract_coef)) %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  arrange(-Sale_Coef) %>% 
  filter(p.value <= (0.05))
  
  
  



# note that in some places, the sale price is recorded multiple 
# times on the same day. You have to add those together
sale_augmented %>% 
  filter(SALE.PRICE>0) %>% 
  mutate(BUILDING_TYPE_SALE = substr(BUILDING.CLASS.AT.PRESENT,1,1)) %>% 
  #filter(BUILDING_TYPE_SALE%in%c("O","L")) %>%
  group_by(BOROUGH, BLOCK, LOT, SALE_YEAR,SALE_DATE) %>% 
  summarise(SALE.PRICE = sum(SALE.PRICE,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(SALE_YEAR) %>% summarise(Volume = sum(SALE.PRICE,na.rm=T)) %>% 
  ggplot()+aes(x = SALE_YEAR, y = Volume)+geom_col()+facet_wrap(~BOROUGH)
  
  group_by(BOROUGH, BLOCK, LOT) %>% 
  summarise(min_year= min(SALE_YEAR)
            , max_year = max(SALE_YEAR)
            , number_sales = n()
            , range = max(SALE.PRICE)-min(SALE.PRICE)
            , themin = min(SALE.PRICE)
            , themax = max(SALE.PRICE)) %>% 
  arrange(-number_sales)
    
    
# how many sales per year, and how many years do we have?
com_sales$SALE_YEAR %>% table() # %>% plot()

# how many times do certain BBL's appear in the data?
com_sales %>% 
  group_by(BOROUGH,BLOCK,LOT) %>% 
  count() %>% 
  arrange(-n) %>% 
  filter(n>1, n<2000)



# many BBL's appear hundreds or thousands of times. Are these apartments?
# only 6,565 transactions are non-apartments
com_sales %>% filter(APARTMENT.NUMBER!="") %>% summarise(n())
com_sales %>% filter(APARTMENT.NUMBER!="") %>% glimpse()

# are apartment sales responsible for the high number transacted buildings?
com_sales %>% 
  mutate(has_apartment = ifelse(APARTMENT.NUMBER!="",1,0)) %>% 
  group_by(BOROUGH,BLOCK,LOT) %>%
  summarise(count = n(), apartments = sum(has_apartment, na.rm=T)) %>% 
  filter(count>1, count<2000) %>% 
  ggplot()+
  aes(x = count, y = apartments)+
  geom_point()


com_sales %>% 
  group_by(BOROUGH,BLOCK,LOT,BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
  count() %>% 
  filter(n>200) %>% 
  ggplot()+
  aes(x = BUILDING.CLASS.AT.TIME.OF.SALE, y = n)+
  geom_col()


# pluto data --------------------------------------------------------------

pluto_lean <- read_rds("data/pluto_lean.rds")


pluto_lean$Address %>% tail(100)
addy <- "^3[0-9] WEST 27 STREET"
pluto_lean %>% filter(str_detect(Address,addy)) %>% glimpse()


pluto_lean %>% filter(Address=="31 WEST 27 STREET") %>% glimpse()










