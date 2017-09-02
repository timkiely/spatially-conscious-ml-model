 
rm(list=ls())
library(tidyverse)
library(sf)
library(stringr)
library(modelr)



# sales data ------------------------------------------------------------------
if(!exists("sale_augmented")){
  sale_augmented <- read_rds("data/sales_augmented.rds")
}

# function to quickly glimpse BBL
lookat <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt) %>% arrange(desc(SALE_DATE)) %>% glimpse()
lookhead <- function(boro= 1,blck = 829,lt = 16) sale_augmented %>% filter(BOROUGH == boro, BLOCK == blck, LOT == lt)


# a look at 31 West 27th St:
sale_augmented %>% filter(BOROUGH == 1, BLOCK == 829, LOT == 16) %>% 
  select(ADDRESS,SALE_DATE, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal)


# does sale price correlate with Assessed Value?
sale_augmented %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio)) %>% 
  filter(AssessTotal<500000000) %>%
  ggplot()+
  aes(x = SALE.PRICE, y=AssessTotal, group = Building_Type, color = Building_Type)+
  geom_point()+
  geom_smooth(se=F, method = "lm")+
  scale_y_continuous(labels=scales::comma)


# apply linear model over grouped dataframes:
group_model <- function(df, formula = "AssessTotal~SALE.PRICE") {
  formula <- as.formula(formula)
  lm(formula, data = df)
}

extract_coef <- function(model, coef = "SALE.PRICE") coef(model)[coef]

# the most linearly model-able property types appear to be Lofts (office), Office, 
# two-family dwellings and elevator apartments
by_group<-
sale_augmented %>% 
  filter(SALE.PRICE>0) %>% 
  select(ADDRESS,SALE_DATE,Building_Type, SALE.PRICE, AssessTotal) %>% 
  mutate(SalePriceToAssesstmentRatio = SALE.PRICE/AssessTotal) %>% 
  filter(!is.na(SalePriceToAssesstmentRatio),is.finite(SalePriceToAssesstmentRatio)) %>% 
  mutate(AssessSD = sd(AssessTotal), AssessZscore = scale(AssessTotal)) %>% 
  #filter(abs(AssessZscore)<3) %>% 
  group_by(Building_Type) %>% 
  nest() %>% 
  mutate(Number_of_Sales = map_dbl(data,nrow)) %>% 
  mutate(model = map(data, group_model, formula = "AssessTotal ~ SALE.PRICE + SalePriceToAssesstmentRatio")) %>% 
  mutate(Sale_Coef = map_dbl(model,extract_coef, coef = "SALE.PRICE")) %>% 
  mutate(SalePriceToAssesstmentRatio = map_dbl(model,extract_coef, coef = "SalePriceToAssesstmentRatio")) %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance) %>% 
  mutate(summary = map(model, summary)) %>% 
  arrange(-adj.r.squared) %>% 
  filter(p.value <= (0.05))
    
    
# how many sales per year, and how many years do we have?
sale_augmented$SALE_YEAR %>% table()

# how many times do certain BBL's appear in the data?
sale_augmented %>% 
  group_by(BOROUGH,BLOCK,LOT) %>% 
  count() %>% 
  arrange(-n) %>% 
  filter(n>1, n<2000)










