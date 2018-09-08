
library(tidyverse)
library(scales)

if(!"p05_pluto_with_sales"%in%ls()){
  p05_pluto_with_sales <- readRDS("C:/Users/tkiely/Dropbox/MSPA/Thesis/Analysis/full-data/p05_pluto_with_sales.rds")
}

glimpse(p05_pluto_with_sales)

# key dimensions: Year, Boro, Class
# key measures: N, Number Sales, Av Sale, Av Sale $/SF

# by year 
p05_pluto_with_sales %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  group_by(Year) %>% 
  summarise(N = comma(n())
            , `# Sales` = comma(sum(Sold))
            , `Median Sale` = dollar(median(`TOTAL_SALES`, na.rm = T))
            , `Median Sale $/SF` = dollar(median(sale_psf, na.rm = T))
  ) %>% 
  write_csv("Writing/Sections/tables and figures/eda_by_year.csv")

# by boro
p05_pluto_with_sales %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  filter(!Borough%in%c("\u001a","SS")) %>% 
  group_by(Borough) %>% 
  summarise(N = comma(n())
            , `# Sales` = comma(sum(Sold))
            , `Median Sale` = dollar(median(`TOTAL_SALES`, na.rm = T))
            , `Median Sale $/SF` = dollar(median(sale_psf, na.rm = T))
  ) %>%
  write_csv("Writing/Sections/tables and figures/eda_by_boro.csv")


# by build class
build_code <- 
  tribble(~Code, ~`Build Type`
          , "A", "One Family Dwellings"
          , "B", "Two Family Dwellings"
          , "C", "Walk Up Apartments"
          , "D", "Elevator Apartments"
          , "E", "Warehouses"
          , "F", "Factory"
          , "G", "Garages"
          , "H", "Hotels"
          , "I", "Hospitals"
          , "J", "Theatres"
          , "K", "Retail"
          , "L", "Loft"
          , "M", "Religious"
          , "N", "Asylum"
          , "O", "Office"
          , "P", "Public Assembly"
          , "Q", "Recreation"
          , "R", "Condo"
          , "S", "Mixed Use Residence"
          , "T", "Transportation"
          , "U", "Utility"
          , "V", "Vacant"
          , "W", "Educational"
          , "Y", "Gov't"
          , "Z", "Misc")


p05_pluto_with_sales %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  filter(!Building_Type%in%c("0")) %>% 
  group_by(Building_Type) %>% 
  summarise(N = comma(n())
            , `# Sales` = comma(sum(Sold))
            , `Median Sale` = dollar(median(`TOTAL_SALES`, na.rm = T))
            , `Median Sale $/SF` = dollar(median(sale_psf, na.rm = T))
  ) %>% left_join(build_code, by = c("Building_Type"= "Code")) %>% 
  select("Bldg Code" = Building_Type, `Build Type`, everything()) %>%
  write_csv("Writing/Sections/tables and figures/eda_by_class.csv")


# boro-class, filtered

p05_pluto_with_sales %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  filter(!Borough%in%c("\u001a","SS")) %>% 
  filter(Building_Type %in% c("A", "B", "C", "D", "F", "G", "L")) %>% 
  group_by(Borough, Building_Type) %>% 
  summarise(`# Sales` = comma(sum(Sold))
            , `Median Sale` = dollar(median(`TOTAL_SALES`, na.rm = T))
            , `Median Sale $/SF` = dollar(median(sale_psf, na.rm = T))
  ) %>% left_join(build_code, by = c("Building_Type"= "Code")) %>% 
  select(Borough, `Build Type`, `# Sales`) %>%
  spread(Borough, `# Sales`) %>% 
  write_csv("Writing/Sections/tables and figures/eda_by_bt_by_boro_num_sales.csv")


p05_pluto_with_sales %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  filter(!Borough%in%c("\u001a","SS")) %>% 
  filter(Building_Type %in% c("A", "B", "C", "D", "F", "G", "L")) %>% 
  group_by(Borough, Building_Type) %>% 
  summarise(`# Sales` = comma(sum(Sold))
            , `Median Sale` = dollar(median(`TOTAL_SALES`, na.rm = T))
            , `Median Sale $/SF` = dollar(median(sale_psf, na.rm = T))
  ) %>% left_join(build_code, by = c("Building_Type"= "Code")) %>% 
  select(Borough, `Build Type`, `Median Sale $/SF`) %>%
  spread(Borough, `Median Sale $/SF`) %>% 
  write_csv("Writing/Sections/tables and figures/eda_by_bt_by_boro_median_sales.csv")

# Appendix table of all variables

library(pastecs)


p05_pluto_with_sales %>% 
  dplyr::filter(Year==2017) %>% 
  head(1000) %>% 
  mutate(sale_psf = TOTAL_SALES/BldgArea
         , sale_psf = ifelse(!is.finite(sale_psf),NA,sale_psf)) %>% 
  select_if(is.numeric) %>% 
  
  
  
  stat.desc(basic=T, desc=F, norm=F) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Variable")







