
# for testing things out



# pluto1 <- read_rds("data/processing steps/p01_pluto_raw.rds")
# pad1 <- read_rds("data/processing steps/p02_pad_raw.rds")
# sales1 <- read_rds("data/processing steps/p03_sales_raw.rds")
# sales2 <- read_rds("data/processing steps/p04_sales_and_pad.rds")
# pluto2 <- read_rds("data/processing steps/p05_pluto_with_sales.rds")

# base1 <- read_rds("data/processing steps/p06_base_model_data.rds")
# zip_level <- read_rds("data/processing steps/p07_zipcode_model_data.rds")
# radii <- read_rds("data/processing steps/p08_radii_model_data.rds")

# "data/processing steps/p09_prob_of_sale_model_base.rds"
# "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
# "data/processing steps/p11_prob_of_sale_model_radii.rds"

# "data/processing steps/p12_sale_price_model_base.rds"
# "data/processing steps/p13_sale_price_model_zipcode.rds"
# "data/processing steps/p14_sale_price_model_radii.rds"

# "data/processing steps/p15_prob_model_evaluations.rds"
# "data/processing steps/p16_sales_model_evaluations.rds"

source("R/helper/load-packages.R")
source("R/helper/source-files.R")

# base1 <- read_rds("data/processing steps/p06_base_model_data.rds")
# base_samp <- base1 %>% filter(BoroCode==1)
# write_rds(base_samp, "data/aux data/sample_p06_base_model_data.rds")
base1 <- read_rds("data/aux data/sample_p06_base_model_data.rds")

data_frame(`a` = names(summary(base1$`SALE PRICE`))) %>% 
  bind_cols(data_frame(`Sale Price per Square Foot` = scales::comma(round(summary(base1$`SALE PRICE`),2)))) %>% 
  filter(a!="NA's") %>% 
  rename(` ` = a) %>% 
  write_rds("Writing/Sections/tables and figures/sale_price_summary_table4.rds")

min1 <- 
  radii %>% 
  select(Radius_Total_Sold_In_Year:Percent_Change_EMA_5_basic_mean_perc_change) %>% 
  summarise_all(min, na.rm = T) %>% 
  transpose() %>% 
  rename("Min" = V1)

median1 <- 
  radii %>% 
  select(Radius_Total_Sold_In_Year:Percent_Change_EMA_5_basic_mean_perc_change) %>% 
  summarise_all(median, na.rm = T) %>% 
  transpose() %>% 
  rename("Median" = V1)

  mean1 <- 
    radii %>% 
    select(Radius_Total_Sold_In_Year:Percent_Change_EMA_5_basic_mean_perc_change) %>% 
  summarise_all(mean, na.rm = T) %>% 
transpose() %>% 
  rename("Mean" = V1)

max1 <- 
  radii %>% 
  select(Radius_Total_Sold_In_Year:Percent_Change_EMA_5_basic_mean_perc_change) %>% 
  summarise_all(max, na.rm = T) %>% 
  transpose() %>% 
  rename("Max" = V1)

table_1 <- 
  data_frame(Feature = names(select(radii, Radius_Total_Sold_In_Year:Percent_Change_EMA_5_basic_mean_perc_change))) %>% 
  bind_cols(min1) %>% 
  bind_cols(median1) %>% 
  bind_cols(mean1) %>% 
  bind_cols(max1) %>% 
  mutate_at(vars(Min:Max), round, 2) %>% 
  mutate_at(vars(Min:Max), scales::comma)

write_rds(table_1, "Writing/Sections/tables and figures/table3.rds")


