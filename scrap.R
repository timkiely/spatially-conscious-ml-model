
# for testing things out

source("R/helper/load-packages.R")
source("R/helper/source-files.R")

# pluto1 <- read_rds("data/processing steps/p01_pluto_raw.rds")
# pad1 <- read_rds("data/processing steps/p02_pad_raw.rds")
# sales1 <- read_rds("data/processing steps/p03_sales_raw.rds")
# sales2 <- read_rds("data/processing steps/p04_sales_and_pad.rds")
# pluto2 <- read_rds("data/processing steps/p05_pluto_with_sales.rds")

# base1 <- read_rds("data/processing steps/p06_base_model_data.rds")
# "data/processing steps/p07_zipcode_model_data.rds"
# radii <- read_rds("data/processing steps/p08_radii_model_data.rds")

# "data/processing steps/p09_prob_of_sale_model_base.rds"
# "data/processing steps/p10_prob_of_sale_model_zipcode.rds"
# "data/processing steps/p11_prob_of_sale_model_radii.rds"

# "data/processing steps/p12_sale_price_model_base.rds"
# "data/processing steps/p13_sale_price_model_zipcode.rds"
# "data/processing steps/p14_sale_price_model_radii.rds"

# "data/processing steps/p15_prob_model_evaluations.rds"
# "data/processing steps/p16_sales_model_evaluations.rds"


base1 <- read_rds("data/processing steps/p06_base_model_data.rds")


base1 %>% glimpse()
base1 %>% summary()






# testing new spatial neighbors find function -----------------------------

source("R/helper/get-spatial-neighbor-points.R")
base1 <- readRDS("data/processing steps/p06_base_model_data.rds")
library(tidyverse)
system.time({
  base2 <- 
    base1 %>% filter(Year==2017) %>% 
    distinct(bbl, .keep_all = T) %>% 
    st_as_sf(coords = c("lon","lat"), na.fail=F, crs = 4326) %>% 
    st_transform(crs = 32618) %>% 
    get_spatial_neighbor_points(id_col = "bbl",
                                max_distance = 500, 
                                n_cuts = 3, 
                                allow_parralell = T, 
                                num_clusters = 9)
})

neigbs <- base2 %>% head(1) %>% select(bbl, geometry, neighbors)
neighbors <- base2 %>% filter(bbl%in%neigbs$neighbors[[1]])
neigbs$lag <- neighbors %>% st_set_geometry(NULL) %>% summarise(mean(EMA_Price_5_year)) %>% as.numeric()

boros <- read_sf("http://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nybb/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson")

boros %>% 
  filter(BoroCode==1) %>% 
  st_transform(32618) %>% 
  st_simplify() %>% 
  select(geometry) %>% 
  plot()

plot(neighbors$geometry, add = T)
plot(neigbs$geometry, add = T, col = "red")
















