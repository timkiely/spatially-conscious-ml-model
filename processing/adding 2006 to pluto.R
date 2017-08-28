


library(tidyverse)
library(stringr)
library(sf)

dir("data/PLUTO_ARCHIVES/nyc_pluto_06c")

MN <- read_csv("PLUTO_ARCHIVES/nyc_pluto_06c/MN06C.TXT")
my_cols <- spec(MN)
SI <- read_csv("PLUTO_ARCHIVES/nyc_pluto_06c/SI06C.TXT", col_types = my_cols)
BK <- read_csv("PLUTO_ARCHIVES/nyc_pluto_06c/BK06C.TXT", col_types = my_cols)
BX <- read_csv("PLUTO_ARCHIVES/nyc_pluto_06c/BX06C.TXT", col_types = my_cols)
QN <- read_csv("PLUTO_ARCHIVES/nyc_pluto_06c/QN06C.TXT", col_types = my_cols)




all_2006 <- bind_rows(list(MN,QN,BX,BK,SI))


# add a few variables
all_2006$file_version <- '2006'
all_2006$Year <- '2006'
all_2006 <-
  all_2006 %>% 
  mutate(BOROUGH = ifelse(Borough=="MN",1,0)
         ,BOROUGH = ifelse(Borough=="BX",2,BOROUGH)
         ,BOROUGH = ifelse(Borough=="BK",3,BOROUGH)
         ,BOROUGH = ifelse(Borough=="QN",4,BOROUGH)
         ,BOROUGH = ifelse(Borough=="SI",5,BOROUGH)
         )

all_2006 <-
  all_2006 %>% 
  mutate(BBL_derive = paste0(BOROUGH
                             ,sprintf("%05.0f", Block)
                             ,sprintf("%04.0f", Lot)
                             )
         )
         
         


# Convert the lat/long from NY State Plan projection to UTM
Convert_XY <- function(x) {
  library(proj4)
  # NEW YORK LONG ISLAND STATE PLANE PROJECTION
  # see: http://spatialreference.org/ref/esri/102718/
  proj4string <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
  xy <- data.frame(x=x$XCoord, y=x$YCoord)
  pj <- proj4::project(xy, proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  x <- x %>% 
    mutate(lat = latlon$lat
           ,lon = latlon$lon)
  
}

all_2006 <- Convert_XY(all_2006)

# What is the building type?
ID_Building_Type <- function(x){
  dat<-x
  dat<-dplyr::mutate(dat,Building_Type=substr(BldgClass,1,1))
}

all_2006 <- ID_Building_Type(all_2006)



# convert col classes -----------------------------------------------------

all_2006 <-
  all_2006 %>% 
  # character columns
  mutate_at(.funs = as.character
            , .vars = vars(Block, Lot, CD, CT2000, CB2000, ZipCode
                           , HealthArea, HealthCtr,BoroCode
                           ,BBL,TaxMap,PLUTOMapID)
            ) %>% 
  # numeric columns
  mutate_at(.funs = as.numeric
            , .vars = vars(PolicePrct,LandUse
                           ,LotArea:BldgDepth
                           ,AssessLand:YearAlter2
                           ,BuiltFAR,MaxAllwFAR
                           ,CondoNo,XCoord,YCoord)
  ) %>% 
  # date columns
  mutate_at(.funs = function(date) as.Date(paste0(date,"/01"),format = "%M/%Y/%d")
            , .vars = vars(RPADDate:PoliDate)
  ) %>% 
  
  # rename columns
  rename('AssessTotal' = AssessTot
         ,'ExemptTotal' = ExemptTot)

# drop columns
all_2006 <- all_2006 %>% select(-InstRegion,-BOROUGH)


# Combine with PLUTO ------------------------------------------------------

pluto <- tbl_df(read_rds("data/pluto-combined/pluto_all.rds"))

pluto2 <- bind_rows(pluto,all_2006)

# write to compressed R file
write_rds(pluto2, "data/pluto-combined/pluto_all.rds", compress = c("gz"))

