library(tidyverse)

# Combine BBLE
pluto <- tbl_df(read_rds("data/pluto-combined/pluto_all.rds"))


pluto_new <-
  pluto %>%
  mutate(Year = as.integer(Year)) %>%
  mutate(BOROUGH = ifelse(Borough=="MN",1,0)
         ,BOROUGH = ifelse(Borough=="BX",2,BOROUGH)
         ,BOROUGH = ifelse(Borough=="BK",3,BOROUGH)
         ,BOROUGH = ifelse(Borough=="QN",4,BOROUGH)
         ,BOROUGH = ifelse(Borough=="SI",5,BOROUGH)
         ,BOROUGH = as.integer(BOROUGH)
  )

# manually select variables
pluto_lean <- select(pluto_new, BOROUGH, Block, Lot, Address, lat, lon, ZipCode
                     , Year, BldgClass, Building_Type, BBL_derive, OwnerType, OwnerName
                     , LotArea, BldgArea, ComArea, ResArea, NumBldgs, NumFloors, UnitsRes, UnitsTotal
                     , LotFront, LotDepth, BldgFront, BldgDepth, ProxCode, IrrLotCode, CornerLot
                     , AssessLand, AssessTotal, ExemptLand, ExemptTotal, YearBuilt, YearAlter1, YearAlter2
                     , OfficeArea, RetailArea, GarageArea, StrgeArea, FactryArea, OtherArea, LotType, BsmtCode
                     , BuiltFAR, BBL, CondoNo, BBL_derive)

write_rds(pluto_lean,"data/pluto_lean.rds", compress = "gz")