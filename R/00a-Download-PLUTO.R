
library(tidyverse)

file_list<-
c('http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_02a.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_03c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_04c.zip'
,'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_05d.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_06c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_07c.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_09v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_09v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_10v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_10v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_11v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_11v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_12v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_12v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_13v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_13v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_14v1.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_14v2.zip'
,'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_15v1.zip'
,'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_16v2%20.zip'
)



# automatically creates a sub directory within specified path
if(!"data"%in%dir()){
  dir.create("data")
}

if(!"PLUTO_ARCHIVES"%in%dir('data')){
  dir.create("data/PLUTO_ARCHIVES")
}

file_names <- gsub(".zip","",basename(file_list))
while(sum(dir("PLUTO_ARCHIVES")%in%file_names)<length(file_list)){
  message("\nStarting from top of while loop...");message(Sys.time())
  file_list_short = file_list[!gsub(".zip","",basename(file_list))%in%dir("PLUTO_ARCHIVES")]
  
  if (length(file_list_short)==0) break
  
  for(file in file_list_short){
    message("Working on ",file)
    fil <- file
    dest_file <- paste0('PLUTO_ARCHIVES/',basename(gsub(".zip","",fil)))
    
    if(exists(dest_file)){
      file.remove(dest_file)
    }
    
    temp <- tempfile()
    download.file(fil,temp)
    unzip(temp, exdir = paste0('PLUTO_ARCHIVES/',basename(gsub(".zip","",fil))))
    unlink(temp)
  }
  
}


# create a list of all file names to loop over
names_list<-list()
for(file in dir("PLUTO_ARCHIVES")){
  names_string <- dir(paste0('PLUTO_ARCHIVES/',file))
  names_string <- names_string[-grep(".pdf",names_string)]
  names_list[[file]] <- names_string
}

names_list_df <- as_data_frame(names_list, stringsAsFactors = F)
# remove deprrecated versions:
names_list_df <- names_list_df[!names(names_list_df)%in%c("nyc_pluto_02a","nyc_pluto_09v1","nyc_pluto_10v1","nyc_pluto_11v1"
                                                          ,"nyc_pluto_12v1","nyc_pluto_13v1","nyc_pluto_14v1")]


# we're indexing the data first to determine which files have variable names
out_list <- list()
for (j in 1:length(names_list_df)){
  cat("\n###outer loop",j)
  dol_df <- names_list_df %>% select(j)
  for(nm in 1:nrow(dol_df)){
    cat("\n......inner loop",nm)
    out_idx <- paste0(j,nm)
    the_name <- names(dol_df)
    the_file <- paste0("PLUTO_ARCHIVES/",the_name,"/",dol_df[nm,])
    readin <- suppressWarnings(suppressMessages(read_csv(the_file, col_names = F, n_max = 1000, progress = F)))
    out_list[[out_idx]] <- readin
  }
}


# setting col classes to most recent version of PLUTO (adding a few missing ones)
nms <- names(out_list)
col_class_df <- read_csv("PLUTO_ARCHIVES/nyc_pluto_16v2%20/SI.csv", n_max = 10000)
col_class_spec <- spec(col_class_df)
col_class_spec$cols$MaxAllwFAR <- col_character()
col_class_spec$cols$CB2000 <- col_character()


all_years <- rep(2003:2016, each = 5)
all_years <- all_years[-which(grepl("2008",all_years))]

out_list_fin <- list()
for (j in length(names_list_df):1){
  message("\n### outer loop ",length(names_list_df)-j+1," of ",length(names_list_df)," at ", Sys.time())
  dol_df <- names_list_df %>% select(j)
  for(nm in nrow(dol_df):1){
    message("\n......inner loop ",nrow(dol_df)-nm+1," of ",nrow(dol_df), " at ", Sys.time())
    out_idx <- paste0(j,nm)
    the_name <- names(dol_df)
    the_file <- paste0("PLUTO_ARCHIVES/",the_name,"/",dol_df[nm,])
    readin <- suppressWarnings(suppressMessages(read_csv(the_file, col_names = T, progress = F, col_types = col_class_spec)))
    readin$file <- the_file
    readin$Year <- tail(all_years,1)
    all_years <- head(all_years,length(all_years)-1)
    out_list_fin[[out_idx]] <- readin
  }
}

final_pluto_df <- bind_rows(out_list_fin)
final_pluto_df$Year %>% table()


# add building ID and lat/lon
ID_Building_Type <- function(x){
  dat<-x
  dat<-dplyr::mutate(dat,Building_Type=substr(BldgClass,1,1))
}

Convert_XY <- function(x) {
  library(proj4)
  # NEW YORK LONG ISLAND STATE PLANE PROJECTION
  # see: http://spatialreference.org/ref/esri/102718/
  proj4string <- 
    "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 
  +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
  xy <- data.frame(x=x$XCoord, y=x$YCoord)
  pj <- proj4::project(xy, proj4string, inverse=TRUE)
  latlon <- data.frame(lat=pj$y, lon=pj$x)
  x <- x %>% 
    mutate(lat = latlon$lat
           ,lon = latlon$lon)
}

final_pluto_df <- final_pluto_df %>% mutate(Building_Type = substr(BldgClass,1,1))
final_pluto_df <- Convert_XY(final_pluto_df)



# Write to disk -----------------------------------------------------------
write_csv(final_pluto_df,"pluto_all.csv")
write_rds(final_pluto_df,"pluto_all_compressed.rds", compress = "gz")

pluto_lean <- 
  final_pluto_df %>% 
  select('Year', 'Borough', 'Block', 'Lot', 'ZipCode'
         , 'Address'
         , 'BldgClass', 'Easements', 'OwnerType', 'OwnerName', 'LotArea'
         , 'BldgArea', 'ComArea', 'ResArea', 'OfficeArea', 'RetailArea'
         , 'GarageArea', 'StrgeArea', 'FactryArea', 'OtherArea'
         , 'NumBldgs', 'NumFloors', 'UnitsRes', 'UnitsTotal'
         , 'LotFront', 'LotDepth', 'BldgFront', 'BldgDepth', 'Ext', 'ProxCode'
         , 'IrrLotCode', 'LotType', 'BsmtCode', 'AssessLand', 'AssessTot'
         , 'ExemptLand', 'ExemptTot', 'YearBuilt', 'YearAlter1'
         , 'YearAlter2', 'BuiltFAR', 'ResidFAR'
         , 'CommFAR', 'FacilFAR', 'BoroCode', 'BBL', 'CondoNo'
         , 'XCoord', 'YCoord', 'file', 'MaxAllwFAR', 'AssessTotal'
         , 'ExemptTotal', 'CornerLot', 'FAR', 'Building_Type', 'lat', 'lon'
         )


write_rds(pluto_lean,"pluto_lean_compressed_2003_2016.rds", compress = "gz")








