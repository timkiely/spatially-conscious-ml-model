
# This function downloads all of NYC PLUTO data

download_nyc_pluto <- function(save_file = "data/processing steps/p01_pluto_raw.rds"){
  
  pluto_archive_dir <- "data/aux data/PLUTO_ARCHIVES"
  
  # manually adjust the file list:
  file_list<-
    c('http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_03c.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_04c.zip'
      , 'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_05d.zip'
      
      ## NOTE: 2006 file was corrupted. Corrected manually and uploaded fixed file to S3 (public bucket)
      #, 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_06c.zip'
      , 'https://s3-us-west-2.amazonaws.com/pluto.data/corrected-nyc_pluto_06c.zip'
      
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_07c.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_09v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_10v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_11v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_12v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_13v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_14v2.zip'
      , 'http://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_15v1.zip'
      , "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_16v1.zip"
      , 'https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_17v1.zip'
    )
  
  
  file_names <- gsub(".zip","",basename(file_list))
  files_not_downloaded <- file_names[!file_names %in% dir(pluto_archive_dir)]
  
  while(length(files_not_downloaded)>0){
    
    message("Downloading links: Starting from top of while loop at ",Sys.time())
    file_list_short = file_list[!file_names%in%dir(pluto_archive_dir)]
    
    if (length(file_list_short)==0) break
    
    for(file in file_list_short){
      message("Working on ",file)
      fil <- file
      dest_file <- paste0(pluto_archive_dir,"/",basename(gsub(".zip","",fil)))
      
      if(exists(dest_file)){
        file.remove(dest_file)
      }
      
      temp <- tempfile()
      download.file(fil, temp)
      unzip(temp, exdir = "data/aux data/PLUTO_ARCHIVES/")
      unlink(temp)
    }
    
  }
  
  
  # create a list of all file names to loop over
  names_list <- list()
  q_file.copy <- quietly(file.copy)
  for(.file in dir(pluto_archive_dir)){
    names_string <- dir(paste0(pluto_archive_dir,"/",.file))
    names_string <- names_string[!grepl("__MACOSX",names_string)]
    
    if(length(names_string)==1){
      paste0(pluto_archive_dir,"/",.file,"/",names_string,"/",dir(paste0(pluto_archive_dir,"/",.file,"/",names_string))) %>% 
        map(.f = function(x){
          shhhh <- q_file.copy(
            from = x
            , to = gsub(paste0(names_string,"/"),"",x)
            , overwrite = TRUE
          )
        })
      unlink(paste0(pluto_archive_dir,"/",.file,"/",names_string), recursive = TRUE)
      names_string <- dir(paste0(pluto_archive_dir,"/",.file))
    }
    names_string <- names_string[-grep(".pdf",names_string)]
    names_list[[.file]] <- names_string
  }
  
  names_list_df <- as_data_frame(names_list, stringsAsFactors = F)
  
  # we're indexing the data first to determine which files have variable names
  out_list <- list()
  for (j in 1:length(names_list_df)){
    message("### Indexing PLUTO Data: outer loop", j," of ",length(names_list_df))
    dol_df <- names_list_df %>% select(j)
    for(nm in 1:nrow(dol_df)){
      message("......inner loop ",nm, " of ",nrow(dol_df))
      out_idx <- paste0(j,nm)
      the_name <- names(dol_df)
      the_file <- paste0(pluto_archive_dir,"/",the_name,"/",dol_df[nm,])
      readin <- suppressWarnings(suppressMessages(read_csv(the_file, col_names = F, n_max = 1000, progress = F)))
      out_list[[out_idx]] <- readin
    }
  }
  
  
  # setting col classes to most recent version of PLUTO (adding a few missing ones)
  nms <- names(out_list)
  col_class_df <- suppressWarnings(suppressMessages(read_csv("data/aux data/PLUTO_ARCHIVES/nyc_pluto_17v1/MN2017V1.csv", n_max = 10000, progress = FALSE)))
  col_class_spec <- spec(col_class_df)
  col_class_spec$cols$MaxAllwFAR <- col_character()
  col_class_spec$cols$CB2000 <- col_character()
  
  
  all_years <- rep(2003:2017, each = 5)
  all_years <- all_years[-which(grepl("2008",all_years))]
  
  out_list_fin <- list()
  for (j in length(names_list_df):1){
    message("### Normalizing PLUTO names: outer loop ",length(names_list_df)-j+1," of ",length(names_list_df))
    dol_df <- names_list_df %>% select(j)
    for(nm in nrow(dol_df):1){
      message("......inner loop ",nrow(dol_df)-nm+1," of ",nrow(dol_df))
      out_idx <- paste0(j,nm)
      the_name <- names(dol_df)
      the_file <- paste0("data/aux data/PLUTO_ARCHIVES/",the_name,"/",dol_df[nm,])
      readin <- suppressWarnings(suppressMessages(read_csv(the_file, col_names = T, progress = F, col_types = col_class_spec)))
      readin$file <- the_file
      readin$Year <- tail(all_years,1)
      all_years <- head(all_years,length(all_years)-1)
      out_list_fin[[out_idx]] <- readin
    }
  }
  
  
  message("Binding PLUTO rows...")
  final_pluto_df <- bind_rows(out_list_fin)
  
  # add building ID and lat/lon
  Convert_XY <- function(data) {
    library(proj4)
    # NEW YORK LONG ISLAND STATE PLANE PROJECTION
    # see: http://spatialreference.org/ref/esri/102718/
    proj4string <- 
      "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 
    +lon_0=-74 +x_0=300000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
    xy <- matrix(c(data$XCoord, data$YCoord), byrow = F, ncol = 2)
    pj <- proj4::project(xy, proj4string, inverse=TRUE)
    data <- data %>% mutate(lat = pj[,2],lon = pj[,1])
  }
  
  message("Adding building class ID to PLUTO...")
  final_pluto_df <- final_pluto_df %>% mutate(Building_Type = substr(BldgClass,1,1))
  message("Converting PLUTO coordinates to lat/lon...")
  final_pluto_df <- Convert_XY(final_pluto_df)
  
  
  
  # Write to disk -----------------------------------------------------------
  message("Writing PLUTO to disk 1 of 2...")
  write_rds(final_pluto_df,"data/aux data/pluto_all_compressed.rds", compress = "gz")
  
  pluto_lean <- 
    final_pluto_df %>% 
    select('Year', 'Borough', 'Block', 'Lot', 'ZipCode'
           , 'Address'
           , 'BldgClass', 'Easements', 'OwnerType', 'OwnerName', 'LotArea'
           , 'BldgArea', 'ComArea', 'ResArea', 'OfficeArea', 'RetailArea'
           , "ZoneDist1", "ZoneDist2"
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
  
  message("Writing PLUTO to disk 2 of 2...")
  write_rds(pluto_lean, save_file, compress = "gz")
  
  message("pluto lean compressed written to ", save_file)
}

