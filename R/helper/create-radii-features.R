create_radii_features <- function(pluto_model, radii_index) {
  
  # pluto_model_bak <- pluto_model
  # pluto_model <- pluto_model %>% filter(Borough=="MN")
  
  pb <- progress::progress_bar$new(total = length(radii_index))
  opts <- list(progress = function(n) pb$tick(token = list("current" = n)))
  
  message("Preparing radii feature loop...")
  
  message("Starting radii feature engineering at ", Sys.time())
  run_start <- Sys.time()
  
  num_cores <- parallel::detectCores()-2
  cl <- makeSOCKcluster(num_cores)
  registerDoSNOW(cl)
  on.exit(closeAllConnections())
  on.exit(stopImplicitCluster())
  
  radii_features <- foreach(jj = 1:length(radii_index)
                        , .verbose = FALSE
                        , .errorhandling = "pass"
                        , .options.snow = opts ) %dopar% {
                          
                          pb$tick()
                          
                          pacman::p_load(sf, dplyr, magrittr)
                          
                          item_interest <- radii_index_loop %>% filter(Origin_Key==all_keys[jj])
                          key <- as.character(unique(item_interest[1, "Origin_Key"]))
                          bbls <- distinct(item_interest, bbl)
                          
                          all_options <- inner_join(bbls, pluto_model, by = "bbl")
                          
                          identity <- function(x) mean(x) # for naming purposes, will be removed
                          radii_average <- function(x) mean(x, na.rm  = T) # for naming purposes, a simple average
                          
                          features <- 
                            all_options %>% 
                            select(-Block, -Lot, -Easements, -BoroCode, -NumBldgs, -ProxCode) %>% 
                            group_by(Year) %>% 
                            summarise_at(.vars = vars(Last_Sale_Price:Percent_Change_EMA_5), .funs = funs(identity, radii_average)) %>% 
                            select(Year,contains("radii_average")) %>% 
                            left_join({
                            all_options %>%
                            group_by(Year) %>% 
                            summarise(Last_Year_radii_Sold = sum(Sold, na.rm = T)) %>% 
                            mutate(Last_Year_radii_Sold = lag(Last_Year_radii_Sold, 1)
                                   , Last_Year_radii_Sold_Percent_Ch = (Last_Year_radii_Sold - lag(Last_Year_radii_Sold,1))/lag(Last_Year_radii_Sold,1))
                            }, by = c("Year"))
                          
                          if(nrow(features)>0|length(features)>1) {
                            features$Origin <- key
                          } else next
                          
                          
                          return(features)
                        }; closeAllConnections(); stopImplicitCluster()
  

  
  end_time <- Sys.time()
  tot_time <- end_time-run_start
  
  message("     ...Finished RADII feature creation at ", Sys.time())
  message("     ...Total feature creation time: ", round(tot_time,2),units(tot_time))
  
  message("Writing intermediate features to disk...")
  write_rds(radii_features, "data/aux data/radii-features-step1.rds")
  
  # ensure no error in the list:
  message("Discarding error messages and binding together...")
  radii_features <- radii_features %>% keep(.p = function(x) "data.frame"%in%class(x))
  all_feats <- bind_rows(radii_features)
  all_feats2 <- distinct(all_feats, Origin, Year)

  message("Merging with original data...")
  pluto_model <- left_join(pluto_model, all_feats2, by = c("bbl"="Origin","Year"))
  message("     ...done")
  pluto_model
}

