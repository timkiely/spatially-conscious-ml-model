

get_spatial_neighbor_points <- function(data,
                                        id_col = NULL, 
                                        max_distance = NULL,
                                        n_cuts = 1,
                                        allow_parralell = FALSE, 
                                        num_clusters = 1
                                        ) {
  message(paste(
    ""
    , paste0("Running spatial indexing on ", scales::comma(nrow(data)), " points")
    , paste0("ID column: ", id_col)
    , paste0("Max distance: ", max_distance, " ", str_extract(st_crs(data)$proj4string, "units=."))
    , paste0("Number of partions: ", n_cuts, " by ", n_cuts, ", ",n_cuts^2, " total data partitions")
    , paste0("Parallel: ", allow_parralell, " with number of clusters set to ", num_clusters)
    , ""
    , sep = "\n"
  ))
  
  on.exit(closeAllConnections())
  if(sum(class(data) %in% c('sf','sfc'))==0) stop("data must be of class 'sf' or 'sfc")
  library(doParallel)
  library(foreach)
  library(data.table)
  library(sf)
  library(tibble)
  
  # set aside the input data for re-merging at the end
  the_crs <- st_crs(data)
  pure_data <- data.table(data)
  pure_data$x <- st_coordinates(data)[1,]
  pure_data$y <- st_coordinates(data)[2,]
  pure_data[,key_id := pure_data[,..id_col]]
  
  # construct a data.table from the sf object with x, y, geometry, neighbor-key, and an ordering column
  geom <-  st_geometry(data)
  pts <- data.table(st_coordinates(geom))
  pts[ , geometry := geom]
  names(pts)[names(pts) == "X"] = "x"
  names(pts)[names(pts) == "Y"] = "y"
  pts[, key_id := st_set_geometry(data, NULL)[,id_col]]
  pts[, order_id := 1:dim(pts)[1]]
  
  # divide the extent into n_cuts*n_cuts quadrants and assign each
  # point to a quadrant
  range_x  <- range(pts$x, na.rm = T)
  limits_x <- (range_x[1] + (0:n_cuts)*(range_x[2] - range_x[1])/n_cuts)
  range_y  <- range(pts$y, na.rm = T)
  limits_y <- range_y[1] + (0:n_cuts)*(range_y[2] - range_y[1])/n_cuts
  
  # take the entire data and assign grid locations for the 'inner data'
  # if n_cuts cuts>1, divide data into n_cuts x n_cuts grid
  if(n_cuts>1){
    pts[, `:=`(x_grid_assignment =  as.integer(cut(x, n_cuts, labels = 1:n_cuts)),
               y_grid_assignment = as.integer(cut(y, n_cuts, labels = 1:n_cuts)))] %>%
      setkey(x_grid_assignment, y_grid_assignment)
  } else {
    pts[, `:=` (x_grid_assignment = 1, y_grid_assignment = 1)] %>% setkey(x_grid_assignment, y_grid_assignment)
  }
  
  # run in parallel or in sequence
  if(allow_parralell == TRUE & !is.null(num_clusters)){
    cl <- parallel::makeCluster(num_clusters)
    doParallel::registerDoParallel(cl)
  } else {
    registerDoSEQ()
  }
  
  
  # all possible x,y combinations to loop over
  xy_cuts <- expand.grid("x_cut" = seq_len(n_cuts), "y_cut" = seq_len(n_cuts))
  
  # start cycling over quadrants
  out <- foreach(each_cut = 1:nrow(xy_cuts)
                 , .packages = c("sf", "data.table")) %dopar% {
                   
                   count <- each_cut
                   cut_x <- xy_cuts[each_cut,"x_cut"]
                   cut_y <- xy_cuts[each_cut,"y_cut"]
                   
                   # first, subset for an 'outer data' set along the x-axis
                   min_x_outer_points    <- ifelse(cut_x == 1, limits_x[cut_x], (limits_x[cut_x] - max_distance))
                   max_x_outer_points    <- ifelse(cut_x == n_cuts,
                                                   limits_x[cut_x + 1],
                                                   (limits_x[cut_x + 1] + max_distance))
                   
                   subset_outer_points_along_x <- 
                     pts[x >= min_x_outer_points & x <= max_x_outer_points] %>%
                     setkey(y)
                   
                   # now subset again along the y-axis to get the final 'outer data'
                   min_y_outer_points  <- ifelse(cut_y == 1
                                                 , limits_y[cut_y]
                                                 , (limits_y[cut_y] - max_distance))
                   
                   max_y_outer_points  <- ifelse(cut_y == n_cuts
                                                 , limits_y[cut_y + 1]
                                                 , (limits_y[cut_y + 1] + max_distance))
                   
                   subset_outer_points_along_x_and_y <- subset_outer_points_along_x[y >= min_y_outer_points & y <= max_y_outer_points]
                   
                   # filter down to just the 'inner data' by using the grid assignments from earlier
                   inner_points <- subset_outer_points_along_x_and_y[y_grid_assignment == cut_y & x_grid_assignment == cut_x]
                   
                   if(nrow(inner_points)==0){
                     return(NULL)
                   }
                   
                   inner_points_buffered <- 
                     inner_points %>% 
                     sf::st_as_sf(coords = c("x","y"), remove = FALSE, na.fail = F) %>%
                     st_buffer(max_distance)
                   
                   # retransform to sf since data.tables lost the geometric attrributes
                   outer_points <- sf::st_as_sf(subset_outer_points_along_x_and_y, coords = c("x","y"))
                   
                   # compute the intersection and save results in a element of "results".
                   inters <- sf::st_intersects(inner_points_buffered, outer_points)
                   
                   # convert local positions to the user-supplied id_col
                   inters_key_id <- lapply(inters, FUN = function(x) outer_points$key_id[x])
                   
                   # save results
                   result <- 
                     data.table(
                       order = inner_points_buffered$order_id
                       , key_id = inner_points_buffered$key_id
                       , neighbors = inters_key_id
                     )
                   return(result)
                 }
  
  if(allow_parralell == TRUE & !is.null(num_clusters)) stopCluster(cl)
  
  ordered_out <- data.table::rbindlist(out)
  ordered_out <- ordered_out[order(ordered_out[,order]),]
  
  # merge back to original data and remove processing columns
  final_data <- pure_data[ordered_out, on = "key_id"]
  final_data[ , `:=` (key_id = NULL, x = NULL, y = NULL, order = NULL)]
  
  # finally, convert to sf and tibble (tibble has better printing for list-columns)
  final_sf <- st_as_sf(as_tibble(final_data), sf_column_name = "geometry", crs = the_crs)
  return(final_sf)
}



















