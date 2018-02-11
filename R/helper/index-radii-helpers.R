


# Final notes: This was an interesting exercise, but 
# my intuition ultimately failed. Recursively partitioning 
# spatial data into smaller clusters fails because after a few
# iterations, the borders of the partitions become too small
# (around when the diameter of the cluster convex approaches the 
# radius of the buffers). An interesting alternative would 
# be to do random, evenly spaced medoid placement?

# how about using st_voroni in some way?


# Note the ClusterR package has CLARA-kmedoids for "large applications" as an alternative
library(sf)
library(cluster)
library(dplyr)
library(purrr)
library(magrittr)
# 0) create data 
pts <- data.frame(id = 1:200, x = runif(200, 0, 100),y = runif(200, 0, 100))
x <- sf::st_as_sf(pts, coords = c("x", "y"), remove = F)
x$buffer_geom <- st_buffer(x$geometry, dist = 5) # NOTE: switch the active column with st_set_geometry("buffer_geom")
outer_border <- x %>% st_combine() %>% st_convex_hull() %>%  st_cast(to = "MULTILINESTRING")

# 1) first partition
x$Success <- FALSE
x$clust <- NA
plot(x$geometry)




num_partitions <- 5 #base::sample(1:5,1)

n <- 10
for(i in 1:n){
  plot(x$geometry, col = as.numeric(x$Success)+2, add = T)
  x_successes <- x %>% filter(Success==TRUE)
  x_failures <- x %>% filter(Success==FALSE)
  message("iteration ",i," of ", n)
  message("        Successes: ",nrow(x_successes))
  message("         Failuers: ",nrow(x_failures))
  
  
  x_failures$clust <- as.factor(pam(st_coordinates(x_failures), k = num_partitions, metric = "euclidean", cluster.only = TRUE, do.swap = FALSE, pamonce = 2))
  
  # visualize the partitions: 
  plot(x_failures$geometry, col = as.numeric(x_failures$Success)+2, add = T)
  x_failures %>% group_by(clust) %>% tally() %>% st_convex_hull() %>% select(geometry) %>% plot(add = T)
  plot(outer_border, add = T, lty = 2)
  
  # 2) split by partition
  x_split <- split(x_failures, x_failures$clust)
  
  out_list <- list()
  for(ii in 1:length(x_split)){
    # split <- x_split[[1]]
    # 3) Get neighbor list
    #message("Working on ",ii,"...")
    split <- x_split[[ii]]
    get_neighbor_list <- function(split){
      buffer <- split$buffer_geom
      points_within_buffer <- st_intersects(buffer, split)
      points_without_self <- map(1:length(points_within_buffer), function(x) points_within_buffer[[x]][-match(x, points_within_buffer[[x]])])
      points_without_self
    }
    
    neighbor_list <- get_neighbor_list(split)
    
    # 4) filter failures
    # split <- x_split[[2]]
    detect_if_fully_contained <- function(split){
      buffer <- split$buffer_geom
      convex_hull <- split %>% group_by(clust) %>% st_combine() %>% st_convex_hull() %>% st_cast(to = "MULTILINESTRING")
      hull_intersects <- st_intersects(buffer, convex_hull) 
      hull_intersects_all <- st_intersects(buffer, outer_border) 
      success_vector <- lengths(hull_intersects)==0 | lengths(hull_intersects_all)==1
      split %<>% mutate(Success = success_vector)
      split
    }
    
    split <- detect_if_fully_contained(split)
    out <- list("split" = as_tibble(split), "neighbor_list" = tibble(neighbor_list))
    out_list[[ii]] <- out
  }
  
  partially_indexed <- 
    out_list %>% 
    map(~.x$split) %>% 
    map(~.x %>% select(-geometry, -buffer_geom)) %>% 
    bind_rows() %>% 
    arrange(id)
  
  x_failures$Success <- partially_indexed$Success
  
  new_frame <- rbind(x_successes, x_failures) %>% arrange(id)
  x$Success <- new_frame$Success
  Sys.sleep(2)
}

 










