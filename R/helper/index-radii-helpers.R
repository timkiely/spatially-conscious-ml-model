




# Note the ClusterR package has CLARA-kmedoids for "large applications" as an alternative

library(sf)
library(cluster)
library(dplyr)
library(purrr)
# 0) create data 
num_partitions <- 5
pts <- data.frame(id = 1:200, x = runif(200, 0, 100),y = runif(200, 0, 100))
x <- sf::st_as_sf(pts, coords = c("x", "y"), remove = F)
x$buffer_geom <- st_buffer(x$geometry, dist = 5)

# NOTE: switch the active column with st_set_geometry
# x %>% st_set_geometry("buffer_geom")

# 1) first partition
x$clust <- as.factor(pam(pts[,2:3], k = num_partitions, metric = "euclidean", cluster.only = TRUE, do.swap = FALSE, pamonce = 2))
x$Success <- FALSE

# visualize the partitions: 
plot(select(x, clust, geometry))
x %>% group_by(clust) %>% tally() %>% st_convex_hull() %>% 
  select(geometry) %>% plot(add = T)

# 2) split by partition
x_split <- split(x, x$clust)

# 3) Get neighbor list
# split <- x_split[[1]]
get_neighbor_list <- function(split){
  buffer <- split$buffer_geom
  points_witin_buffer <- st_intersects(buffer, split)
  points_without_self <- map(1:length(points_witin_buffer), function(x) points_witin_buffer[[x]][-match(x, points_witin_buffer[[x]])])
  points_without_self
}

get_neighbor_list(x_split[[1]])


# 4) filter failures
# split <- x_split[[1]]
detect_if_fully_contained <- function(split){
  buffer <- split$buffer_geom
  convex_hull <- split %>% group_by(clust) %>% tally() %>% st_convex_hull() %>% st_cast(to = "MULTILINESTRING")
  hull_intersects <- st_intersects(buffer, convex_hull) 
  split$Success <- lengths(hull_intersects)==0
  split
}

detect_if_fully_contained(x_split[[1]])

plot(select(split, geometry, Success), add = F)
plot(split$buffer_geom, add = T)
plot(convex_hull$geometry, add = T)

# 5) re-combine



# 6) partion again using failures

# 7) Repeat process until all points are succesfully calculated











