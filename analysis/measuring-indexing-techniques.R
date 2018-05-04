

library(sf)
library(tidyverse)

nn <- c(1000, 3000, 5000, 10000, 15000, 30000, 50000, 80000, 100000,150000)


times <- NULL
for(nn in nn){
  message(nn," points...")
  pts <- data.frame(id = 1:nn, x = runif(nn, 0, 1000),y = runif(nn, 0, 1000))
  pts$neighbors_found <- FALSE
  x <- sf::st_as_sf(pts, coords = c("x", "y"), remove = F)
  x$buffer_geom <- st_buffer(x$geometry, dist = 50) # NOTE: switch the active column with st_set_geometry("buffer_geom")
  outer_border <- bbox_polygon(x) %>% st_cast(to = "MULTILINESTRING")
  
  
  
  grid_method <- function(grid_number = 60){
    # function to make bouding box polygon from set of points:
    bbox_polygon <- function(x) {
      bb <- sf::st_bbox(x)
      p <- matrix(
        c(bb["xmin"], bb["ymin"], 
          bb["xmin"], bb["ymax"],
          bb["xmax"], bb["ymax"], 
          bb["xmax"], bb["ymin"], 
          bb["xmin"], bb["ymin"]),
        ncol = 2, byrow = T
      )
      sf::st_polygon(list(p))
    }
    
    # plot(x$geometry)
    # plot(outer_border, add = T, lty = 2)
    grid_number <- grid_number 
    grid_xcrs <- ceiling(sqrt(grid_number))
    #message("Making a ",grid_xcrs," by ",grid_xcrs," grid...")
    grid <- st_make_grid(outer_border, n = grid_xcrs)
    plot(grid, add = T)
    
    list_out <- list()
    for(i in 1:length(grid)){
      #message(i)
      grid_x <- grid[i]
      #plot(grid_x, add = T, border = "green")
      grid_buf <- st_buffer(grid_x, 50) %>% st_cast(to="MULTILINESTRING")
      #plot(grid_buf, add = T, col = "black", lty = 2) 
      
      min_max_pts <- st_bbox(grid_x)
      min_max_buff <- st_bbox(grid_buf)
      
      pts_main <- x %>% 
        filter(x >= min_max_pts["xmin"]
               , x <= min_max_pts["xmax"]
               , y >= min_max_pts["ymin"]
               , y <= min_max_pts["ymax"] )
      
      pts_buff <- x %>% 
        filter(x >= min_max_buff["xmin"]
               , x <= min_max_buff["xmax"]
               , y >= min_max_buff["ymin"]
               , y <= min_max_buff["ymax"] )
      
      #plot(pts_buff$geometry, add = T, col = "blue")
      #plot(pts_main$geometry, add = T, col = "red")
      
      get_neighbor_list <- function(pts_main){
        buffer <- pts_main$buffer_geom
        points_within_buffer <- st_intersects(buffer, pts_buff)
        points_without_self <- map(1:length(points_within_buffer), function(x) points_within_buffer[[x]][-match(x, points_within_buffer[[x]])])
        points_without_self
      }
      
      neighbor_list <- get_neighbor_list(pts_main)
      x[pts_main$id,"neighbors_found"] <- TRUE
      #plot(x$geometry, col = as.numeric(x$neighbors_found)+2, add = T)
      list_out[[i]] <- tibble(neighbor_list)
    }
  }
  
  
  t1 <- 
    system.time({
      grid_method()
    })
  
  t2 <- system.time({
    all_neigbs <- sf::st_intersects(x$buffer_geom, x)
  })
  
  time_out <- 
    data_frame("points" = nn
               , "grid_method" = as.numeric(round(t1[3],4))
               , "sf_method" = as.numeric(round(t2[3],4))
    )
  
  times <- bind_rows(times, time_out)
  
  message("Grid: ",round(t1[3],2))
  message("sf: ",round(t2[3],2))
  
}


times %>% 
  gather(Method, Time, -points) %>% 
  ggplot()+
  aes(x = points, y = Time, group = Method, color = Method)+
  geom_line(size = 2)+
  geom_point()+
  theme_bw()


