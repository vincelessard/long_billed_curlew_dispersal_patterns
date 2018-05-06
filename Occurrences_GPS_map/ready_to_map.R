# ready_to_map function to map points

require(ggplot2)
require(ggthemes)
pd <- position_dodge(.1)
# Function to map spatial points with a caption and color on a world map, limiting the display
# to the extent of the supplied dataframe

ready_to_map_points <- function(data, color, caption){
  extent <- data %>% select(x = longitude, y = latitude) %>% raster::extent()
  base_layer <- ggplot(data = data, aes(x = longitude, y = latitude)) +
    geom_point(size=4, color = color, shape = ".")+
    coord_equal()+
    theme_map()
  mapdata <- base_layer+
    geom_path(data = ggplot2::map_data("world"), aes(x = long, y =lat, group=group))+
    scale_x_continuous(limits = c(extent@xmin, extent@xmax))+
    scale_y_continuous(limits = c(extent@ymin, extent@ymax))+
    coord_fixed(1.3)+  
    labs(x=NULL, y=NULL, 
         title="Occurrences of long-billed curlew",
         subtitle=NULL,
         caption= caption)
  return(mapdata)
}

# Function to map hull polygon, provides the ggplot layer
source("make_hull_poly.R")
ready_to_map_hull <- function(data, hulldata, color){
  onland <- make_hull_poly(data, hulldata)
  onland_layer <- geom_polygon(data = fortify(onland), aes(x = long, y = lat, group = piece), 
                               fill=color, alpha=0.5,
                               size = 1.25)
  return(onland_layer)
}