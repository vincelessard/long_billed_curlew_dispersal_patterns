# make_hull_poly function

require(sp)
require(rgdal)
require(rworldmap)

# Make polygon for alpha shaped concave hull providing the datapoints and hull data. Returns
# a spatial polygon
make_hull_poly <- function(data, hulldata){
  ahull.poly <- Polygon(data[hulldata, c("longitude", "latitude")]) %>% 
    list %>% Polygons(ID=1) %>%
    list %>% SpatialPolygons()
  proj4string(ahull.poly) <- CRS("+init=epsg:4326")
  world_map <- getMap(resolution = "high") %>% subset(continent = "North America")
  world_map <- sp::spTransform(world_map, proj4string(ahull.poly))
  onland.poly <- raster::intersect(world_map, ahull.poly)
  lps <- getSpPPolygonsLabptSlots(onland.poly) #deprecated, should switch to new one
  IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest = TRUE)
  onland <- maptools::unionSpatialPolygons(onland.poly, IDOneBin)
  return(onland)
}