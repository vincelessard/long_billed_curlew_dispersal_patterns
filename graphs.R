library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(mapview)
library(spocc)
library(scrubr)
library(dplyr)
library(doParallel)
library(ggplot2)
library(geosphere)
library(dismo)
library(tidyr)


GPS = read.csv("~/Documents/Maîtrise/E2018/Summer_school/Project/GPS_clust_cent.csv")

#Split dataframe so that every bird is separate from the others
birds = split(GPS,GPS$bird_id)

centroid = birds[[1]] %>% select("clust_val","cent_long", "cent_lat") %>% distinct()
  
# Plot circles


for (i in 66:1){
  xy <- SpatialPointsDataFrame(birds[[i]][2:3], data.frame(ID=seq(1:nrow(birds[[i]]))),
                               proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

  centroid = birds[[i]] %>% select("clust_val","cent_long", "cent_lat") %>% distinct()
  centroid = centroid[c("cent_long", "cent_lat", "clust_val")]
  
  circles <- circles(centroid, d=200000, lonlat=T)
  
plot(circles@polygons, axes=T, main=i)
plot(xy, col=rainbow(nrow(centroid))[birds[[i]]$clust_val], add=T, main=i)
}
