# Project Long-billed curlew distribution and land use #####
require(plyr)
require(dplyr)
require(tidyr)
require(rworldxtra)
require(leaflet)
require(dbscan)

windowsFonts(Times=windowsFont("TT Times New Roman"))
setwd("C:/Users/Morgane/Documents/SBL workshop/Project/map")

source("create_ahull.R")
source("ready_to_map.R")
source("make_hull_poly.R")

# 1. Read files ####
occdata <- read.table("Occurrences_data_f.csv",sep=";", header=T)
gpsdata <- read.table("GPS_data_map.csv",sep=";", header=T)

# 2. Create the alpha shape around the points ####
occ_ahull <- create_ahull(occdata, 3)
gps_ahull <- create_ahull(gpsdata, 3)

# 3. Map the points and the alpha shape ####
caption_occ <- "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04"
caption_gps <- "MoveBank"

occurrences_map <- ready_to_map_points(occdata, "grey20", caption_occ)+
  ready_to_map_hull(occdata, occ_ahull,"grey70")
gps_map <- ready_to_map_points(gpsdata, "forestgreen", caption_gps)+
  ready_to_map_hull(occdata, occ_ahull,"chartreuse3")

# 4. Map both datasets and hulls together ####
base_layer <- ggplot() +
  geom_point(data = occdata, aes(x = longitude, y = latitude),
             size=0.5, color = "grey20", pch=20)+
  geom_point(data = gpsdata, aes(x = longitude, y = latitude),
             size=0.5, color = "forestgreen", pch=20)+
  coord_equal()+
  theme_map()+
  theme(text = element_text(family="Times"))
extent <- occdata %>% select(x = longitude, y = latitude) %>% raster::extent()
mapdata <- base_layer+
  geom_path(data = ggplot2::map_data("world"), aes(x = long, y =lat, group=group))+
  scale_x_continuous(limits = c(extent@xmin, extent@xmax))+
  scale_y_continuous(limits = c(extent@ymin, extent@ymax))+
  coord_fixed(1.3)+  
  labs(x=NULL, y=NULL, 
       title="Occurrences of long-billed curlew",
       subtitle=NULL,
       caption= "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04 \n MoveBank")+
  theme(text = element_text(family="Times"))

final_occ_graph <- mapdata + ready_to_map_hull(occdata, occ_ahull, "grey70") +
  ready_to_map_hull(gpsdata, gps_ahull, "chartreuse3")

# 5. Calculate the area overlap #####

# 5.1 Plot the intersection polygon ####

onland.gps <- make_hull_poly(gpsdata, gps_ahull)
onland.occ <- make_hull_poly(occdata, occ_ahull)

gps_inters_occ <- raster::intersect(onland.occ,onland.gps)
layer_inters <- geom_polygon(data = fortify(gps_inters_occ), aes(x = long, y = lat, group = piece), 
                                 fill="deepskyblue3", alpha=0.5,
                                 size = 1.25)
final_intersect_graph <- mapdata + layer_inters

# 5.2 Calculate and annotate plot with percent overlap ####

full_range_area <- sapply(slot(onland.occ, "polygons"), slot, "area")
intersect_area <- sapply(slot(gps_inters_occ, "polygons"), slot, "area")
percentcover <- intersect_area/full_range_area
# 41.86492%

interm_graph <- mapdata + ready_to_map_hull(occdata, occ_ahull, "grey70") +
  ready_to_map_hull(gpsdata, gps_ahull, "chartreuse3")
final_graph <- interm_graph+  annotate("text",x=-120,y=20,label="41.87% overlap", family="Times",size=4)

tiff("longbilled_curlew_rangemap_nonclustered.tif", width=150,height=120,units="mm",res=300)
final_graph
dev.off()


# 6. Restrict overlap to dense occurrences areas ####

# 6.1 Hierarchical density-based clustering ####

# cluster points, minimum 500 points in a cluster, large step (eps value)
dens<-dbscan(occdata,MinPts=500,eps=5)
nonoccdata <- cbind(occdata, dens$cluster)
colnames(nonoccdata) <- c("longitude", "latitude", "cluster")
nonoccdata$cluster <- as.factor(nonoccdata$cluster)
nonocc_ahull <- create_ahull(nonoccdata[nonoccdata$cluster==1,], 3)

# 6.2 Map the clustered data, intersection and annotate overlap percentage ####
base_layer <- ggplot() +
  geom_point(data = nonoccdata, aes(x = longitude, y = latitude, color=cluster),
             size=0.5, pch=20)+
  geom_point(data = gpsdata, aes(x = longitude, y = latitude),
             size=0.5, color = "forestgreen", pch=20)+
  scale_color_manual(values = c("firebrick3","grey20"),
                     labels=c("occasional","clustered"),
                     guide = guide_legend(title="Occurrence data clustered",
                                          override.aes = list(size=3)))+
  coord_equal()+
  theme_map()+
  theme(text = element_text(family="Times"),
        legend.position=c(0.1, 0.02),
        legend.background = element_blank())
extent <- nonoccdata %>% select(x = longitude, y = latitude) %>% raster::extent()
mapdata <- base_layer+
  geom_path(data = ggplot2::map_data("world"), aes(x = long, y =lat, group=group))+
  scale_x_continuous(limits = c(extent@xmin, extent@xmax))+
  scale_y_continuous(limits = c(extent@ymin, extent@ymax))+
  coord_fixed(1.3)+  
  labs(x=NULL, y=NULL, 
       title="Occurrences of long-billed curlew",
       subtitle=NULL,
       caption= "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04 \n MoveBank")+
  theme(text = element_text(family="Times"))

full_range_area <- sapply(slot(onland.nonocc, "polygons"), slot, "area")
intersect_area <- sapply(slot(gps_inters_occ, "polygons"), slot, "area")
percentcover <- intersect_area/full_range_area
# 56.26897%

interm_graph <- mapdata + ready_to_map_hull(nonoccdata[nonoccdata$cluster==1,], nonocc_ahull, "grey70") +
  ready_to_map_hull(gpsdata, gps_ahull, "chartreuse3")
final_graph <- interm_graph+  annotate("text",x=-120,y=20,label="56.27% overlap", family="Times", size=4)

tiff("longbilled_curlew_rangemap.tif", width=150,height=120,units="mm",res=300)
final_graph
dev.off()

# z. Leaflet map ####
leaflet(data = occdata) %>% addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, color = "red", radius = 2,
                   clusterOptions = markerClusterOptions(),
                   label = ~sprintf("lng = %.2f, lat = %.2f", longitude, latitude))

# z. interactive map for occurrences ####
require(speciesgeocodeR)
require(raster)
require(proj4)
require(tidyverse)
library(rgdal)
proj <- "+proj=longlat +datum=WGS84"
occpt <- sp::SpatialPointsDataFrame(coords = polygondata[, c("longitude", "latitude")], data = polygondata, 
                                    proj4string = CRS(proj), match.ID = TRUE)

mapview::mapview(occpt, cex = 4, fill=as.numeric(polygondata$time))@map





