# Project Long-billed curlew distribution and land use #####
library('scales')
require(spocc)
require(rinat)
require(scrubr)
require(plyr)
require(dplyr)
require(tidyr)
require(ggplot2)
library("sp")
library("rgdal")
library(alphahull)
library(igraph)
require(ggthemes)
require(RgoogleMaps)
require(rworldmap)
require(rworldxtra)
require(leaflet)
windowsFonts(Times=windowsFont("TT Times New Roman"))
pd <- position_dodge(.1)

setwd("C:/Users/Morgane/Documents/SBL workshop/Project")

# 1. Download the occurrences data ####
# download the data from GBIF and BISON databases
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = 1700, width = 300)
gbif <- NULL
for (i in c(1:1700)){
  dat <- occ2df(occ(query = "Numenius americanus", from=c("gbif"), limit=100, start=((i-1)*100)+1))
  gbif <- rbind(gbif, dat)
  setWinProgressBar(pb, i, title=paste( round(i/1700*100, 0),
                                        "% done"))
}
close(pb)
write.table(gbif, paste("gbif_Numenius_americanusf",".csv",sep=""),sep=";", na="", row.names=F)


pb <- winProgressBar(title = "progress bar", min = 0,
                     max = 1314, width = 300)
bison <- NULL
for (i in c(1:1314)){
  dat <- occ2df(occ(query = "Numenius americanus", from=c("bison"), limit=100, start=((i-1)*100)+1))
  bison <- rbind(bison, dat)
  setWinProgressBar(pb, i, title=paste( round(i/1314*100, 0),
                                        "% done"))
}
close(pb)
write.table(gbif, paste("gbif_Numenius_americanus",".csv",sep=""),sep=";", na="", row.names=F)
write.table(bison, paste("bison_Numenius_americanus",".csv",sep=""),sep=";", na="", row.names=F)

gbif <- read.table("gbif_Numenius_americanus.csv", sep=";", header=T)
bison <- read.table("bison_Numenius_americanus.csv", sep=";", header=T)

# 2. Merge the two data frames and clean #####
# remove the missing dates, impossible coordinates, incomplete and unlikely coordinates, extract year from date
curlew <- rbind(gbif, bison)
curlew$date <- as.character(curlew$date)
curlewc <- curlew %>%
  date_missing()%>%
  coord_impossible()%>%
  coord_incomplete()%>%
  coord_unlikely()%>%
  select(name, longitude, latitude, date, key) %>%
  distinct()%>%
  date_standardize("%Y-%m-%d")%>%
  separate(date, c("year","month","day"))

write.table(curlewc, paste("gbif_bison_Numenius_americanus",".csv",sep=""),sep=";", na="", row.names=F)
curlewdata <- read.table("gbif_bison_Numenius_americanus.csv",sep=";", header=T)
# filter for years 2013 to 2018
curlew1318 <- curlewc %>%
  filter(year==2013|year==2014|year==2015|year==2016|year==2017|year==2018)
write.table(curlew1318, paste("gbif_bison_Numenius_americanus20132018",".csv",sep=""),sep=";", na="", row.names=F)
curlewdata1318 <- read.table("gbif_bison_Numenius_americanus20132018.csv",sep=";", header=T)

# 3. Load the migration data points ####
gps <- read.table("all_data.csv",sep=",",header=T)

# 4. Make the occurrences map ####
# all data restrict to america
lat_lim <- c(0, 60)
lon_lim <- c(-51, -147)
curlewdata_america <- curlewdata %>%
  filter(latitude >= 0 & latitude <= 60 & longitude <= -51 & longitude >=-151)

# reduce the data set to presence absence to draw the polygons
occdata <- curlewdata_america %>%
  select(longitude, latitude)%>%
  distinct
gpsdata <- gps %>%
  select(longitude, latitude)%>%
  distinct()

create_ahull <- function(data, alpha){
  ex_ashape = ashape(data, alpha = alpha)# Computation And Order Alpha Shape
  ex_mat = ex_ashape$edges[, c("ind1", "ind2")]# Take the coordinates of points on the edges of the alpha shape
  class(ex_mat) = "character"# Convert 'numeric' matrix to 'character' matrix, to avoid wrong node orders 
  ex_graph = graph.edgelist(ex_mat, directed = F)# Make the graph of points
  # plot(ex_graph)# Verify its a cyclic graph
  cut_graph = ex_graph - E(ex_graph)[1]  # Cut the first edge
  ends = names(which(degree(cut_graph) == 1))   # Get two nodes with degree = 1
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]# Compute a path
  path_nodes = as.numeric(V(ex_graph)[path]$name)# Get node names (= row numbers in original data.frame)
  return(path_nodes)
}
occ_ahull <- create_ahull(occdata, 3)
gps_ahull <- create_ahull(gpsdata, 3)

extent <- occdata %>% select(x = longitude, y = latitude) %>% raster::extent()

ready_to_map_points <- function(data, color, caption){
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

ready_to_map_hull <- function(data, hulldata, color){
  ahull.poly <- Polygon(data[hulldata, c("longitude", "latitude")]) %>% 
    list %>% Polygons(ID=1) %>%
    list %>% SpatialPolygons()
  proj4string(ahull.poly) <- CRS("+init=epsg:4326")
  world_map <- getMap(resolution = "high") %>% subset(continent = "North America")
  world_map <- sp::spTransform(world_map, proj4string(ahull.poly))
  onland.poly <- raster::intersect(world_map, ahull.poly)
  lps <- getSpPPolygonsLabptSlots(onland.poly)
  IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest = TRUE)
  onland <- maptools::unionSpatialPolygons(onland.poly, IDOneBin)
  onland_layer <- geom_polygon(data = fortify(onland), aes(x = long, y = lat, group = piece), 
                               fill=color, alpha=0.5,
                               size = 1.25)
  return(onland_layer)
}
caption_occ <- "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04"
caption_gps <- "MoveBank"

occurrences_map <- ready_to_map_points(occdata, "firebrick3", caption_occ)+
  ready_to_map_hull(occdata, occ_ahull,"darkorange")
gps_map <- ready_to_map_points(gpsdata, "forestgreen", caption_gps)+
  ready_to_map_hull(occdata, occ_ahull,"darkolivegreen2")

# all together
base_layer <- ggplot() +
  geom_point(data = occdata, aes(x = longitude, y = latitude),
             size=4, color = "firebrick3", shape = ".")+
  geom_point(data = gpsdata, aes(x = longitude, y = latitude),
             size=4, color = "forestgreen", shape = ".")+
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
       caption= "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04 \n MoveBank")

final_occ_graph <- mapdata + ready_to_map_hull(occdata, occ_ahull, "darkorange") +
  ready_to_map_hull(gpsdata, gps_ahull, "darkolivegreen2")

# 5. Calculate the area overlap
gps_inters_occ <- raster::intersect(onland.gps, onland.occ)
layer_inters <- geom_polygon(data = fortify(gps_inters_occ), aes(x = long, y = lat, group = piece), 
                                 fill="deepskyblue3", alpha=0.5,
                                 size = 1.25)
final_intersect_graph <- mapdata + layer_inters

# total area
full_range_area <- sapply(slot(onland.occ, "polygons"), slot, "area")
intersect_area <- sapply(slot(gps_inters_occ, "polygons"), slot, "area")
percentcover <- intersect_area/full_range_area
# 41.86492%
interm_graph <- mapdata + ready_to_map_hull(occdata, occ_ahull, "darkorange") +
  ready_to_map_hull(gpsdata, gps_ahull, "darkolivegreen2")
final_graph <- interm_graph+  annotate("text",x=-120,y=15,label="41.87% overlap")

# Leaflet map
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





