# Project Long-billed curlew distribution and land use #####
require(scales)
require(spocc)
require(rinat)
require(scrubr)
require(plyr)
require(dplyr)
require(tidyr)
require(rworldxtra)
require(leaflet)
windowsFonts(Times=windowsFont("TT Times New Roman"))
setwd("C:/Users/Morgane/Documents/SBL workshop/Project/map")

source("create_ahull.R")
source("ready_to_map.R")
source("make_hull_poly.R")

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

# 4.1 reduce the data set to presence absence to draw the polygons ####
occdata <- curlewdata_america %>%
  select(longitude, latitude)%>%
  distinct
gpsdata <- gps %>%
  select(longitude, latitude)%>%
  distinct()
write.table(occdata, paste("Occurrences_data_f",".csv",sep=""), sep=";", na="", row.names=F)
write.table(gpsdata, paste("GPS_data_map",".csv",sep=""), sep=";", na="", row.names=F)

occdata <- read.table("Occurrences_data_f.csv",sep=";", header=T)
gpsdata <- read.table("GPS_data_map.csv",sep=";", header=T)

# 4.2 Create the alpha shape around the points ####
occ_ahull <- create_ahull(occdata, 3)
gps_ahull <- create_ahull(gpsdata, 3)

# 4.3 Map the points and the alpha shape ####
caption_occ <- "https://doi.org/10.15468/dl.nmoa7l \n https://bison.usgs.gov, 2018-05-04"
caption_gps <- "MoveBank"

occurrences_map <- ready_to_map_points(occdata, "firebrick3", caption_occ)+
  ready_to_map_hull(occdata, occ_ahull,"darkorange")
gps_map <- ready_to_map_points(gpsdata, "forestgreen", caption_gps)+
  ready_to_map_hull(occdata, occ_ahull,"darkolivegreen2")

# 4.4 Map both datasets and hulls together ####
base_layer <- ggplot() +
  geom_point(data = occdata, aes(x = longitude, y = latitude),
             size=4, color = "firebrick3", shape = ".")+
  geom_point(data = gpsdata, aes(x = longitude, y = latitude),
             size=4, color = "forestgreen", shape = ".")+
  coord_equal()+
  theme_map()
extent <- occdata %>% select(x = longitude, y = latitude) %>% raster::extent()
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

# 5. Calculate the area overlap #####
onland.gps <- make_hull_poly(gpsdata, gps_ahull)
onland.occ <- make_hull_poly(occdata, occ_ahull)

gps_inters_occ <- raster::intersect(onland.occ,onland.gps)
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

# 6. Restrict overlap to dense occurrences areas ####

mc <- apply(occdata, 2, mean)
as.numeric(mc[1])
# standard distance
sd <- sqrt(sum((occdata[,1] - mc[1])^2 + (occdata[,2] - mc[2])^2) / nrow(occdata))
final_graph + annotate("point",x=as.numeric(mc[1]), y=as.numeric(mc[2]), col="blue", size=10)


library("dbscan")
# density-based clustering
x <- as.matrix(occdata)
db <- dbscan(x, eps = 5, minPts = 500)
db
pairs(x, col = db$cluster + 1L)
lof <- lof(x, k = 500)
pairs(x, cex = lof)

city <- readRDS('city.rds')
CityArea <- area(city)
ab <- map_data("world")
extent <- occdata %>% select(x = longitude, y = latitude) %>% raster::extent()

cd <- ab %>%
  select(lat, long, region)%>%
  filter(lat >= extent@ymin & lat <= extent@ymax & long >= extent@xmin & long <= extent@xmax)
coordinates(cd)<-c("long", "lat")
proj4string(cd) <- CRS("+init=epsg:4326")
r <- raster(cd)
res(r) <- 0.5
r
ra <- rasterize(cd, r)
plot(ra)
quads <- as(ra, 'SpatialPolygons')
proj4string(quads) <- CRS("+init=epsg:4326")
plot(quads, add=TRUE)
points(occdata, col='red', cex=.5)
nc <- rasterize(coordinates(occdata), r, fun='count', background=0)
nc_spdf <- as(nc, "SpatialPixelsDataFrame")
nc_spdf <- as.data.frame(nc_spdf)
colnames(nc_spdf) <- c("value", "x", "y")
proj4string(nc_spdf) <- CRS("+init=epsg:4326")
world_map <- getMap(resolution = "high") %>% subset(continent = "North America")
world_map <- sp::spTransform(world_map, proj4string(nc_spdf))
onland.poly <- raster::intersect(world_map, nc_spdf)
lps <- getSpPPolygonsLabptSlots(onland.poly)
IDOneBin <- cut(lps[,1], range(lps[,1]), include.lowest = TRUE)
onland <- maptools::unionSpatialPolygons(onland.poly, IDOneBin)
onland_layer <- geom_polygon(data = fortify(onland), aes(x = long, y = lat, group = piece), 
                             fill=color, alpha=0.5,
                             size = 1.25)
require(viridis)
ggplot() +  
  geom_tile(data=nc_spdf, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_path(data = ggplot2::map_data("world"), aes(x = long, y =lat, group=group))+
  scale_x_continuous(limits = c(extent@xmin, extent@xmax))+
  scale_y_continuous(limits = c(extent@ymin, extent@ymax))+
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"))
plot(nc)
plot(cd, add=TRUE, cex=0.1)


# Leaflet map
leaflet(data = occdata) %>% addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, color = "red", radius = 2,
                   clusterOptions = markerClusterOptions(),
                   label = ~sprintf("lng = %.2f, lat = %.2f", longitude, latitude))

require(geosphere)


# Build hulls based on point density
require(dbscan)
dbscan(occdata)



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





