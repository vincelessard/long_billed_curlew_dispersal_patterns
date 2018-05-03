#I don't remember what package you need (probably only mapview), but you can
#load them all just to be sure

library(sf)
library(sp)
library(rgeos)
library(rgdal)
library(raster)
library(mapview)
library(spocc)
library(scrubr)

## Write the path to yor data
Danaus_plexippus = read.csv("YOUR DATA", header=T, row=1)

# Clean data to remove missing coordinates
cleaned = coord_incomplete(Danaus_plexippus, lat="latitude", lon="longitude")

# Use long and lat to map the occurrences and tell the function what information to take
# "longitude" and "latitude" refer to the name of the corresponding columns
# "name", "prov", etc. are the name of the columns containing the info you want to keep
occ_transf <- SpatialPointsDataFrame(
 cleaned[c('longitude','latitude')],
  data = cleaned[c('name', 'prov','date','key')],
  proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))

#View mapped data
mapview(occ_transf)

#kwjbvouwvb
