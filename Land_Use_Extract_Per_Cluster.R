library(raster)
setwd("/Users/jaymelewthwaite/Documents/Masters/DDESSS_2018/Project/long_billed_curlew_dispersal_patterns")

#30 m resolution land use data: too big of a file!
#land_use_raster=raster("NA_NALCMS_LC_30m_LAEA_mmu12_urb05.tif")

#250 m land use data
land_use_raster=raster("NA_LandCover_2010_25haMMU.tif")
#to get projection of land use data:
proj4string(land_use_raster)

#Before doing anything, we need to decrease the cell size from 250 m to 2.5 km (factor of 10) because the file takes too long to work with! 
#The modal function means that the new larger cell will take the most common value found in the smaller cells that it aggregates
land_use_reduced<-aggregate(land_use_raster, fact=10, fun=modal,na.rm=T)

#need to add projection information to this raster that is missing (the datum and ellipse) in order to reproject. 
#Found this info in the metadata for the file
crs(land_use_reduced) <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +datum=WGS84 +ellps=WGS84"

#define the new projection (newproj) that we want to reproject the data into to match the GPS and GBIF data
newproj <- "+proj=longlat +datum=WGS84 +ellps=WGS84"

#reproject the raster. Because re-projecting a raster requires a certain amount of warping of the cells and by necessity 
#interpolation between the values. If we want to keep the same values we have to change the method of interpolation, so I made it nearest neighbour
land_use_reduced_proj<-projectRaster(land_use_reduced, crs=newproj, method="ngb")

#get a look at the final land use map
plot(land_use_reduced_proj)

#to write the raster to a file:
land_use_raster_write<-writeRaster(land_use_raster,filename="land_use_raster.grd")


#load cluster gps data
gps_data_clust<-read.csv("GPS_clust_cent.csv")
coordinates(gps_data_clust) <- c("longitude", "latitude")

#have to specify raster::extract to show that it is the extract function from the raster package (and not dplyr)
land_use_per_observation<-function(gps_data){
  land_cover<-raster::extract(land_use_reduced_proj, gps_data)
  data.frame<-data.frame(coordinates(gps_data),land_cover, gps_data)
  return(data.frame)
}

land_use_extract<-land_use_per_observation(gps_data_clust)

#get the total number of land use types by each bird AND by cluster
library(dplyr)  
summary_land_use_per_bird <- land_use_extract %>%
  group_by(bird_id,clust_val) %>%
  summarise(n_land_cover= length(unique(land_cover)))

#get all the list of unique values of landcover types per bird AND by cluster
all_land_use_types_per_bird <- land_use_extract %>%
  group_by(bird_id,clust_val) %>%
  summarise(land_cover_types = paste(unique(land_cover),collapse = ","))
#separate all landcover values into separate columns
all_land_use_types_per_bird<-separate(all_land_use_types_per_bird,col=land_cover_types, sep=",",into=c("lc_1","lc_2","lc_3","lc_4","lc_5","lc_6","lc_7","lc_8","lc_9","lc_10","lc_11","lc_12","lc_13","lc_14","lc_15","lc_16","lc_17","lc_18","lc_19"))

join_clust<-all_land_use_types_per_bird %>%
  left_join(summary_land_use_per_bird, by=c("bird_id","clust_val"))
head(join_clust)

write.table(join_clust, "summary_land_use_per_bird_per_clust.csv", sep=",")
