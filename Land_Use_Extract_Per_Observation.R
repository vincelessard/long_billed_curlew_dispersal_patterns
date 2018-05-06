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


#Load bird gps data
gps_data<-read.csv("all_data.csv")

#map of the two combined
plot(land_use_reduced_proj, xlim = c(-180, 0), ylim = c(0, 90))
points(gps_data$longitude, gps_data$latitude, col = "red", cex = .6)

#define coordinates in the gps data to create a spatial object
coordinates(gps_data) <- c("longitude", "latitude")


#extract land use at each coordinate in the gps data. First we will take a small random subset of the data (50 points)
#gps_data_sample <- gps_data[sample(nrow(gps_data), 50), ]

#old version; turned it into a function
#sample_extract_landuse <- data.frame(coordinates(gps_data_sample),gps_data_sample, raster::extract(land_use_reduced_proj, gps_data_sample))

#have to specify raster::extract to show that it is the extract function from the raster package (and not dplyr)
land_use_per_observation<-function(gps_data){
  land_cover<-raster::extract(land_use_reduced_proj, gps_data)
  data.frame<-data.frame(coordinates(gps_data),land_cover, gps_data)
  return(data.frame)
}

land_use_extract<-land_use_per_observation(gps_data)



#get the total number of land use types by each bird 
library(dplyr)  
summary_land_use_per_bird <- land_use_extract %>%
  group_by(bird_id) %>%
  summarise(n_land_cover= length(unique(land_cover)))

#get all the list of unique values of landcover types per bird
all_land_use_types_per_bird <- land_use_extract %>%
  group_by(bird_id) %>%
  summarise(land_cover_types = paste(unique(land_cover),collapse = ","))
#separate all landcover values into separate columns
all_land_use_types_per_bird<-separate(all_land_use_types_per_bird,col=land_cover_types, sep=",",into=c("lc_1","lc_2","lc_3","lc_4","lc_5","lc_6","lc_7","lc_8","lc_9","lc_10","lc_11","lc_12","lc_13","lc_14","lc_15","lc_16","lc_17","lc_18","lc_19"))

#join the two metrics into one datafrme
join<-summary_land_use_per_bird %>%
  inner_join(all_land_use_types_per_bird, by="bird_id")
join


write.table(join, "summary_land_use_per_bird.csv", sep=",")

#histogram of how many land use types each bird uses

hist_number_land_cover_per_bird<-hist(join$n_land_cover, main="How many land cover types does each bird use?", xlab="# of Land Cover Types", ylab="Frequency",col="cornflowerblue")

land_class_frequency_all_birds <- c("lc_1","lc_2","lc_3","lc_4","lc_5","lc_6","lc_7","lc_8","lc_9","lc_10","lc_11","lc_12","lc_13","lc_14","lc_15","lc_16","lc_17","lc_18","lc_19")
df <- data.frame(cbind(id, var1, var2, var3, var4))


cbind<-cbind(join[3:21])

get_most_common_land_cover<-function(x){
  gather<-gather(join[3:21])
  most_common<-sort(table(gather$value),decreasing=TRUE, na.rm=TRUE)[1:12]
  most_common<-data.frame(most_common)
  colnames(most_common) <- c("land_cover_class", "frequency")
return(most_common)
  }
most_common_land_classes<-get_most_common_land_cover(join)

habitat_types<-read.csv("Habitat_type.csv")
habitat_types$land_cover_number <- as.character(habitat_types$land_cover_number)
most_common_land_classes$land_cover_class<-as.character(most_common_land_classes$land_cover_class)
join2<-inner_join(most_common_land_classes, habitat_types, by=c("land_cover_class" = "land_cover_number"))
factor<-factor(join2$land_cover_name, levels=c("Cropland", "Temperate grassland", "Temperate shrubland","Water","Sub-tropical shrubland","Barren lands","Urban","Temperate forest","Wetland","Temperate deciduous forest","Sub-tropical grassland","Tropical deciduous forest"))
join2$land_cover_name<-as.factor(join2$land_cover_name)

par(mar=c(12,4,4,4))
hist_most_common_land_classes<-plot(x=factor,y=join2$frequency, main="What are the most commonly-used land cover classes?", ylab="Frequency",col="darkcyan",type="p",las=2)



####
points(y=join2$frequency, cex = .5, col = "dark red")
hist_most_common_land_classes<-hist(join2$frequency, main="What are the most commonly-used land cover classes?", ylab="Frequency",col="darkcyan",pch=5)
barplot(table(onderlaag)[,c(4,1,2,3)]