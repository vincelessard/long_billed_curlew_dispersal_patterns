# Project Long-billed curlew distribution and land use #####
library('scales')
require(spocc)
require(rinat)
require(scrubr)
require(dplyr)
require(tidyr)
require(ggplot2)
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
setwd("Project/")
gps <- read.table("all_data.csv",sep=";",header=T)

# 4. Make the occurrences map ####

# recent data
gbif1318
coordinates(gbif1318)<-c("longitude", "latitude")
plot(gbif1318)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(gbif1318) <- crs.geo  # define projection system of our data
plot(gbif1318, pch = 20, col = as.numeric(gbif1318$time))
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)
plot(countriesLow, add = T)
summary(gbif1318)

# all data restrict to america
lat_lim <- c(0, 60)
lon_lim <- c(-51, -147)
curlewdata$time <- NA
curlewdata$year <- as.numeric(as.character(curlewdata$year))
curlewdata$time[curlewdata$year <= 1900] <- "before 1900"
curlewdata$time[curlewdata$year <= 1950 & curlewdata$year >=1900] <- "1900-1950"
curlewdata$time[curlewdata$year > 1950] <- "1951-present"
curlewdata$time <- as.factor(as.character(curlewdata$time))
curlewdata_america <- curlewdata %>%
  filter(latitude >= 0 & latitude <= 60 & longitude <= -51 & longitude >=-151)

library("sp")
library("rgdal")
test <- curlewdata_america
coordinates(curlewdata_america)<-c("longitude", "latitude")
# plot(curlewdata_america)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(curlewdata_america) <- crs.geo  # define projection system of our data
plot(curlewdata_america, pch = 20, col = as.numeric(curlewdata_america$time))
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)
plot(countriesLow, add = T)
summary(curlewdata_america)

coords <- test[c("longitude", "latitude")]

# Making sure we are working with rows that don't have any blanks
coords <- coords[complete.cases(coords),]

# Letting R know that these are specifically spatial coordinates
sp <- SpatialPoints(coords)

coasts <- fortify(coastsCoarse)
coasts <- coasts %>%
  filter(lat >= 0 & lat <= 60 & long <= -51 & long >=-151)

us = map_data("usa")
ca = map_data("world", "Canada")
mex = map_data("world", "Mexico")

gg <- ggplot()+
  geom_tile()+ 
  geom_polygon(data=us, aes(x=long, y=lat, group=group), color = "black", fill="white", alpha=0)+
  geom_polygon(data=ca, aes(x=long, y=lat, group=group), color = "black", fill="white", alpha=0)+
  geom_polygon(data=mex, aes(x=long, y=lat, group=group), color = "black", fill="white", alpha=0)+
  geom_point(data=test, aes(x=longitude, y=latitude, color=time))+
  coord_fixed(1.3)+
  labs(x=NULL, y=NULL, 
       title="Occurrences of long-billed curlew",
       subtitle=NULL,
       caption="https://doi.org/10.15468/dl.nmoa7l")+
  theme(plot.title=element_text(face="bold", family="Times", size=13))+
  theme(plot.caption=element_text(face="bold", family="Times", size=7, color="gray", margin=margin(t=10, r=80)))+
  theme(legend.position="none")+
  theme(axis.line =  element_blank(),
        axis.text =  element_blank(),
        axis.ticks =  element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
gg

set.seed(1)
ch <- chull(curlew1318)
coords <- dat[c(ch, ch[1]), ]  # closed polygon
library("sp")
library("rgdal")





coordinates(locs)<-c("lon", "lat")
plot(locs)
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(locs) <- crs.geo  # define projection system of our data
plot(locs, pch = 20, col = "steelblue")
library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)
table(locs$country)
locs.gb <- subset(locs, locs$country == "United Kingdom")  # select only locs in UK
plot(locs.gb, pch = 20, cex = 2, col = "steelblue")
title("Laurus nobilis occurrences in UK")
plot(countriesLow, add = T)
summary(locs.gb)
set.seed(1)
dat <- matrix(stats::rnorm(2000), ncol = 2)
ch <- chull(dat)
coords <- dat[c(ch, ch[1]), ]  # closed polygon
library("sp")
library("rgdal")

sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
# set coordinate reference system with SpatialPolygons(..., proj4string=CRS(...))
# e.g. CRS("+proj=longlat +datum=WGS84")
sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
writeOGR(sp_poly_df, "chull", layer="chull", driver="ESRI Shapefile")

plot(dat, pch=19)
lines(coords, col="red")
summary(locs)