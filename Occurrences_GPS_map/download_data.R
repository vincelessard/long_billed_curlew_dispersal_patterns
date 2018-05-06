# Download, clean the data for occurrences and GPS points for the long-billed curlew in 
# North America

require(spocc)
require(scrubr)
require(plyr)
require(dplyr)
require(tidyr)


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
