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
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}



setwd("C:/Users/Morgane/Documents/SBL workshop")

# retrieve data from gbif
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
write.table(gbif,paste("gbiflongbilledcurlew",".txt",sep=""),sep="\t",na="",row.names=F)
