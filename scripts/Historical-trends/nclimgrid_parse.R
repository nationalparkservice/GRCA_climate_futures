library(sf)
library(sp)
library(dplyr)
library(raster)
library(ggplot2)
library(grid)

library(zoo)

rm(list = ls())

data.dir <- "E:/nClimGrid" 
OutDir <- "C:/Users/achildress/Documents/NOAA-data/"

sites = c("GRCA-NE","GRCA-SE","GRCA-SW")
lats = c(36.27,36,10,36.27)
lons = c(-112.09,-112.09,-113.09)

start.time=Sys.time()

for (s in 1:length(sites)){

site = sites[s]
Lat = lats[s]
Lon = lons[s]

Buffer <- 0.05    # dec degree.  
AoaExt <- extent(Lon-Buffer, Lon+Buffer, Lat-Buffer, Lat+Buffer)

var.list = c("tmax", "tmin", "tave", "prcp")
for (v in 1:length(var.list)){
DF <- data.frame()
var = var.list[v]
print(paste0("extracting ",var))
pnt.list<-list.files(path=data.dir, pattern=paste(var, "conus", sep = ".")) #list all files by var
#tmax, tmin, tave, prcp

# Create list of tables

for(i in 1:length(pnt.list)){
  t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
  yrmon = substr(pnt.list[i], 1, 6) 
  colnames(t) = c("Lat","Lon", var)
  tt = subset(t, Lat >= AoaExt@ymin & Lat <= AoaExt@ymax &
                Lon >=AoaExt@xmin & Lon<=AoaExt@xmax)
  tt$Date = as.Date(paste0(yrmon,"01"),format="%Y%m%d")
  tt.merge = aggregate(.~Date,tt,mean)
  DF = rbind(DF,tt.merge)
}
assign(paste0(var,".data"), DF)
# write.csv(DF,paste0(OutDir,var,"_",site,".csv"),row.names = F)
}
save.image(sprintf("%s%s_%s_%s_nClimGrid_IntermediateFiles.RData", OutDir,site, Lat, Lon))
}

end.time = Sys.time()
run.time = end.time-start.time
run.time

