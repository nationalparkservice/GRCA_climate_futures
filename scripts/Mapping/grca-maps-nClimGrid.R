##############################################
###     MAPS      ############################
##############################################

rm(list = ls())

library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(sf)
library(ggplot2)
library(rasterVis)
library(colorspace)
library(RStoolbox)
library(maptools)
library(RColorBrewer)
library(raster)

site = "GRCA"
aa <- CRS('+init=EPSG:5070') # Conus Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long

PlotIn <- "C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/rasters"
PlotOut <- "C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/maps"

# NPS Boundary

nps_boundary <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\nps_boundary\\nps_boundary.shp") # Directory Garrett comp
park <- dplyr::filter(nps_boundary, UNIT_CODE == site) # subset to GRCA only
#park <- st_transform(park, crs = 5070)
Sp_park <- as_Spatial(park)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:5070") 

GGCL <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp") # greater grand canyon landscape
GGCL <- st_transform(GGCL, crs(park)) # CONUS Albers

Sp_ggcl <- as_Spatial(GGCL)
Sp_ggcl<-spTransform(Sp_ggcl,CRSobj = "+init=epsg:5070") 

# Base map from Natural Earth - https://www.naturalearthdata.com/downloads/50m-cross-blend-hypso/50m-cross-blended-hypso-with-shaded-relief-and-water/

# topo <- stack('./data/spatial-data/HYP_HR_SR_W/HYP_HR_SR_W/HYP_HR_SR_W.tif') # read in as stack so can see RBG layers
topo <- stack("C:\\Users\\gknowlton\\DOI\\Climate Change Scenario Planning - Documents\\WRST RSS\\3.0 Climate futures development (Summer 2021)\\Data\\Boundary_shapefiles\\HYP_HR_SR_W\\HYP_HR_SR_W.tif") # read in as stack so can see RBG layers
ext <- extent(GGCL) # extent defined by lat/long

az <- crop(topo, ext)
plotRGB(az)

az2 <- projectRaster(az, crs = aa)
#basemap <- ggRGB(ak2)

plotRGB(az2)
plot(Sp_park, add = TRUE)
plot(Sp_ggcl, add = TRUE)

# Boundaries

#lab <- st_read('./data/spatial-data/ne_10m_admin_0_boundary_lines_land')
#lines <- dplyr::filter(lab, adm0_right == "United States of America")
#ak_ca <- lines[2,]

################################# PLOTS #############################################

###################################
# Precipitation
###################################
# Mean
r <- raster("C:\\Users\\gknowlton\\DOI\\NPS-NRSS-CCRP-FC Science Adaptation - General\\RSS Stuff\\Parks\\GRCA_CCSP\\nClimGrid - Historical\\rasters\\prcp\\ras_mean.nc")
r<-mask(r,Sp_ggcl)
plot(r)

crs(r) <- aa

# Precip mean
col=hcl.colors(n = 9, palette = "Oslo", alpha = 0.7)[3:9]
barplot(1/sqrt(1:length(col)), col = col)

png("C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/maps/Precip_mean.png")

plotRGB(az2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_ggcl, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = "Precip (in)", line = 1)) 

dev.off()


########################
# Delta

r <- raster("C:\\Users\\gknowlton\\DOI\\NPS-NRSS-CCRP-FC Science Adaptation - General\\RSS Stuff\\Parks\\GRCA_CCSP\\nClimGrid - Historical\\rasters\\prcp\\precip_delta.tif")
r<-mask(r,Sp_ggcl)
plot(r)

crs(r) <- aa

# Precip delta
col=hcl.colors(n = 7, palette = "Blue-Yellow", alpha = 0.7, rev=TRUE)
barplot(1/sqrt(1:length(col)), col = col)

#png(paste(PlotOut,'Precip_delta.png',sep=""))
png("C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/maps/Precip_delta.png")


plotRGB(az2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_ggcl, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = "Precip (in)", line = 1)) 

dev.off()


###################################
# Tmax
###################################
# Mean
r <- raster("C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/rasters/tmax/ras_mean.nc")
plot(r)
r<-mask(r,Sp_ggcl)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmax_mean.png',sep=""))
png("C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/maps/tmax_mean.png")

plotRGB(az2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_ggcl, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmax_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmax_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


###################################
# Tmin
###################################
# Mean
r <- raster("C:/Users/gknowlton/DOI/NPS-NRSS-CCRP-FC Science Adaptation - General/RSS Stuff/Parks/GRCA_CCSP/nClimGrid - Historical/rasters/tmin/ras_mean.nc")
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmin_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmin_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmin_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


###################################
# Tmean
###################################
# Mean
r <- raster(paste(PlotIn,'Tmean_val.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp mean
col=hcl.colors(n = 7, palette = "Viridis", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmean_mean.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text =  expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()


########################
# Delta

r <- raster(paste(PlotIn,'tmean_delta.tif',sep=""))
r<-mask(r,Sp_park)
plot(r)

crs(r) <- aa

# Temp delta
col=hcl.colors(n = 7, palette = "Lajolla", alpha = 0.5)
barplot(1/sqrt(1:length(col)), col = col)

png(paste(PlotOut,'Tmean_delta.png',sep=""))

plotRGB(ak2) 
plot(r, col = col,  legend = FALSE, add = TRUE) 
plot(Sp_park, add = TRUE) + 
  plot(r, col = col, legend.only = TRUE, horizontal = TRUE, legend.args = list(text = expression("Temperature ("*~degree*F*")"), line = 1)) 

dev.off()