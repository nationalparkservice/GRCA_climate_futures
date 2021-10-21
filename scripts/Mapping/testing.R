# MACA mapping
# Using geoTIFFs from https://climate.northwestknowledge.net/MACA/tool_summarymaps2.php

library(raster)
library(sf)
library(tidyverse)
library(ggplot2)
library(RStoolbox)
library(stars)


site = "GRCA"
ca <- CRS('+init=EPSG:5070') # Conus Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long

topo <- stack("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\Rasters\\HYP_HR_SR_W.tif") # read in as stack so can see RBG layers
ext <- extent(-114.5, -110.7, 35.3, 37.5) # extent defined by lat/long

gcaz <- raster::crop(topo, ext)
raster::plotRGB(gcaz)

gcaz2 <- projectRaster(gcaz, crs = ca)

# NPS Boundary

nps_boundary <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\nps_boundary\\nps_boundary.shp")
park <- filter(nps_boundary, UNIT_CODE == site) # subset to GRCA only
park <- st_transform(park, crs = 5070)

GCGL <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp")
GCGL <- st_transform(GCGL, crs = 5070)
Sp_gcgl <- as_Spatial(GCGL)
Sp_gcgl <- spTransform(Sp_gcgl, CRSobj = "+init=epsg:5070")

Sp_park <- as_Spatial(park)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:5070") 

plotRGB(gcaz2)
plot(Sp_gcgl, add = TRUE, col = "blue")
plot(Sp_park, add = TRUE, col = "orange")

# Read in MACA Rasters

pr_19712000_historical_CHEM <- read_stars("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_pr_ANN_19712000_historical_MIROC-ESM-CHEM.nc")

plot(pr_19712000_historical_CHEM)
pr_19712000_historical_CHEM_crop <- st_transform(pr_19712000_historical_CHEM, Sp_gcgl)

pr_19712000_historical_CHEM_crop <- st_crop(pr_19712000_historical_CHEM, Sp_gcgl)
plot(pr_19712000_historical_CHEM_crop)

plot(Sp_gcgl, add=TRUE)

pr_19712000_historical_CHEM_crop <- projectRaster(pr_19712000_historical_CHEM,
                                                  crs = crs(Sp_gcgl))
pr_19712000_historical_CHEM_crop <- crop(pr_19712000_historical_CHEM, Sp_gcgl)

