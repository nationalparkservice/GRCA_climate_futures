# MACA mapping
# Using geoTIFFs from https://climate.northwestknowledge.net/MACA/tool_summarymaps2.php

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
library(stars)

site = "GRCA"
ca <- CRS('+init=EPSG:5070') # Conus Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long

PlotIn <- "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/GRCA_maps/Rasters"
PlotOut <- "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/GRCA_maps/Maps"

topo <- stack("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\Rasters\\HYP_HR_SR_W.tif") # read in as stack so can see RBG layers
ext <- extent(-114.5, -110.7, 35.3, 37.5) # extent defined by lat/long

gcaz <- raster::crop(topo, ext)
raster::plotRGB(gcaz)

gcaz2 <- projectRaster(gcaz, crs = ca)


# NPS Boundary

nps_boundary <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\nps_boundary\\nps_boundary.shp")
park <- dplyr::filter(nps_boundary, UNIT_CODE == site) # subset to GRCA only
park <- st_transform(park, crs = 5070)

GCGL <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp")
GCGL <- st_transform(GCGL, crs = 5070)
Sp_gcgl <- as_Spatial(GCGL)
Sp_gcgl <- spTransform(Sp_gcgl, CRSobj = "+init=epsg:5070")

Sp_park <- as_Spatial(park)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:5070") 

raster::plotRGB(gcaz, addfun = TRUE)
plot(Sp_park, add = TRUE)
plot(Sp_gcgl, add = TRUE)


file = "D:\\GRCA\\MACA_Summaries\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc"

# x <- raster(file)

var.name = sub(".*data_*(.*?) *_2040.*", "//1", file)
var.name
stars <- read_stars("D:\\GRCA\\MACA_Summaries\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
names(stars) = var.name

geom_stars(stars)

st_transform(stars, Sp_gcgl)
st_crop(stars, Sp_gcgl)

# add in shapefile of area and clip to area. See code from following repositories for examples:
# https://github.com/nationalparkservice/WRST-climate-futures/blob/main/Code/Historical-trends/wrst_maps.R
# https://github.com/nationalparkservice/WRST-climate-futures/blob/main/Code/Metric-development/met/met-monthly-stars.R

ggplot() + # Resolution is course
  geom_stars(data =stars, alpha = 0.8) + 
  # facet_wrap("time") +
  # scale_fill_viridis() + 
  #coord_equal() + 
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))
