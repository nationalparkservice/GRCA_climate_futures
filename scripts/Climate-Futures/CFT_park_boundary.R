rm(list=ls())

library(sf)
library(raster)
library(dplyr)
library(rgdal)
library(tmap)
library(tmaptools)
library(ggplot2)
library(cft)

# Enter the following code if you need to install Java and point R to the directory:
#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_261') # for 64-bit version #make sure to check the program file for the jre1 numbers.

# Required data files for DEM and Soil WHC measurements are located on the Shared Drive:
# DOI\WB working group - General\Data_Files
# It is probably best to download the files to your local computer if you will use this script regularly

###########################   USER INPUTS ###################################################################################### 
# Park info

site <- "GRCA"

# Set projection to be used for all spatial data:

proj4 <-"+init=epsg:5070" #  North America Albers Equal Area Conic
epsg <- 5070 # North American Albers Equal Area Conic

OutDir <- "C:/Users/achildress/Documents/Parse_test/Boundary/" # Output directory. Ouput created is a .csv file with site parameters and a series of maps for ppt presentation

###########################   END USER INPUTS   #################################################################################
OutDir="C:/Users/achildress/Documents/Parse_test/Boundary/scbl/"
# shapefiles - can use epsg code or existing object to define projection

nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary2018/nps_boundary.shp')
nps_centroids <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')


# select park

# park <- filter(nps_boundary, UNIT_CODE == site)
park = subset(nps_boundary, UNIT_CODE == "SCBL")
# park <- st_transform(park, st_crs(epsg))
Sp_park= as(park, "Spatial")
Sp_park <- as_Spatial(park)


d <- cftdata(aoi = Sp_park, area_name = "SCBL", parameters = "tasmax", 
             years = c(2020, 2023), models = "GFDL-ESM2M", scenarios = "rcp85",local_dir = OutDir)
d$full_varname

scbl<-brick(paste(OutDir,"scbl/tasmax_scbl_CCSM4_r6i1p1_rcp85_macav2metdata_2006_2007_daily.tif",sep="")) #Stack 
scbl
plot(scbl)
r_mean<-calc(scbl,mean)
plot(r_mean)



df <- cft_df(d, ncores = parallel::detectCores() / 2)
df

df %>%
  ggplot(aes(date, tasmax)) + 
  geom_point() + 
  geom_line(alpha = .1) + 
  xlab("Date") + 
  ylab("Max. air temp. (K)") + 
  ggtitle("Grand Canyon National Park, CCSM4, RCP 8.5")
  