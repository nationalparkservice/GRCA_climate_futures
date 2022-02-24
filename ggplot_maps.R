


rm(list = ls())
##### load packages #####
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
library(cowplot)
library(ggpubr)
library(gridExtra)
library(grid)

##### bringing in initial data #####

site = "GRCA"
aa <- CRS('+init=EPSG:5070') # Conus Albers
latlong = CRS('+init=EPSG:4326') # Lat/Long

PlotIn <- "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output_corrected/"
PlotOut <- "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output_corrected/maps-figs/feb22/"

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
ext2 <- extent(GGCL)+0.4 # extent defined by lat/long
ext <- extent(GGCL)


az <- crop(topo, ext2)
ggRGB(az,r=1,b=3,g=2)

az2 <- projectRaster(az, crs = aa)
#basemap <- ggRGB(ak2)

ggRGB(az2,r=1,b=3,g=2) +
  ggplot(Sp_ggcl, aes(x = long, y= lat))

##### make ggcl and park boundaries usable in ggplot #####  
  
# in order to plot polygons, first fortify the data
Sp_ggcl@data$id <- rownames(Sp_ggcl@data)
# create a data.frame from our spatial object
ggcl_plot <- fortify(Sp_ggcl, region = "id")
# merge the "fortified" data with the data from our spatial object
ggcl_plot_df <- merge(ggcl_plot, Sp_ggcl@data,
                   by = "id")

# in order to plot polygons, first fortify the data
Sp_park@data$id <- rownames(Sp_park@data)
# create a data.frame from our spatial object
park_plot <- fortify(Sp_park, region = "id")
# merge the "fortified" data with the data from our spatial object
park_plot_df <- merge(park_plot, Sp_park@data,
                      by = "id")
crs(Sp_ggcl) <- aa
crs(Sp_park) <- aa
##### creating plots #####

## precip mean

precip_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\prcp\\ras_mean.tif")
crs(precip_r) <- latlong
precip_r<-projectRaster(precip_r, crs=crs(aa))

precip_r<-mask(precip_r,Sp_ggcl)

#--- convert to data.frame ---#
precip_r_df <- as.data.frame(precip_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(precip_r_df)

precip_mean <- ggplot()

precip_mean <- precip_mean + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=precip_r_df,aes(x=x,y=y,fill=ras_mean),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Average precipitation (1895-2020)") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
# scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7), trans = 'reverse')
precip_mean

## precip delta

pr_delta_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\prcp\\prcp_delta.tif")
crs(pr_delta_r) <- latlong
pr_delta_r<-projectRaster(pr_delta_r, crs=crs(aa))
pr_delta_r<-mask(pr_delta_r,Sp_ggcl)

#--- convert to data.frame ---#
pr_delta_r_df <- as.data.frame(pr_delta_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
 na.omit()
#--- take a look ---#
head(pr_delta_r_df)

precip_delta <- ggplot()

precip_delta <- precip_delta + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=pr_delta_r_df,aes(x=x,y=y,fill=prcp_delta),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Precipitation Change (1895-2020)") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
# scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7), trans = 'reverse')
precip_delta

## tmax mean

tmax_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tmax\\ras_mean.tif")
crs(tmax_r) <- latlong
tmax_r<-projectRaster(tmax_r, crs=crs(aa))

tmax_r<-mask(tmax_r,Sp_ggcl)

#--- convert to data.frame ---#
tmax_r_df <- as.data.frame(tmax_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tmax_r_df)

tmax_mean <- ggplot()

tmax_mean <- tmax_mean + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tmax_r_df,aes(x=x,y=y,fill=ras_mean),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Maximum temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
 #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
 scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tmax_mean

## tmax delta

tmax_delta_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tmax\\tmax_delta.tif")
crs(tmax_delta_r) <- latlong
tmax_delta_r<-projectRaster(tmax_delta_r, crs=crs(aa))

tmax_delta_r<-mask(tmax_delta_r,Sp_ggcl)

#--- convert to data.frame ---#
tmax_delta_r_df <- as.data.frame(tmax_delta_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tmax_delta_r_df)

tmax_delta <- ggplot()

tmax_delta <- tmax_delta + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tmax_delta_r_df,aes(x=x,y=y,fill=tmax_delta),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5 ) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5 ) +
  labs(title = "Maximum temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
  scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tmax_delta




## tmin mean

tmin_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tmin\\ras_mean.tif")
crs(tmin_r) <- latlong
tmin_r<-projectRaster(tmin_r, crs=crs(aa))

tmin_r<-mask(tmin_r,Sp_ggcl)

#--- convert to data.frame ---#
tmin_r_df <- as.data.frame(tmin_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tmin_r_df)

tmin_mean <- ggplot()

tmin_mean <- tmin_mean + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tmin_r_df,aes(x=x,y=y,fill=ras_mean),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Minimum temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
  scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tmin_mean

## tmax delta

tmin_delta_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tmin\\tmin_delta.tif")
crs(tmin_delta_r) <- latlong
tmin_delta_r<-projectRaster(tmin_delta_r, crs=crs(aa))

tmin_delta_r<-mask(tmin_delta_r,Sp_ggcl)

#--- convert to data.frame ---#
tmin_delta_r_df <- as.data.frame(tmin_delta_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tmin_delta_r_df)

tmin_delta <- ggplot()

tmin_delta <- tmin_delta + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tmin_delta_r_df,aes(x=x,y=y,fill=tmin_delta),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Minimum temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
  scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tmin_delta



## tave mean

tave_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tave\\ras_mean.tif")
crs(tave_r) <- latlong
tave_r<-projectRaster(tave_r, crs=crs(aa))

tave_r<-mask(tave_r,Sp_ggcl)

#--- convert to data.frame ---#
tave_r_df <- as.data.frame(tave_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tave_r_df)

tave_mean <- ggplot()

tave_mean <- tave_mean + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tave_r_df,aes(x=x,y=y,fill=ras_mean),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Average temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
  scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tave_mean

## tmin delta

tave_delta_r <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\output_corrected\\tave\\tave_delta.tif")
crs(tave_delta_r) <- latlong
tave_delta_r<-projectRaster(tave_delta_r, crs=crs(aa))
tave_delta_r<-mask(tave_delta_r,Sp_ggcl)

#--- convert to data.frame ---#
tave_delta_r_df <- as.data.frame(tave_delta_r, xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit()
#--- take a look ---#
head(tave_delta_r_df)

tave_delta <- ggplot()

tave_delta <- tave_delta + ggRGB(az2,r=1,b=3,g=2, ggLayer = TRUE) + 
  geom_raster(data=tave_delta_r_df,aes(x=x,y=y,fill=tave_delta),alpha = 0.6)+
  geom_polygon(data = ggcl_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  geom_polygon(data = park_plot_df, aes(x = long, y = lat, group = group), color = "black", fill = NA,lwd=0.5) +
  labs(title = "Average temperature") +
  theme_map() +
  theme(legend.position = "right",
        legend.key.width = unit(.3, "cm"),
        legend.key.height = unit(.75, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=9,face="bold",hjust=0.5)) +
  labs(fill = "") +
  #scale_fill_gradientn(colours = rainbow(999, start = 0.05, end = .7))
  scale_fill_gradientn(colours = rev(rainbow(999, start = 0.05, end = .7))) 

tave_delta


##### Creating figures #####

## GGCL precipitation figure

precip_fig <- ggarrange(precip_mean, ncol=1, nrow=1)

precip_fig_print <-grid.arrange(precip_fig,bottom=textGrob("Precipitation (in) (1895-2020)",
                                                                 gp=gpar(fontface="bold", col="black", fontsize=12)))

precip_fig_print

ggsave("output/maps/precip_fig.jpg", precip_fig_print,width = 4.5, height = 3.5)


## GGCL temp averages figure

temp_mean_fig <- ggarrange(tave_mean, tmax_mean, tmin_mean, ncol=1, nrow=3)

temp_mean_fig_print <-grid.arrange(temp_mean_fig,bottom=textGrob("Annual mean temperature (°F) (1895-2020)",
                                  gp=gpar(fontface="bold", col="black", fontsize=12)))

temp_mean_fig_print

ggsave("output/maps/temp_mean_fig.jpg", temp_mean_fig_print,width = 4.5, height = 7)

## GGCL temp delta figure

temp_delta_fig <- ggarrange(tave_delta, tmax_delta, tmin_delta, ncol=1, nrow=3)

temp_delta_fig_print <-grid.arrange(temp_delta_fig,bottom=textGrob("Change in mean temperature (°F) (1895-2020)",
                                                                 gp=gpar(fontface="bold", col="black", fontsize=12)))
temp_delta_fig_print

ggsave("output/maps/temp_delta_fig.jpg", temp_delta_fig_print,width = 4.5, height = 7)


