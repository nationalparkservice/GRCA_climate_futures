
library(stars)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(units)
library(viridis)
library(grid)
library(lemon)
library(raster)

# x = read_stars("C:/Users/achildress/Downloads/macav2metdata_tasmean_ANN_20402069_rcp45_vs_19712000_bcc-csm1-1.tif")

grca = st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp")

nps_boundary <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\nps_boundary\\nps_boundary.shp")

grca_park <- nps_boundary %>% 
  filter(nps_boundary$UNIT_CODE == "GRCA")
grca_park = st_transform(grca_park, st_crs(grca))

# read in maca files
pr_historical_CHEM <- read_ncdf( "D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_pr_ANN_19712000_historical_MIROC-ESM-CHEM.nc")
pr_historical_CGCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_pr_ANN_19712000_historical_MRI-CGCM3.nc")
pr_20402069_rcp85_CHEM <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
pr_20402069_rcp85_CGCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")
tasmean_historical_CHEM <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_19712000_historical_MIROC-ESM-CHEM.nc")
tasmean_historical_CFCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.nc")
tasmean_20402069_rcp85_CHEM <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
tasmean_20402069_rcp85_CGCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")

# crop
#nc = st_transform(, st_crs(grca))

pr_historical_CHEM = st_transform(pr_historical_CHEM, st_crs(grca))
pr_historical_CHEM_crop = st_crop(pr_historical_CHEM, grca, crop = TRUE)

pr_historical_CGCM3 = st_transform(pr_historical_CGCM3, st_crs(grca))
pr_historical_CGCM3_crop = st_crop(pr_historical_CGCM3, grca, crop = TRUE)

pr_20402069_rcp85_CHEM = st_transform(pr_20402069_rcp85_CHEM, st_crs(grca))
pr_20402069_rcp85_CHEM_crop = st_crop(pr_20402069_rcp85_CHEM, grca, crop = TRUE)

pr_20402069_rcp85_CGCM3 = st_transform(pr_20402069_rcp85_CGCM3, st_crs(grca))
pr_20402069_rcp85_CGCM3_crop = st_crop(pr_20402069_rcp85_CGCM3, grca, crop = TRUE)

tasmean_historical_CHEM = st_transform(tasmean_historical_CHEM, st_crs(grca))
tasmean_historical_CHEM_crop = st_crop(tasmean_historical_CHEM, grca, crop = TRUE)

tasmean_historical_CFCM3 = st_transform(tasmean_historical_CFCM3, st_crs(grca))
tasmean_historical_CFCM3_crop = st_crop(tasmean_historical_CFCM3, grca, crop = TRUE)

tasmean_20402069_rcp85_CHEM = st_transform(tasmean_20402069_rcp85_CHEM, st_crs(grca))
tasmean_20402069_rcp85_CHEM_crop = st_crop(tasmean_20402069_rcp85_CHEM, grca, crop = TRUE)

tasmean_20402069_rcp85_CGCM3 = st_transform(tasmean_20402069_rcp85_CGCM3, st_crs(grca))
tasmean_20402069_rcp85_CGCM3_crop = st_crop(tasmean_20402069_rcp85_CGCM3, grca, crop = TRUE)

#topo <- stack("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\Rasters\\HYP_HR_SR_W.tif") # read in as stack so can see RBG layers
#ext <- st_bbox(grca) # extent defined by lat/long
#topo_crop <- crop(topo, grca)
#topo2<- projectRaster(topo_crop, crs = crs(grca)) 
#t_df  <- as.data.frame(topo2, xy = TRUE)

#calculate average historical rasters for precip and temperature

# temp

hist_cf1_temp <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.tif")
hist_cf2_temp <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_tasmean_ANN_19712000_historical_MIROC-ESM-CHEM.tif")

hist_cf1_temp = projectRaster(hist_cf1_temp, crs = grca)
hist_cf2_temp = projectRaster(hist_cf2_temp, crs = grca)

hist_cf1_temp_crop <- crop(hist_cf1_temp, grca)
plot(hist_cf1_temp_crop, main = "Cropped hist temp cf1")

hist_cf2_temp_crop <- crop(hist_cf2_temp, hist_cf1_temp_crop)
plot(hist_cf2_temp_crop, main = "Cropped hist temp cf2")

hist_temp_stack <- stack(hist_cf1_temp_crop,hist_cf2_temp_crop)

hist_temp_avg <- calc(hist_temp_stack, fun = mean)

plot(hist_temp_avg)

hist_temp_avg_crop <-  st_as_stars(hist_temp_avg)

tempHistAvg_crop = st_transform(hist_temp_avg_crop, st_crs(grca))
tempHistAvg_crop = st_crop(hist_temp_avg_crop, grca, crop = TRUE)
plot(tempHistAvg_crop)

#precip

hist_cf1_pr <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_pr_ANN_19712000_historical_MRI-CGCM3.tif")
hist_cf2_pr <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_pr_ANN_19712000_historical_MIROC-ESM-CHEM.tif")

hist_cf1_pr = projectRaster(hist_cf1_pr, crs = grca)
hist_cf2_pr = projectRaster(hist_cf2_pr, crs = grca)

hist_cf1_pr_crop <- crop(hist_cf1_pr, grca)
plot(hist_cf1_pr_crop, main = "Cropped hist pr cf1")

hist_cf2_pr_crop <- crop(hist_cf2_pr, hist_cf1_pr_crop)
plot(hist_cf2_pr_crop, main = "Cropped hist pr cf2")

hist_temp_stack <- stack(hist_cf1_temp_crop,hist_cf2_temp_crop)

prcpHistAvg_crop <- calc(hist_temp_stack, fun = mean)

plot(prcpHistAvg_crop)

hist_temp_avg_crop <-  st_as_stars(prcpHistAvg_crop)

prcpHistAvg_crop = st_transform(prcpHistAvg_crop, st_crs(grca))
prcpHistAvg_crop = st_crop(prcpHistAvg_crop, grca, crop = TRUE)
plot(prcpHistAvg_crop)

# precipitation plots

prcpHistAvg_plot <- ggplot() +
  geom_stars(data = prcpHistAvg_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "D", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "Historical Precipitation") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
prcpHistAvg_plot

#pr_historical_CHEM_plot <- ggplot() +
#  geom_stars(data = pr_historical_CHEM_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
#  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
#  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
#  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
#  scale_fill_viridis(direction=-1, option = "D", begin = .5, end = 1,
#  guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
#  labs(title = "CHEM Historical Precipitation") +
#  theme_map() +
#  theme(legend.position = "bottom",
#        legend.key.width = unit(6, "cm"),
#        legend.key.height = unit(.3, "cm"),
#        legend.justification = "center",
#        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
#pr_historical_CHEM_plot

#pr_historical_CGCM3_plot <- ggplot() +
#  geom_stars(data = pr_historical_CGCM3_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
#  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
#  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
#  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
#  scale_fill_viridis(direction=-1, option = "D", begin = .5, end = 1,
#                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
#  labs(title = "CGCM3 Historical Precipitation") +
#  theme_map() +
#  theme(legend.position = "bottom",
#        legend.key.width = unit(6, "cm"),
#        legend.key.height = unit(.3, "cm"),
#        legend.justification = "center",
#        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
#pr_historical_CGCM3_plot

pr_20402069_rcp85_CHEM_plot <- ggplot() +
  geom_stars(data = pr_20402069_rcp85_CHEM_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "D", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "CHEM RCP8.5 2040-2069 Precipitation") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
pr_20402069_rcp85_CHEM_plot

pr_20402069_rcp85_CGCM3_plot <- ggplot() +
  geom_stars(data = pr_20402069_rcp85_CGCM3_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "D", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "CGCM3 RCP8.5 2040-2069 Precipitation") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
pr_20402069_rcp85_CGCM3_plot



#temperature plots

tempHistAvg_plot <- ggplot() +
  geom_stars(data = tempHistAvg_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "C", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "Historical Temperature") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
tempHistAvg_plot

#tasmean_historical_CHEM_plot <- ggplot() +
#  geom_stars(data = tasmean_historical_CHEM_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
#  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
#  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
#  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
#  scale_fill_viridis(direction=-1, option = "C", begin = .5, end = 1,
#                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
#  labs(title = "CHEM Historical Temperature") +
#  theme_map() +
#  theme(legend.position = "bottom",
#        legend.key.width = unit(6, "cm"),
#        legend.key.height = unit(.3, "cm"),
#        legend.justification = "center",
#        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
#tasmean_historical_CHEM_plot

#tasmean_historical_CFCM3_plot <- ggplot() +
#  geom_stars(data = tasmean_historical_CFCM3_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
#  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
#  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
#  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
#  scale_fill_viridis(direction=-1, option = "C", begin = .5, end = 1,
#                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
#  labs(title = "CGCM3 Historical Temperature") +
#  theme_map() +
#  theme(legend.position = "bottom",
#        legend.key.width = unit(6, "cm"),
#        legend.key.height = unit(.3, "cm"),
#        legend.justification = "center",
#        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
#tasmean_historical_CFCM3_plot

tasmean_20402069_rcp85_CHEM_plot <- ggplot() +
  geom_stars(data = tasmean_20402069_rcp85_CHEM_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "C", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "CHEM RCP8.5 2040-2069 Temperature") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
tasmean_20402069_rcp85_CHEM_plot

tasmean_20402069_rcp85_CGCM3_plot <- ggplot() +
  geom_stars(data = tasmean_20402069_rcp85_CGCM3_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the landscape I'm plotting
  geom_sf(data = grca_park, aes(), fill = NA,lwd=1.4,colour="gray50") + # shapefile outlining the park boundary
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  scale_fill_viridis(direction=-1, option = "C", begin = .5, end = 1,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "CGCM3 RCP8.5 2040-2069 Temperature") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
tasmean_20402069_rcp85_CGCM3_plot


# creating plots

temp_map_plots <- grid_arrange_shared_legend(tempHistAvg_plot, tasmean_20402069_rcp85_CHEM_plot,tasmean_20402069_rcp85_CGCM3_plot,
                                    ncol = 3, nrow = 1, position = "bottom",
                                    top =  ggpubr::text_grob("GRCA MACA Temperature", face = "italic", size = 20))
temp_map_plots

prcp_map_plots <- grid_arrange_shared_legend(prcpHistAvg_plot, pr_20402069_rcp85_CHEM_plot, pr_20402069_rcp85_CGCM3_plot,
                                    ncol = 3, nrow = 1, position = "bottom",
                                    top =  ggpubr::text_grob("GRCA MACA Precipitation", face = "italic", size = 20))
prcp_map_plots
