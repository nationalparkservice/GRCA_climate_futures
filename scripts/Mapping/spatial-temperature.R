# Spatial Temperature Plots

library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);library(units); library(tidyr); library(lemon);library(ggpubr);library(gridExtra);library(grid); library(gtable); library(lubridate);library(raster)
#
#base.dir = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/GRCA_maps/spatial-maps"
#data.dir = paste0(base.dir,"/Data")
#plot.dir = "./Data/figures"

var = "Annual.Temp"
long.title = "average annual temperature"
scale = "viridis"

GCMs <- c("Average","CGCM3.rcp85","CHEM.rcp85")
CFs <- c("Historical","Climate Future 1", "Climate Future 2")
cols <- c("#F28FE2","#12045C","#ffcccc")
CF_GCM <- data.frame(CF=CFs,GCM=GCMs,CF_col=cols)

grca = st_read("C://Users//gknowlton//OneDrive - DOI//Documents//GRCA//GRCA_maps//GIS//GGCL//GGCL.shp")
grca <- st_transform(grca, 3338)

shp = grca

nps_boundary <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\nps_boundary\\nps_boundary.shp")

grca_park <- nps_boundary %>% 
  filter(nps_boundary$UNIT_CODE == "GRCA")
grca_park = st_transform(grca_park, st_crs(grca))

# read in maca files

tasmean_historical_CHEM <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_19712000_historical_MIROC-ESM-CHEM.nc")
tasmean_historical_CFCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.nc")
tasmean_20402069_rcp85_CHEM <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
tasmean_20402069_rcp85_CGCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\stars\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")

# transform and crop

tasmean_historical_CHEM = st_transform(tasmean_historical_CHEM, st_crs(grca))
tasmean_historical_CHEM_crop = st_crop(tasmean_historical_CHEM, grca, crop = TRUE)

tasmean_historical_CFCM3 = st_transform(tasmean_historical_CFCM3, st_crs(grca))
tasmean_historical_CFCM3_crop = st_crop(tasmean_historical_CFCM3, grca, crop = TRUE)
plot(tasmean_historical_CFCM3_crop)

tasmean_20402069_rcp85_CHEM = st_transform(tasmean_20402069_rcp85_CHEM, st_crs(grca))
tasmean_20402069_rcp85_CHEM_crop = st_crop(tasmean_20402069_rcp85_CHEM, grca, crop = TRUE)

tasmean_20402069_rcp85_CGCM3 = st_transform(tasmean_20402069_rcp85_CGCM3, st_crs(grca))
tasmean_20402069_rcp85_CGCM3_crop = st_crop(tasmean_20402069_rcp85_CGCM3, grca, crop = TRUE)

# create mean temperature raster (need to get this working to make historical data)

tasmean_historical_CHEM_crop
tasmean_historical_CFCM3_crop

temp_hist_mean_stars <- (tasmean_historical_CHEM_crop / tasmean_historical_CFCM3_crop) / 2

#
#hist_cf1_temp <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.tif")
#hist_cf2_temp <- raster("D:\\GRCA\\MACA_Summaries\\tif\\macav2metdata_tasmean_ANN_19712000_historical_MIROC-ESM-CHEM.tif")
#
#hist_cf1_temp = projectRaster(hist_cf1_temp, crs = grca)
#hist_cf2_temp = projectRaster(hist_cf2_temp, crs = grca)
#
#hist_cf1_temp_crop <- crop(hist_cf1_temp, grca)
#plot(hist_cf1_temp_crop, main = "Cropped hist temp cf1")
#
#hist_cf2_temp_crop <- crop(hist_cf2_temp, hist_cf1_temp_crop)
#plot(hist_cf2_temp_crop, main = "Cropped hist temp cf2")
#
#hist_temp_stack <- stack(hist_cf1_temp_crop,hist_cf2_temp_crop)
#
#hist_temp_avg <- calc(hist_temp_stack, fun = mean)
#
#plot(hist_temp_avg)
#
#hist_temp_avg_crop <-  st_as_stars(hist_temp_avg)
#
#hist_temp_avg_crop = st_transform(hist_temp_avg_crop, st_crs(grca))
#hist_temp_avg_crop = st_crop(hist_temp_avg_crop, grca, crop = TRUE)
#plot(hist_temp_avg_crop)

# read in RDS for setting scale limits
historical <- temp_hist_mean_stars
cf1 <- tasmean_20402069_rcp85_CGCM3_crop
cf2 <- tasmean_20402069_rcp85_CHEM_crop

historical$air_temperature <- drop_units(historical$air_temperature)
cf1$air_temperature <- drop_units(cf1$air_temperature)
cf2$air_temperature <- drop_units(cf2$air_temperature)

#need to include historical data in this list when it is calculated...

scale.min = min(c(historical$air_temperature,cf1$air_temperature,cf2$air_temperature),na.rm=TRUE)
scale.max = max(c(historical$air_temperature,cf1$air_temperature,cf2$air_temperature),na.rm=TRUE)

# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA) + 
    geom_sf(data = grca_park, aes(), fill = NA) +
    scale_fill_viridis(direction=-1, option = scale,
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5),
                       limits = c(scale.min, scale.max), oob = scales::squish) + #mako for WB delta
    labs(title = title) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, size=5)) + 
    labs(fill = paste0(metric))
}

historical.plot <- map.plot(data=historical,title=CFs[1],metric=long.title,col=cols[1])
cf1.plot <- map.plot(data=cf1,title=CFs[2],metric=long.title,col=cols[2])
cf2.plot <- map.plot(data=cf2,title=CFs[3],metric=long.title,col=cols[3])

historical.plot
cf1.plot
cf2.plot


maps <- grid_arrange_shared_legend(historical.plot, cf1.plot, cf2.plot, ncol = 3, nrow = 1, position = "bottom", 
                                   top = textGrob(paste0("Change in ",long.title),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))





