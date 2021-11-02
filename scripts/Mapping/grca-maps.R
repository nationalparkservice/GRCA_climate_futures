# Spatial Temperature Plots

# Load Packages
library(stars);library(dplyr);library(ggplot2);library(ggthemes);library(viridis);library(here);library(ggrepel);library(rlang);library(units); library(tidyr); library(lemon);library(ggpubr);library(gridExtra);library(grid); library(gtable); library(lubridate);library(raster)

# Set params

var = "Annual.Temp"
long.title = "Change in annual temperature (\u00B0F)"
scale = "inferno"

GCMs <- c("Average","CGCM3.rcp85","CHEM.rcp85")
CFs <- c("Historical (1971-2010)","Climate Future 1 (2040-2069)", "Climate Future 2 (2040-2069)")
cols <- c("#B4B4B4","#9A9EE5","#E10720")
CF_GCM <- data.frame(CF=CFs,GCM=GCMs,CF_col=cols)

grca = st_read("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/GRCA_maps/GIS/GGCL/GGCL.shp")
grca <- st_transform(grca, 3338)

shp = grca

nps_boundary <- st_read("C:/Users/gknowlton/OneDrive - DOI/Documents/GIS/nps_boundary/nps_boundary.shp")

grca_park <- nps_boundary %>% 
  filter(nps_boundary$UNIT_CODE == "GRCA")

grca_park = st_transform(grca_park, st_crs(grca))

### read in all maca files

##temperature

tasmean_historical_CHEM <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\historical\\macav2metdata_tasmean_ANN_19712000_historical_MIROC-ESM-CHEM.nc")
tasmean_historical_CFCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\historical\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.nc")

tasmean_20402069_rcp85_CHEM_abs <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\absolute\\macav2metdata_tasmean_ANN_20402069_rcp85_MIROC-ESM-CHEM.nc")
tasmean_20402069_rcp85_CGCM3_abs <- read_ncdf("D:\\GRCA\\MACA_Summaries\\absolute\\macav2metdata_tasmean_ANN_20402069_rcp85_MRI-CGCM3.nc")

tasmean_20402069_rcp85_CHEM_changeVals <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\changeVals\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
tasmean_20402069_rcp85_CGCM3_changeVals <- read_ncdf("D:\\GRCA\\MACA_Summaries\\changeVals\\macav2metdata_tasmean_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")

# calculate average historical temperature

tasmean_hist_avg <- ((tasmean_historical_CHEM + tasmean_historical_CFCM3) / 2)

# transform and crop 

tasmean_hist_avg = st_transform(tasmean_hist_avg, st_crs(grca))
tasmean_hist_avg_crop = st_crop(tasmean_hist_avg, grca, crop = TRUE)

tasmean_20402069_rcp85_CHEM_abs = st_transform(tasmean_20402069_rcp85_CHEM_abs, st_crs(grca))
tasmean_20402069_rcp85_CHEM_abs_crop = st_crop(tasmean_20402069_rcp85_CHEM_abs, grca, crop = TRUE)

tasmean_20402069_rcp85_CGCM3_abs = st_transform(tasmean_20402069_rcp85_CGCM3_abs, st_crs(grca))
tasmean_20402069_rcp85_CGCM3_abs_crop = st_crop(tasmean_20402069_rcp85_CGCM3_abs, grca, crop = TRUE)

tasmean_20402069_rcp85_CHEM_changeVals = st_transform(tasmean_20402069_rcp85_CHEM_changeVals, st_crs(grca))
tasmean_20402069_rcp85_CHEM_changeVals_crop = st_crop(tasmean_20402069_rcp85_CHEM_changeVals, grca, crop = TRUE)

tasmean_20402069_rcp85_CGCM3_changeVals = st_transform(tasmean_20402069_rcp85_CGCM3_changeVals, st_crs(grca))
tasmean_20402069_rcp85_CGCM3_changeVals_crop = st_crop(tasmean_20402069_rcp85_CGCM3_changeVals, grca, crop = TRUE)

##precipitation

pr_historical_CHEM <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\historical\\macav2metdata_pr_ANN_19712000_historical_MIROC-ESM-CHEM.nc")
pr_historical_CFCM3 <- read_ncdf("D:\\GRCA\\MACA_Summaries\\historical\\macav2metdata_tasmean_ANN_19712000_historical_MRI-CGCM3.nc")

pr_20402069_rcp85_CHEM_abs <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\absolute\\macav2metdata_pr_ANN_20402069_rcp85_MIROC-ESM-CHEM.nc")
pr_20402069_rcp85_CGCM3_abs <- read_ncdf("D:\\GRCA\\MACA_Summaries\\absolute\\macav2metdata_pr_ANN_20402069_rcp85_MRI-CGCM3.nc")

pr_20402069_rcp85_CHEM_changeVals <-  read_ncdf("D:\\GRCA\\MACA_Summaries\\changeVals\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MIROC-ESM-CHEM.nc")
pr_20402069_rcp85_CGCM3_changeVals <- read_ncdf("D:\\GRCA\\MACA_Summaries\\changeVals\\macav2metdata_pr_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")

# calculate average historical precipitation
pr_historical_CHEM$precipitation <- drop_units(pr_historical_CHEM$precipitation)
pr_historical_CFCM3$air_temperature <- drop_units(pr_historical_CFCM3$air_temperature)

pr_hist_avg <- ((pr_historical_CHEM + pr_historical_CFCM3) / 2)

# transform and crop 

pr_hist_avg = st_transform(pr_hist_avg, st_crs(grca))
pr_hist_avg_crop = st_crop(pr_hist_avg, grca, crop = TRUE)

pr_20402069_rcp85_CHEM_abs = st_transform(pr_20402069_rcp85_CHEM_abs, st_crs(grca))
pr_20402069_rcp85_CHEM_abs_crop = st_crop(pr_20402069_rcp85_CHEM_abs, grca, crop = TRUE)

pr_20402069_rcp85_CGCM3_abs = st_transform(pr_20402069_rcp85_CGCM3_abs, st_crs(grca))
pr_20402069_rcp85_CGCM3_abs_crop = st_crop(pr_20402069_rcp85_CGCM3_abs, grca, crop = TRUE)

pr_20402069_rcp85_CHEM_changeVals = st_transform(pr_20402069_rcp85_CHEM_changeVals, st_crs(grca))
pr_20402069_rcp85_CHEM_changeVals_crop = st_crop(pr_20402069_rcp85_CHEM_changeVals, grca, crop = TRUE)

pr_20402069_rcp85_CGCM3_changeVals = st_transform(pr_20402069_rcp85_CGCM3_changeVals, st_crs(grca))
pr_20402069_rcp85_CGCM3_changeVals_crop = st_crop(pr_20402069_rcp85_CGCM3_changeVals, grca, crop = TRUE)

######################################
# Below this section, need to plug-in the data of interest each time to produce plot
# Be sure to change the variable when setting the scale.min and scale.max
######################################

# read in RDS for setting scale limits
historical <- tasmean_hist_avg_crop
cf1 <- tasmean_20402069_rcp85_CGCM3_changeVals_crop
cf2 <- tasmean_20402069_rcp85_CHEM_changeVals_crop

historical$air_temperature <- drop_units(historical$air_temperature)
cf1$air_temperature <- drop_units(cf1$air_temperature)
cf2$air_temperature <- drop_units(cf2$air_temperature)

#need to include historical data in this list when it is calculated...

#scale.min = min(c(historical$air_temperature, cf1$air_temperature, cf2$air_temperature),na.rm=TRUE)
scale.min = min(cf2$air_temperature)
#scale.max = max(c(historical$air_temperature,cf1$air_temperature, cf2$air_temperature),na.rm=TRUE)
scale.max = max(cf2$air_temperature)
# ggplot
map.plot <- function(data, title,xaxis,metric,col){
  ggplot() + 
    geom_stars(data = data, alpha = 0.8) + 
    geom_sf(data = shp, aes(), fill = NA, size = 2, color = "grey80") + 
    geom_sf(data = grca_park, aes(), fill = NA, size = 1, color = "grey80") +
    scale_fill_viridis(direction=1, option = scale,
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

maps <- grid_arrange_shared_legend(#historical.plot, 
                                   #cf1.plot, 
                                   cf2.plot,
                                   ncol = 1, nrow = 1, position = "bottom", 
                                   top = textGrob(paste0(long.title),
                                                  gp=gpar(fontface="bold", col="black", fontsize=16)))

ggsave("change-annual-temp-cf2.png", plot = maps, width = 15, height = 9, path = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/GRCA_maps/Maps/updated")

