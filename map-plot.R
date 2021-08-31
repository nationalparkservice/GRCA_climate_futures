
library(stars)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(units)

# x = read_stars("C:/Users/achildress/Downloads/macav2metdata_tasmean_ANN_20402069_rcp45_vs_19712000_bcc-csm1-1.tif")

x <- read_ncdf("C:/Users/achildress/Downloads/macav2metdata_tasmax_ANN_20102039_rcp45_vs_19712000_MIROC-ESM.nc")

grca = st_read("C:/Users/achildress/OneDrive - DOI/Documents/GIS/GRCA/GRCA.shp")
grca <- st_transform(grca, 3338)

# crop
nc = st_transform(x, st_crs(grca))
nc_crop = nc[grca]

nc_crop = drop_units(nc_crop)
nc_crop %>% mutate(categories = if_else(air_temperature < 3.85, 1,2),
                   as.factor(categories, levels=c("precip","snow"))) %>% select(categories) -> crop_cat


nc_crop %>% 
  mutate(categories = case_when(
    air_temperature < 3.9  ~ "Snow",
    air_temperature > 3.9 & air_temperature < 3.95 ~ "Mix",
    air_temperature > 3.95  ~ "Precip"),
    categories = factor(categories, levels=c("Snow","Mix","Precip"))
  )  %>% select(categories) -> cat_crop



ggplot() +
  # geom_raster(data = ak_df ,aes(x = x, y = y,alpha=HYP_HR_SR_W.1), show.legend=FALSE) + #it's a multiband raster so alpha is band I wanted to use
  geom_stars(data = cat_crop, alpha = 0.4) + # the WB layer I was plotting - alpha is transparency
  geom_sf(data = grca, aes(), fill = NA,lwd=2,colour="black") + # shapefile outlining the part of the park I'm plotting
  # geom_sf(data = wrst_mtns, aes(), fill = "blue",lwd=1,colour="black") +
  # scale_fill_viridis(direction=-1, option = "C", #begin = .5, end = 1,
  # guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + # I don't actually love this palette, but good enough for now
  labs(title = "MIROC-ESM RCP 4.5") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5)) 
  # plot.background = element_rect(colour = col, fill=NA, size=5)) +
  # labs(fill = "Water Balance") +
  # scale_colour_manual(values = c(rgb(207, 31, 46, maxColorValue = 255)), "#ffda85")



