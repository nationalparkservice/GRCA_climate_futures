# MACA mapping
# Using geoTIFFs from https://climate.northwestknowledge.net/MACA/tool_summarymaps2.php

library(raster)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stars)

file = "C:/Users/achildress/Downloads/macav2metdata_prpercent_ANN_20402069_rcp85_vs_19712000_MRI-CGCM3.tif"

# x <- raster(file)

var.name = sub(".*data_*(.*?) *_2040.*", "\\1", file)
var.name
stars <- read_stars(file)
names(stars) = var.name

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
