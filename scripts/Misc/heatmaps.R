## GRCA Heatmaps

# Last updated 2021-12-20 by GJK

## Load in packages & DATA

# packages 

library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(tidyverse)

######### data wrangling ######### 
# data

DEM <- raster("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\Repos\\CCRP_Climate_Futures_v1.0\\data\\general\\spatial-data\\elevation_cropped.tif")
GGCL <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp")
#prcp <- raster("C:\\Users\\gknowlton\\DOI\\NPS-NRSS-CCRP-FC Science Adaptation - General\\RSS Stuff\\Parks\\GRCA_CCSP\\nClimGrid - Historical\\rasters\\prcp\\ras_mean.tif")
#tmin <- raster("C:\\Users\\gknowlton\\DOI\\NPS-NRSS-CCRP-FC Science Adaptation - General\\RSS Stuff\\Parks\\GRCA_CCSP\\nClimGrid - Historical\\rasters\\prcp\\ras_mean.tif")
# Resampling DEM

ca <- CRS("+init=epsg:5070") #conus albers
latlong = CRS('+init=EPSG:4326') # Lat/Long


GGCL <- st_transform(GGCL, ca)
DEM <- projectRaster(DEM, crs = ca)
crs(prcp) <- latlong
#prcp<-projectRaster(prcp, crs=crs(ca))
prcp_stack <- readRDS("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\prcp_stack.Rds")
crs(prcp_stack) <- latlong
prcp_stack <- projectRaster(prcp_stack, crs=crs(ca))

tmin_stack <- readRDS("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\tmin_stack.Rds")
crs(tmin_stack) <- latlong
tmin_stack <- projectRaster(tmin_stack, crs=crs(ca))

tmax_stack <- readRDS("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\nClimGrid\\tmax_stack.Rds")
crs(tmax_stack) <- latlong
tmax_stack <- projectRaster(tmax_stack, crs=crs(ca))

#rename raster stacks

dates <- seq(ym("1895-1"),ym("2020-12"), by = "months")
dates <- format(dates, "%Y-%m")

names(prcp_stack) <- dates
names(tmin_stack) <- dates
names(tmax_stack) <- dates

# check alignment
plot(DEM)
plot(GGCL, add = TRUE)

pal <- colorRampPalette(c("white","black"))
plot(DEM, col = pal(1000))
plot(prcp_stack, add = TRUE, alpha =0.8)

# resample

DEM_resampled <- resample(DEM, prcp_stack, method = "bilinear")

plot(DEM_resampled)
plot(prcp_stack[[1]], add = TRUE)

#convert to spatial points data frame

PRCP_points <- rasterToPoints(prcp_stack, spatial = TRUE)
tmax_points <- rasterToPoints(tmax_stack, spatial = TRUE)
tmin_points <- rasterToPoints(tmin_stack, spatial = TRUE)
#write.table(PRCP_points, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/prcp_points.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

plot(DEM_resampled)
plot(PRCP_points, add = TRUE)

DEM_values_prcp <- raster::extract(DEM_resampled, PRCP_points)
DEM_values_tmax <- raster::extract(DEM_resampled, tmax_points)
DEM_values_tmin <- raster::extract(DEM_resampled, tmin_points)

DEMvals_prcp <- data.frame(DEM_values_prcp)
DEMvals_tmax <- data.frame(DEM_values_tmax)
DEMvals_tmin <- data.frame(DEM_values_tmin)

combinePointValue_prcp=cbind(PRCP_points,DEMvals_prcp)
combinePointValue_tmax=cbind(tmax_points,DEMvals_tmax)
combinePointValue_tmin=cbind(tmin_points,DEMvals_tmin)

write.table(combinePointValue_prcp, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_prcp.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)
write.table(combinePointValue_tmax, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_tmax.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)
write.table(combinePointValue_tmin, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_tmin.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)



##### read in the .csv with heatmap values

#prcp

prcp_hmData <- read_csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_prcp.csv")
names(prcp_hmData)[1:1512] <- substring(names(prcp_hmData)[1:1512],2)
prcp_hmData <- prcp_hmData %>% 
  select(-optional)

prcp_hmData$CellID <- 1:nrow(prcp_hmData)

long_prcp <- prcp_hmData %>% 
  gather(prcp_hmData, value, -c(CellID, DEM_values_prcp, x, y)) %>% 
  mutate(year = substr(prcp_hmData,1,4),
         month = substr(prcp_hmData, 6,7),
         inches = (value/25.4)) %>% 
  select(-prcp_hmData,-value)

# double check this later.. convert negative values to Zero(0)
long_prcp <- long_prcp %>% 
  mutate(inches = replace(inches, which(inches<0), 0)) 
min(long_prcp$inches)

write.csv(long_prcp, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/prcp_hm.csv")

#tmax

tmax_hmData <- read_csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_tmax.csv")
names(tmax_hmData)[1:1512] <- substring(names(tmax_hmData)[1:1512],2)
tmax_hmData <- tmax_hmData %>% 
  select(-optional)

tmax_hmData$CellID <- 1:nrow(tmax_hmData)

long_tmax <- tmax_hmData %>% 
  gather(tmax_hmData, value, -c(CellID, DEM_values_tmax, x, y)) %>% 
  mutate(year = substr(tmax_hmData,1,4),
         month = substr(tmax_hmData, 6,7),
         tempF = (value*9/5 + 32)) %>% 
  select(-tmax_hmData,-value)


write.csv(long_tmax, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmax_hm.csv")

#tmin

tmin_hmData <- read_csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/heatmapValues_tmin.csv")
names(tmin_hmData)[1:1512] <- substring(names(tmin_hmData)[1:1512],2)
tmin_hmData <- tmin_hmData %>% 
  select(-optional)

tmin_hmData$CellID <- 1:nrow(tmin_hmData)

long_tmin <- tmin_hmData %>% 
  gather(tmin_hmData, value, -c(CellID, DEM_values_tmin, x, y)) %>% 
  mutate(year = substr(tmin_hmData,1,4),
         month = substr(tmin_hmData, 6,7),
         tempF = (value*9/5 + 32)) %>% 
  select(-tmin_hmData,-value)


write.csv(long_tmin, file = "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmin_hm.csv")


######### plotting heatmaps ######### 

tmax <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmax_hm.csv")
tmin <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmin_hm.csv")
prcp <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/prcp_hm.csv")

#create bins
elevation_levels = c("338-1160", "1161-1980", "1981-2739")


tmax <- tmax%>%mutate(Elevation = cut(DEM_values_tmax, breaks = c(338,1161,1980,2739)))
tmin <- tmin%>%mutate(Elevation = cut(DEM_values_tmin, breaks = c(338,1161,1980,2739)))
prcp <- prcp%>%mutate(Elevation = cut(DEM_values_prcp, breaks = c(338,1161,1980,2739)))

#rename factor levels
tmax$Elevation <- recode(tmax$Elevation, "(338,1.16e+03]" = '338-1160',
                                         "(1.16e+03,1.98e+03]" = '1161-1980',
                                         "(1.98e+03,2.74e+03]" = '1981-2739')

tmin$Elevation <- recode(tmin$Elevation, "(338,1.16e+03]" = '338-1160',
                         "(1.16e+03,1.98e+03]" = '1161-1980',
                         "(1.98e+03,2.74e+03]" = '1981-2739')

prcp$Elevation <- recode(prcp$Elevation, "(338,1.16e+03]" = '338-1160',
                         "(1.16e+03,1.98e+03]" = '1161-1980',
                         "(1.98e+03,2.74e+03]" = '1981-2739')


#x <- ggplot(tmax, aes(x=month, y=Elevation, fill=tempF)) +
#  geom_tile(color="white") +
#  scale_fill_distiller(palette = "Greens", trans = "reverse") +
#  theme_bw(base_size = 12)
#x
 
# tmax plots
tmax$month <- as.numeric(tmax$month)
tmax$year <- as.numeric(tmax$year)

tmax <- tmax %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))
  
tmax_means <- tmax %>% 
  aggregate(by = list(tmax$Elevation, tmax$season),
            FUN = mean) %>% 
  select(Group.1, Group.2, tempF) %>% 
  rename(Elevation = Group.1,
         season = Group.2)

annual_avg_tmax <- tmax %>% 
  group_by(Elevation) %>% 
  summarise_at(vars(tempF), list(tempF = mean)) %>% 
  mutate(season = "Annual") 


tmax_plot <- rbind(tmax_means, annual_avg_tmax)

png("output/figs/tmax_heatmap.png",
    width = 900, height = 600)

ggplot(tmax_plot, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=Elevation, fill=tempF)) +
  geom_tile(color="white", size=0.2) +
  geom_text(size = 7,aes(label=round(tempF,2))) +
  guides(fill=guide_legend(title = "Average Tmax (F)", reverse = TRUE)) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Maximum Temp (F) by Season (1895-2020)",
       x = "",
       y = "Elevation (m)") +
  theme(plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90))

dev.off()

# tmin plots
tmin$month <- as.numeric(tmin$month)
tmin$year <- as.numeric(tmin$year)

tmin <- tmin %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))

tmin_means <- tmin %>% 
  aggregate(by = list(tmin$Elevation, tmin$season),
            FUN = mean) %>% 
  select(Group.1, Group.2, tempF) %>% 
  rename(Elevation = Group.1,
         season = Group.2)

annual_avg_tmin <- tmin %>% 
  group_by(Elevation) %>% 
  summarise_at(vars(tempF), list(tempF = mean)) %>% 
  mutate(season = "Annual") 


tmin_plot <- rbind(tmin_means, annual_avg_tmin)

png("output/figs/tmin_heatmap.png",
    width = 900, height = 600)

ggplot(tmin_plot, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=Elevation, fill=tempF)) +
  geom_tile(color="white", size=0.2) +
  geom_text(size = 7,aes(label=round(tempF,2))) +
  guides(fill=guide_legend(title = "Average Tmin (F)", reverse = TRUE)) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Minimum Temp (F) by Season (1895-2020)",
       x = "",
       y = "Elevation (m)") +
  theme(plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90))

dev.off()

# prcp plots
prcp$month <- as.numeric(prcp$month)
prcp$year <- as.numeric(prcp$year)

prcp <- prcp %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))

prcp_means <- prcp %>% 
  aggregate(by = list(prcp$Elevation, prcp$season),
            FUN = mean) %>% 
  select(Group.1, Group.2, inches) %>% 
  rename(Elevation = Group.1,
         season = Group.2)

annual_avg_prcp <- prcp %>% 
  group_by(Elevation) %>% 
  summarise_at(vars(inches), list(inches = mean)) %>% 
  mutate(season = "Annual") 


prcp_plot <- rbind(prcp_means, annual_avg_prcp)

png("output/figs/prcp_heatmap.png",
    width = 900, height = 600)

ggplot(prcp_plot, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=Elevation, fill=inches)) +
  geom_tile(color="white", size=0.2) +
  geom_text(size = 7,aes(label=round(inches,2))) +
  guides(fill=guide_legend(title = "Average Precip (In)", reverse = TRUE)) +
  scale_fill_distiller(palette = "BrBG", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Monthly Precipitation (In) by Season (1895-2020)",
       x = "",
       y = "Elevation (m)") +
  theme(plot.title = element_text(hjust=0.5),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 17),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90))

dev.off()
