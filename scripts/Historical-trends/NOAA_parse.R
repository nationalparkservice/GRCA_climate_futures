# Author - Amber Runyon, 2021-08-12
# Must have NOAA nClimGrid data downloaded locally - See script:
# https://github.com/CCRP-Adaptation/noaa-nClimGrid/blob/main/scripts/download-untar.R

library(sf)
library(sp)
library(dplyr)
library(raster)
library(rasterVis)
library(ggplot2)
library(grid)
library(cowplot)
library(reshape2)
library(zoo)

rm(list = ls())

# ---   USER INPUTS -------------------------------------- #

data.dir <- "D:/nClimGrid" 
pnt.list<-list.files(path=data.dir, pattern=".prcp.alaska") #list all files by var
#tmax, tmin, tave, prcp

var <- "prcp"

plotDir <- "./output/plots" # AKD PlotDir


# ---   INITIALS  ---------------------------------------- #

site = "WRST"

# Read in climate divisions shp

div <- st_read('./data/spatial-data/AK_climate_divisions/AK_divisions_NAD83.shp') # citation: Bieniek et al. 2012
div <- st_transform(div, 3338) # Alaska Albers

div_gulf <- filter(div, Name == "Northeast Gulf") 
div_int <- filter(div, Name == "Southeast Interior") 

# read in parks shapefile
nps_boundary <- st_read('./data/spatial-data/nps_boundary') # Directory Annie comp
#nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary2018/nps_boundary.shp') #Directory Amber comp

park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
Sp_park <- as_Spatial(park) # park <- st_transform(park, st_crs(epsg))

bbox<-data.frame(Sp_park@bbox) # get bounding box

# out <- './output' 
# if(dir.exists(out) == FALSE){
#   dir.create(out)
# }
# 
# maps <- './output/maps' 
# if(dir.exists(maps) == FALSE){
#   dir.create(maps)
# }
# 
# ras <- './output/rasters' 
# if(dir.exists(ras) == FALSE){
#   dir.create(ras)
# }

# ----  CREATE RASTER STACKS  ----------------------------- #

# Create list of tables

tables <- list()

for(i in 1:length(pnt.list)){
  t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
  colnames(t) = c("Lat","Lon", var)
  tt = subset(t, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
                Lon >=bbox["x","min"] & Lon<=bbox["x","max"])
  tables[[i]] = tt 
}

# Create raster list

rasters <- list()

for(i in 1:length(tables)) {
  df <- tables[[i]]
  coordinates(df) = ~Lon+Lat
  proj4string(df) = "+proj=longlat +datum=WGS84 +no_defs " #same proj4string used in NPS_boundary_centroids.shp
  df = spTransform(df, CRSobj = "+init=epsg:3338") #reproj sp obj
  y = data.frame(df@coords)
  y$var<-df@data
  df = as.matrix(y)
  e = extent(df[,1:2])
  r =  raster(e, ncol=85, nrow=71)
  x = rasterize(df[, 1:2], r, df[,3])
  rasters[[i]] <- x
}

st <- stack(rasters) # Create raster stack

index <- rep(1:96, each = 12)

# Summarize by year

###############################
##### Run for only tempvar
# Calculate annual means: output = rasterstack with 1 layer per year

st_mean <- stackApply(st, indices = c(rep(1:96, each = 12)), fun = mean, na.rm = TRUE) # get annual mean first
plot(st_mean[[1]])
st_fahr <- calc(st_mean, fun = function(x){x*9/5 + 32}) # then convert to Fahrenheit
plot(st_fahr)

# Calculate overall mean: output = single raster with overall mean values

#ras_mean <- calc(st_fahr, fun = mean)
#plot(ras_mean)

##################################

# ###############################
# ##### Run for only precip
st_sum <- stackApply(st, indices = c(rep(1:96, each = 12)), fun = sum, na.rm = TRUE) # get total annual precip
# plot(st_sum[[1]]) #
# 
# st_mean_pr_tot <- calc(st_sum, fun = mean)
# plot(st_mean_pr_tot)
# 
st_in <- calc(st_sum, fun = function(x){((x)/25.4)})
plot(st_in)


# Calculate overall mean: output = single raster with overall mean values

# ras_mean <- calc(st_in, fun = mean)
# plot(ras_mean)

#######################

# ------  REGRESSION  -------------------------------------------------- #

time <- 1:nlayers(st_in) # all years 1925 - 2020

# Function to calculate slope and p-value

fun <- function(y) {
  if(all(is.na(y))) {
    c(NA, NA)
  } else {
    m = lm(y ~ time) 
    s = summary(m)
    slope = s$coefficients[2] * 100 # change per 100 years
    pval =  pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],lower.tail = FALSE)
    cbind(slope, pval)
  }
}

# <- calc(st_fahr, fun)
#plot(r)

# -- PLOTTING ---------------------------------------------------------- #

# Reclassify raster so that values <= 0.05 -> 1 or else NA
#slope <- subset(r, 1)

#pval <- subset(r, 2)
#pval[pval > 0.1] <- NA # Make values NA that are greater than 0.05

#sig <- mask(slope, pval)

#plot(sig)

# Plot park over raster
#Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") # project to Alaska Albers
#plot(Sp_park, add = TRUE)


# writeRaster(sig, file = "./output/rasters/precip_delta.tif") # save raster of significant slope values
# writeRaster(sig, file = paste(plotDir,"/Rasters/tmean_delta.tif",sep=""),overwrite=TRUE) # save raster of significant slope values

# -- TIME SERIES REGRESSION ---------------------------------------------- #
# create dfs from rasters -- run parsing script, create dataframe from cell avgs, save

yr<-seq(1925,2020,1)
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:3338") # project to Alaska Albers


# Precip 

m_pr <- mask(st_in, Sp_park)
plot(m_pr)

pr_gulf <- mask(m_pr, div_gulf)
plot(pr_gulf)

pr_int <- mask(m_pr, div_int)
plot(pr_int)

# pr<-data.frame(prcp=cellStats(m_pr,stat='mean'),year=yr)
# row.names(pr)<-NULL
# write.csv(pr,"prcp.csv",row.names=F)


m_temp_park <- mask(st_fahr,Sp_park)

temp_gulf <- mask(m_temp_park, div_gulf)
plot(temp_gulf)

temp_int <- mask(m_temp_park, div_int)
plot(temp_int)

# Write csv's

pr_gulf_df <- data.frame(pr_gulf = cellStats(pr_gulf, stat = 'mean'), year = yr)
pr_int_df <- data.frame(pr_int = cellStats(pr_int, stat = 'mean'), year = yr)

row.names(pr_gulf_df)<-NULL
row.names(pr_int_df) <- NULL 

temp_gulf_df <-data.frame(temp_gulf=cellStats(temp_gulf,stat='mean'),year=yr)
temp_int_df <- data.frame(temp_int = cellStats(temp_int, stat = 'mean', year = yr))

row.names(temp_gulf_df)<-NULL
row.names(temp_int_df) <- NULL 

write.csv(temp_gulf_df,"./output/csvs/tmax_gulf.csv",row.names=F)
write.csv(temp_int_df, "./output/csvs/tmax_int.csv", row.names = FALSE)

write.csv(pr_int_df, "./output/csvs/pr_int.csv", row.names = FALSE)
write.csv(pr_gulf_df, "./output/csvs/pr_gulf.csv", row.names = FALSE)

# ---  PLOTS ----------------------------------------- #      

# read in csvs
#prcp<-read.csv("prcp.csv")
#tmax<-read.csv("tmax.csv")
#tmin<-read.csv("tmin.csv")
#tave<-read.csv("./output/csvs/tave_gulf.csv")

# -- TIME SERIES FROM PRISM SCRIPTS
beginRefYr = 1925
endRefYr = 1970

BeginYr	= 1925   # is this data file or for plots?
EndYr = 2020
dataEndYr = 2019   # needed for rolling mean plot below.  
stepYrs	= 10		  # for period plots 
rollLen = 10      # period of calc for rolling average; default 10 = decadal

dpi = 600    


##ggplot theme for all plots
#Theme for all plots
PlotTheme = theme_gray() %+replace% 
  theme(plot.title = element_text(size=18, face='bold', hjust=0.5, vjust=0.5),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.title.y = element_text(size = 18, angle = 90, margin=margin(0,5,0,0)),
        axis.text.x = element_text(size = 16, colour="black"),
        axis.title.x = element_text(size = 18, margin=margin(5,0,0,0)),
        legend.position = "none",
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

theme_set(PlotTheme)
TitleSize = theme_get()$plot.title$size  ##Needed for cowplot layouts


#-------------------------------------------------#
############  Running average plots   #############
#-------------------------------------------------#

# Gulf

tave<-read.csv("./output/csvs/tave_int.csv")
prcp<-read.csv("./output/csvs/pr_int.csv")
tmax<-read.csv("./output/csvs/tmax_int.csv")
tmin<-read.csv("./output/csvs/tmin_int.csv")

tave$tave <- tave$temp_int
tmin$tmin <- tmin$temp_int
tmax$tmax <- tmax$temp_int
prcp$prcp <- prcp$pr_int