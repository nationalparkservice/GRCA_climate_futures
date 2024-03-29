############################################################
##    REGRESSION ANALYSIS AND MAP   ########################
############################################################

# Create rasterstack of climate data for WRST (NOAA ClimGrid)
# Run linear regression analysis by pixel
# Units for raw data: temps in Celsius, precip in mm


library(sf)
library(sp)
library(dplyr)
library(raster)
library(rasterVis)
library(ggplot2)
library(grid)
library(reshape2)
library(zoo)
library(cowplot)
library(gridExtra)
library(ggpubr)

rm(list = ls())

# ---   USER INPUTS -------------------------------------- #

data.dir <- "D:/NOAA-nClimGrid" 
pnt.list<-list.files(path=data.dir, pattern=".prcp.conus") #list all files by var
#tmax, tmin, tave, prcp
# 
# var <- "prcp"
# 
plotDir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/TARs/GRCA_Climate_Report/GRCA_report_proper/Revised_Figs_ACR/" # AKD PlotDir
data.dir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/TARs/GRCA_Climate_Report/GRCA Repo Data/"
# 
# 
# # ---   INITIALS  ---------------------------------------- #
# nps_boundary <- st_read(paste0(data.dir,"Park GIS Data/nps_boundary/nps_boundary.shp")) # Directory Annie comp
site = "GRCA"
# park <- filter(nps_boundary, UNIT_CODE == site) # subset to GRCA only
# Sp_park <- as_Spatial(park) # park <- st_transform(park, st_crs(epsg))
# 
# 
# # Read in climate divisions shp
# 
# GGCL <- st_read(paste0(data.dir,"GRCA_maps/GIS/GGCL/GGCL.shp")) # greater grand canyon landscape
# GGCL <- st_transform(GGCL, crs(Sp_park)) # CONUS Albers
# 
# # read in parks shapefile
# #nps_boundary <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary2018/nps_boundary.shp') #Directory Amber comp
# 
# 
# 
# Sp_ggcl <- as_Spatial(GGCL)
# 
# 
# bbox<-data.frame(Sp_ggcl@bbox) # get bounding box
# 
# #out <- './output' 
# #if(dir.exists(out) == FALSE){
# #  dir.create(out)
# #}
# #
# #maps <- './output/maps' 
# #if(dir.exists(maps) == FALSE){
# #  dir.create(maps)
# #}
# #
# #ras <- './output/rasters' 
# #if(dir.exists(ras) == FALSE){
# #  dir.create(ras)
# #}
# 
# ##### ONLY NEED TO DO THIS THE FIRST TIME -- SUBSEQUENT TIMES REFER TO .RDS FILES
# #  #----  CREATE RASTER STACKS  ----------------------------- #
# # 
# # # Create list of tables
# # tables <- list()
# # for(i in 1:length(pnt.list)){
# #   t = read.table(paste(data.dir, pnt.list[i], sep = '/'))
# #   colnames(t) = c("Lat","Lon", var)
# #   tt = subset(t, Lat >= bbox["y","min"]-0.05 & Lat <= bbox["y","max"]+0.05 &
# #                 Lon >=bbox["x","min"]-0.05 & Lon<=bbox["x","max"]+0.05)
# #   tables[[i]] = tt 
# # }
# # 
# # # Create raster list
# # 
# # rasters <- list()
# # 
# # for(i in 1:length(tables)) {
# #   df <- tables[[i]]
# #   coordinates(df) = ~Lon+Lat
# #   proj4string(df) = "+proj=longlat +datum=WGS84 +no_defs " #same proj4string used in NPS_boundary_centroids.shp
# #   df = spTransform(df, CRSobj = "+init=epsg:4326") #reproj sp obj
# #   y = data.frame(df@coords)
# #   y$var<-df@data
# #   df = as.matrix(y)
# #   e = extent(df[,1:2])
# #   r =  raster(e,ncol=length(unique(df[,1])), nrow=length(unique(df[,2]))) # this needs to be updated for the GGCL. Not sure where these numbers come from. 
# #   x = rasterize(df[, 1:2], r, df[,3])
# #   rasters[[i]] <- x
# # }
# 
# st <- readRDS(paste0(data.dir,"nClimGrid/prcp_stack.Rds"))
# 
# #st <- stack(rasters) # Create raster stack
# plot(st[[1]])
# 
# index <- rep(1:96, each = 12)
# 
# # Summarize by year
# 
# ###############################
# ##### Run for only tempvar
# # Calculate annual means: output = rasterstack with 1 layer per year
#   
# st_mean <- stackApply(st, indices = c(rep(1:(nlayers(st)/12), each = 12)), fun = mean, na.rm = TRUE) # get annual mean first
# plot(st_mean[[1]])
# writeRaster(st_mean, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/tmax/st_mean.tif", format = "GTiff")
# 
# st_fahr <- calc(st_mean, fun = function(x){x*9/5 + 32}) # then convert to Fahrenheit
# plot(st_fahr)
# writeRaster(st_fahr, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/tmax/st_fahr.tif", format = "GTiff")
# 
# # Calculate overall mean: output = single raster with overall mean values
# 
# ras_mean <- calc(st_fahr, fun = mean)
# plot(ras_mean)
# plot(Sp_ggcl, add = TRUE)
# writeRaster(ras_mean, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/tmax/ras_mean.tif", format = "GTiff")
# 
# ##################################
# 
# # ###############################
# # ##### Run for only precip
# st_sum <- stackApply(st, indices = c(rep(1:(nlayers(st)/12), each = 12)), fun = sum, na.rm = TRUE) # get total annual precip
# plot(st_sum[[1]]) #
# writeRaster(st_sum, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/prcp_mean.tif", format = "GTiff")
# 
# # 
#  st_mean_pr_tot <- calc(st_sum, fun = mean)
#  plot(st_mean_pr_tot)
#  writeRaster(st_mean_pr_tot, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/st_mean_pr_tot.tif", format = "GTiff")
# # 
# st_in <- calc(st_sum, fun = function(x){((x)/25.4)})
# plot(st_in)
# writeRaster(st_in, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/st_in.tif", format = "GTiff")
# 
# 
# # Calculate overall mean: output = single raster with overall mean values
# 
#  ras_mean <- calc(st_in, fun = mean)
#  plot(ras_mean)
#  
#  writeRaster(ras_mean, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/ras_mean.tif", format = "GTiff")
#  
# #######################
# 
# # ------  REGRESSION  -------------------------------------------------- #
# 
# time <- 1:nlayers(st_in) # all years 1895 - 2020
# 
# # Function to calculate slope and p-value
# 
# fun <- function(y) {
#   if(all(is.na(y))) {
#     c(NA, NA)
#   } else {
#     m = lm(y ~ time) 
#     s = summary(m)
#     slope = s$coefficients[2] * 10 # change per 10 years
#     pval =  pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],lower.tail = FALSE)
#     cbind(slope, pval)
#   }
# }
# 
# r <- calc(st_in, fun)
# plot(r)
# 
# # -- PLOTTING ---------------------------------------------------------- #
# 
# # Reclassify raster so that values <= 0.05 -> 1 or else NA
# slope <- subset(r, 1)
# 
# pval <- subset(r, 2)
# pval[pval > 0.05] <- NA # Make values NA that are greater than 0.05
# 
# sig <- mask(slope, pval)
# 
# plot(sig)
# 
# # Plot park over raster
# #Sp_ggcl<-spTransform(Sp_ggcl,CRSobj = "+init=epsg:5070") # project to Conus Albers
# plot(Sp_ggcl, add = TRUE)
# 
# 
# writeRaster(sig, "C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/prcp_delta.tif", format = "GTiff") # save raster of significant slope values
# #writeRaster(sig, file = paste(plotDir,"/Rasters/tmean_delta.tif",sep=""),overwrite=TRUE) # save raster of significant slope values
# 
# # -- TIME SERIES REGRESSION ---------------------------------------------- #
# # create dfs from rasters -- run parsing script, create dataframe from cell avgs, save
# 
# yr<-seq(1895,2020,1)
# 
# #remove 1956 due to error
# yr <- yr[yr != 1956]
# 
# Sp_ggcl<-spTransform(Sp_ggcl,CRSobj = "+init=epsg:4326") # project to Alaska Albers
# 
# 
# # Precip 
# 
# m_pr <- mask(st_in, Sp_ggcl)
# plot(m_pr)
# 
# pr<-data.frame(prcp=cellStats(m_pr,stat='mean'),year=yr)
# row.names(pr)<-NULL
# write.csv(pr,"C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/prcp/prcp.csv",row.names=F)
# 
# 
# m_temp_park <- mask(st_fahr,Sp_park)
# 
# # Temp
# m_fahr <- mask(st_fahr, Sp_ggcl)
# plot(m_fahr)
# 
# fahr<-data.frame(tave=cellStats(st_fahr,stat='mean'),year=yr)
# row.names(fahr)<-NULL
# write.csv(fahr,"C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/nClimGrid/output/tmax/tmax.csv",row.names=F)
# 
# 
# m_temp_park <- mask(st_fahr,Sp_park)

# Write csv's

#pr_gulf_df <- data.frame(pr_gulf = cellStats(pr_gulf, stat = 'mean'), year = yr)
#pr_int_df <- data.frame(pr_int = cellStats(pr_int, stat = 'mean'), year = yr)
#
#row.names(pr_gulf_df)<-NULL
#row.names(pr_int_df) <- NULL 
#
#temp_gulf_df <-data.frame(temp_gulf=cellStats(temp_gulf,stat='mean'),year=yr)
#temp_int_df <- data.frame(temp_int = cellStats(temp_int, stat = 'mean', year = yr))
#
#row.names(temp_gulf_df)<-NULL
#row.names(temp_int_df) <- NULL 
#
#write.csv(temp_gulf_df,"./output/csvs/tmax_gulf.csv",row.names=F)
#write.csv(temp_int_df, "./output/csvs/tmax_int.csv", row.names = FALSE)
#
#write.csv(pr_int_df, "./output/csvs/pr_int.csv", row.names = FALSE)
#write.csv(pr_gulf_df, "./output/csvs/pr_gulf.csv", row.names = FALSE)

# ---  PLOTS ----------------------------------------- #      

# read in csvs
prcp<-read.csv(paste0(data.dir,"nClimGrid/output_corrected/prcp/prcp.csv"))
tmax<-read.csv(paste0(data.dir,"nClimGrid/output_corrected/tmax/tmax.csv"))
tmin<-read.csv(paste0(data.dir,"nClimGrid/output_corrected/tmin/tmin.csv"))
tave<-read.csv(paste0(data.dir,"nClimGrid/output_corrected/tave/tave.csv"))

# -- TIME SERIES FROM PRISM SCRIPTS
beginRefYr = 1895
endRefYr = 1970

BeginYr	= 1895   # is this data file or for plots?
EndYr = 2020
dataEndYr = 2020   # needed for rolling mean plot below.  
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

length(tave$tave) 
length(tmin$tmin) 
length(tmax$tmax) 
length(prcp$prcp) 

#add NA column to tave
new.row <- data.frame(tave = NA, year = 1956, stringsAsFactors = F)
as.integer(new.row$year)
tave <- plyr::rbind.fill(tave, new.row)
as.integer(tave$year)

#rename column issue (all temp variables called in as prcp for some reason)
#tave <- tave %>% 
#  rename(tave = prcp)
#
#tmin <- tmin %>% 
#  rename(tmin = prcp)
#
#tmax <- tmax %>% 
#  rename(tmax = prcp)
#
#prcp

# data from 1956 missing, remove rows from other data frames to make equal lengths
# 
# tmin <- subset(tmin, year != 1956)
# tmax <- subset(tmax, year != 1956)
# prcp <- subset(prcp, year != 1956)


cYr <- BeginYr:EndYr

# edit to remove 1956

#cYr <- cYr[cYr != 1956]


yrAvgs <- data.frame(cYr, tave$tave, tmin$tmin, tmax$tmax, prcp$prcp)
names(yrAvgs)<-c("cYr","tave","tmin","tmax","prcp")

rTmin <- rollmean(tmin$tmin, rollLen)
rTmax <- rollmean(tmax$tmax, rollLen)
rTmean <- rollmean(tave$tave, rollLen)
rPpt  <- rollmean(prcp$prcp, rollLen)

rYr = seq(dataEndYr - length(rTmin)+1, dataEndYr)

rDat <- data.frame(cbind(rYr, rTmin, rTmax, rTmean, rPpt))
names(rDat)[1] <- "cYr"
rDat$yr <- rDat$cYr
yrAvgs <- merge(rDat, yrAvgs, all=TRUE)

##ggplot
PlotName <- "10-yr Running Means"

#Colors for running means
######################  Periods of Analysis  ######################

p1_start  = beginRefYr
p1_end    = endRefYr
p2_start  = endRefYr
p2_end    = EndYr

yrAvgs$tmaxP1 <- yrAvgs$tmax
yrAvgs$tmaxP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tminP1 <- yrAvgs$tmin
yrAvgs$tminP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$taveP1 <- yrAvgs$tave
yrAvgs$taveP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$prcpP1 <- yrAvgs$prcp
yrAvgs$prcpP1[which(yrAvgs$cYr < p1_start | yrAvgs$cYr > p1_end)] = NA

yrAvgs$tmaxP2 <- yrAvgs$tmax
yrAvgs$tmaxP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$tminP2 <- yrAvgs$tmin
yrAvgs$tminP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$taveP2 <- yrAvgs$tave
yrAvgs$taveP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

yrAvgs$prcpP2 <- yrAvgs$prcp
yrAvgs$prcpP2[which(yrAvgs$cYr < p2_start | yrAvgs$cYr > p2_end)] = NA

########################################

# regressions for trends
lmTmax <- lm(yrAvgs$tmax~cYr)
lmTmaxP1 <- lm(yrAvgs$tmaxP1~cYr)
lmTmaxP2 <- lm(yrAvgs$tmaxP2~cYr)

lmTmin <- lm(yrAvgs$tmin~cYr)
lmTminP1 <- lm(yrAvgs$tminP1~cYr)
lmTminP2 <- lm(yrAvgs$tminP2~cYr)

lmTmean <- lm(yrAvgs$tave~cYr)
lmTmeanP1 <- lm(yrAvgs$taveP1~cYr)
lmTmeanP2 <- lm(yrAvgs$taveP2~cYr)

lmPpt  <- lm(yrAvgs$prcp~cYr)		
lmPptP1 <- lm(yrAvgs$prcpP1~cYr)		
lmPptP2 <- lm(yrAvgs$prcpP2~cYr)		

# make table of coefficients
probStar <- function(pVal){
  probStar <- "NS"
  if(pVal < 0.10)probStar <- "."
  if(pVal < 0.05)probStar <- "*"
  if(pVal < 0.01)probStar <- "**"
  if(pVal < 0.001)probStar <- "***"
  probStar
}

lmMetrics <- function(lmout){
  s <- summary(lmout)
  # equ <- as.character(s$call)
  # eq <- equ[2]
  YrCoeff <- s$coefficients[2,1]
  ses <- coef(s)[,"Std. Error"]   # gets intercept & slope
  seSlope <- ses[2]
  probCoeff <- s$coefficients[2,4]
  probSign <- probStar(probCoeff)
  r2 <- s$r.squared
  data.frame(YrCoeff,seSlope,probCoeff, probSign, r2)
}

regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP1), lmMetrics(lmTmaxP2))
regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP1), lmMetrics(lmTminP2))
regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP1),lmMetrics(lmTmeanP2))
regsPpt <-   rbind(lmMetrics(lmPpt),  lmMetrics(lmPptP1),  lmMetrics(lmPptP2))

perAll <- paste(min(yrAvgs$cYr), max(yrAvgs$cYr), sep="-")
per1 <- paste(p1_start, p1_end, sep="-")
per2 <- paste(p2_start, p2_end, sep="-")
Period <- rep(c(perAll, per1, per2), 4)

lmTable <- cbind( Var=rep(c("Tmax", "Tmin", "Tmean", "Precip"),each=3), Period, rbind(regsTmax, regsTmin, regsTmean, regsPpt))

lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
lmTable$seSlope <- lmTable$seSlope * 100
#add units to YrCoeff field
colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")

print(lmTable, row.names = F)

# write.csv(lmTable, paste(plotDir, site, " Regression Table test ", Sys.Date(), ".csv", 
#                          sep=""), row.names=FALSE)


#-----------------------------------------------------------#
#            ANNUAL AVERAGE LINES WITH REGRESSION           #
#-----------------------------------------------------------#
# Amend to incl. running means and dashed lines for significance
#    ggplot graphics    #
# annaul points, linear regression, and 95% CI
# need to manually set position of a, b, c labels

# gray zone is 95% confidence interval
doP1 <- "NO"  # Should a separate regression be calculated for the reference period (default 1900-1970)? 
doP2 <- "YES"  # Should a separate regression be calculate for the period after the reference period (default 1971-present)? 

PlotName = "Annual Means Lines Regressions"

a <- ggplot(yrAvgs) +	geom_smooth(method = lm, aes(cYr, tmax), na.rm=TRUE,linetype=if(summary(lmTmax)$coefficients[2,4]<0.1) {
  1
} else{2}) +
  geom_line(aes(cYr, tmax), na.rm=TRUE) + geom_point(aes(cYr, tmax), na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") +
  scale_x_continuous(breaks=c(1930, 1950, 1970, 1990, 2010)) 
  # geom_line(aes(cYr, rTmax), colour = 'brown', size=1)  # rolling mean
if(doP1 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP1), na.rm=TRUE,linetype=if(summary(lmTmaxP1)$coefficients[2,4]<0.1) {
  1
} else{2})
if(doP2 == "YES")a <- a + geom_smooth(method = lm, aes(cYr, tmaxP2), na.rm=TRUE,linetype=if(summary(lmTmaxP2)$coefficients[2,4]<0.1) {
  1
} else{2})
a

b <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tmin), na.rm=TRUE) + geom_point(aes(cYr, tmin), na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(cYr, tmin), method="lm", na.rm=TRUE,linetype=if(summary(lmTmin)$coefficients[2,4]<0.1) {
    1
  } else{2}) +
  scale_x_continuous(breaks=c(1930, 1950, 1970, 1990, 2010)) 
  # geom_line(aes(cYr, rTmin), colour = 'brown', size=1)

if(doP1 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP1), na.rm=TRUE,linetype=if(summary(lmTminP1)$coefficients[2,4]<0.1) {
  1
} else{2})
if(doP2 == "YES")b <- b +	geom_smooth(method = lm, aes(cYr, tminP2), na.rm=TRUE,linetype=if(summary(lmTminP2)$coefficients[2,4]<0.1) {
  1
} else{2})
b

c <- ggplot(data=yrAvgs) + geom_line(aes(cYr, tave), na.rm=TRUE) + geom_point(aes(cYr, tave), na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(cYr, tave), method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.1) {
    1
  } else{2}) +
  # geom_line(aes(cYr, rTmean), colour = 'brown', size=1) +
  scale_x_continuous(breaks=c(1930, 1950, 1970, 1990, 2010))

if(doP1 == "YES")c <- c + geom_smooth(method = lm, aes(cYr, taveP1), na.rm=TRUE,linetype=if(summary(lmTmeanP1)$coefficients[2,4]<0.1) {
  1
} else{2})
if(doP2 == "YES") c <- c + geom_smooth(method = lm, aes(cYr, taveP2), na.rm=TRUE,linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.1) {
  1
} else{2}) 

d <- ggplot(data=yrAvgs) + geom_line(aes(cYr, prcp), na.rm=TRUE) + geom_point(aes(cYr, prcp), na.rm=TRUE) +
  ylab("Precip (in/yr)") + xlab("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(aes(cYr, prcp), method="lm", na.rm=TRUE,linetype=if(summary(lmPpt)$coefficients[2,4]<0.1) {
    1
  } else{2}) +
  # geom_line(aes(cYr, rPpt), colour = 'brown', size=1) +
  scale_x_continuous(breaks=c(1930, 1950, 1970, 1990, 2010))

if(doP1 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, prcpP1), na.rm=TRUE,linetype=if(summary(lmPptP1)$coefficients[2,4]<0.1) {
  1
} else{2})
if(doP2 == "YES")d <- d + geom_smooth(method = lm, aes(cYr, prcpP2), na.rm=TRUE,linetype=if(summary(lmPptP2)$coefficients[2,4]<0.1) {
  1
} else{2}) 


#tmean, tmax, tmin		
g <- grid.arrange(c, a,b,nrow=3)
ggsave(plot=g,paste0(plotDir,"TS-Tmean-Tmax-Tmin-regression.png"), width=6.5, height=6.5, dpi=dpi,bg="white")

#prcp
g <- grid.arrange(d,nrow=1)
ggsave(plot=g,paste0(plotDir,"TS-Precip-regression.png"), width=6.5, height=2.9, dpi=dpi,bg="white")

