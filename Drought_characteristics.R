library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(forcats)
library(reshape2)
library(zoo)
library(SPEI) # Make sure to install this pkg!
library(gridExtra)
library(grid)
library(lemon)
library(ggpubr)
# library(WaterBalance)

rm(list=ls())
plotDir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/TARs/GRCA_Climate_Report/GRCA_report_proper/Revised_Figs_ACR/" # AKD PlotDir

### Drought timeseries bar plot function
#Height and width 
PlotWidth = 15
PlotHeight = 9

#Panel plot width and height
PanelWidth = 15
PanelHeight = 15

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom")  

BarPlotTheme = theme(axis.text.x=element_text(size=24),    #Text size for axis tick mark labels
                     axis.text.y=element_text(size=20),
                     axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                     axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                     plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                     legend.position = "none") 

SPEI_annual_bar <- function(data, period.box=T, title){
  ggplot(data = data, aes(x=as.numeric(as.character(Year)), y=SPEI,fill = col)) + 
    {if(period.box==T) geom_rect(xmin=Yr-Range/2, xmax=Yr+Range/2, ymin=-Inf, ymax=Inf, alpha=0.1, fill="darkgray", col="darkgray")} +
    geom_bar(stat="identity",aes(fill=col),col="black") + 
    geom_hline(yintercept=-.5,linetype=2,colour="black",size=1) +
    scale_fill_manual(name="",values =c("turquoise2","orange1")) +
    labs(title = title, 
         x = "Date", y = "SPEI") +
    guides(color=guide_legend(override.aes = list(size=7))) + PlotTheme
}

var_bar_plot <- function(data,var, cols, title, ylab){
  At<-aggregate(eval(parse(text=var))~CF,data=data,mean);
  names(At)<-c("CF",var)
  ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) +
    geom_bar(stat="identity",position="dodge",colour="black") +
    BarPlotTheme +
    # coord_cartesian(ylim=c(0, 40)) +
    labs(title = title, 
         y = ylab, colour = "Climate Future")  +
    scale_fill_manual(name="",values = cols) +
    coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep="")))), max(eval(parse(text=paste("At$",var,sep=""))))))
}

############################### FORMAT DATAFRAMES  ############################################
Yr = 2055
Range = 30
Lat=  36.2135
Location = "North Rim"
GCMs = c("MRI-CGCM3.rcp85","MIROC-ESM-CHEM.rcp85") # per teams chat 6/23/20 with AR
CFs<- c("Warm Wet", "Hot Dry")
CF_GCM <- data.frame(CF = CFs, GCM = GCMs)

SPEI_start <- 1981
SPEI_end <- 2010

## SPEI variables
SPEI_per<-6 # This is the value for the period SPEI is aggregated. 6-months is standard but could change if want. 
truncation<- -.5 # SPEI value, under which drought events are considered


## All 508-compliant color scheme -- navy (hot wet), light blue (warm wet), pink (warm dry), red (hot dry)
colors2<- c("#9A9EE5","#E10720")  # WarmWet/HotDry
# colors2<- c("#F3D3CB","#12045C")  # HotWet/WarmDry

colors3<-c("white",colors2)

# Set wd for saving plots
FigDir<-("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/GRCA_CCSP/MACA point/Drought/north-rim")

DataDir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/GRCA_CCSP/MACA point/"
Gridmet <- read.csv(paste0(DataDir,"GridMet-north-rim.csv"),header=T)

# Historical
Grid <- Gridmet
ALL_FUTURE <- read.csv(paste0(DataDir,"grca-north-rim.csv"),header=T)
ALL_FUTURE$tmean_K = (ALL_FUTURE$tmax_k + ALL_FUTURE$tmin_k)/2
ALL_FUTURE$tmean_F =  (9/5) * (ALL_FUTURE$tmean_K - 273) + 32
ALL_FUTURE$precip_in = ALL_FUTURE$precip_mm/25.4

Gridmet$Date<-ymd(Gridmet$Date)
Gridmet$Month<-format(Gridmet$Date,format="%m")
Gridmet$Year<-format(Gridmet$Date,format="%Y")
Gridmet <- subset(Gridmet, Year >= 1981 & Year <= 2020)

Gridmet$TavgC<-(((Gridmet$tmax+Gridmet$tmin)/2)-32)*5/9
Gridmet$Prcpmm<-Gridmet$precip*25.4
d<-aggregate(Prcpmm~Month+Year,Gridmet,sum)
d2<-aggregate(TavgC~Month+Year,Gridmet,mean)
drt<-merge(d,d2,by=c("Month","Year"));rm(d,d2)
drt<-drt[with(drt, order(Year, Month)),]
drt$PET<-thornthwaite(drt$TavgC,lat = Lat)

# Run SPEI on gridmet
tp<-ts(drt$Prcpmm,frequency=12,start=c(1979,1))
tpet<-ts(drt$PET,frequency=12,start=c(1979,1))
SPEI<-spei(tp - tpet, SPEI_per, ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
plot(x=SPEI,main="Gridmet") #eventually prob want to figure out how to make x-axis date

drt$SPEI<-SPEI$fitted;drt$SPEI[which(is.na(drt$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
names(drt)[6]<-"SPEI"
drt3<-aggregate(cbind(Prcpmm,SPEI)~Year,drt,mean)

ALL_FUTURE$GCM <- sub("_rcp",".rcp",ALL_FUTURE$GCM)
ALL_FUTURE$Date <- as.Date(ALL_FUTURE$Date,format = "%m/%d/%Y")
ALL_FUTURE$Month<-format(ALL_FUTURE$Date,format="%m")
ALL_FUTURE$Year<-format(ALL_FUTURE$Date,format="%Y")
ALL_FUTURE$Prcpmm<-ALL_FUTURE$precip_mm
ALL_FUTURE$TavgC<-(ALL_FUTURE$tmean_F-32)*5/9
ALL_FUTURE$PET<-thornthwaite(ALL_FUTURE$TavgC,lat=Lat)

Mon <- subset(ALL_FUTURE, as.numeric(Year)>2020,select=c("Month","Year","GCM","TavgC","Prcpmm","PET"))

drt$GCM<-GCMs[1]
d<-drt; d$GCM <- GCMs[2]
drt<-rbind(drt,d)
d<-subset(drt,  select=c("Month","Year","GCM","TavgC","Prcpmm","PET"))
Mon <- rbind(d,Mon)

Mon<-merge(Mon,CF_GCM,by="GCM")
Mon$CF<-factor(Mon$CF,levels=unique(Mon$CF))
MON<-aggregate(cbind(Prcpmm,PET)~Month+Year+CF,Mon,mean) 
MON<-MON[with(MON, order(CF,Year, Month)),]

CF.split<-split(MON,MON$CF) #Splits df into array by CF
# this step is done because each CF has unique historical record and SPEI normalized to average conditions at beginning of record

for (i in 1:length(CF.split)){
  name=names(CF.split)[i]
  t<-CF.split[[i]]
  tp<-ts(t$Prcpmm,frequency=12,start=c(SPEI_start,1))
  tpet<-ts(t$PET,frequency=12,start=c(SPEI_start,1))
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
  CF.split[[i]]$SPEI <- SPEI$fitted[1:length(SPEI$fitted)]
  plot(x=SPEI,main=name) #eventually prob want to figure out how to make x-axis date
}

all2<- ldply(CF.split, data.frame) #convert back to df
all2$SPEI[which(is.na(all2$SPEI))]<-0 #records used to normalize data are NAs - convert to 0s
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(Prcpmm,SPEI)~Year+CF,all2,mean)

###################################### PLOT ANNUAL TIME-SERIES #################################################
# Gridmet
drt3$col[drt3$SPEI>=0]<-"wet"
drt3$col[drt3$SPEI<0]<-"dry"
drt3$col<-factor(drt3$col, levels=c("wet","dry"))

SPEI_annual_bar(drt3, period.box=F,title="SPEI values for Historical Period (gridMET)")
ggsave("Annual-bar-SPEI-gridmet.png", path = FigDir, width = PlotWidth, height = PlotHeight)

# MACA prep dataframe
all3$col[all3$SPEI>=0]<-"wet"
all3$col[all3$SPEI<0]<-"dry"
all3$col<-factor(all3$col, levels=c("wet","dry"))
all3$Year<-as.numeric(all3$Year)

# CF 
CF1<-subset(all3, CF %in% CFs[1] )

SPEI_annual_bar(subset(CF1,Year>=Yr-Range/2 & Year<=Yr+Range/2), period.box=T,
                title=paste("SPEI values for", CFs[1], "climate future", sep = " " )) 
ggsave("Annual-bar-SPEI-CF1.png", path = FigDir, width = PlotWidth, height = PlotHeight)

SPEI_annual_bar(CF1, period.box=T,
                title=paste("SPEI values for", CFs[1], "climate future", sep = " " )) 
ggsave("Annual-bar-SPEI-CF1-gridmet.png", path = FigDir, width = PlotWidth, height = PlotHeight)

# CF 2
CF2<-subset(all3, CF %in% CFs[2] )

SPEI_annual_bar(subset(CF2,Year>=Yr-Range/2 & Year<=Yr+Range/2), period.box=T,
                title=paste("SPEI values for", CFs[2], "climate future", sep = " " )) 
ggsave("Annual-bar-SPEI-CF2.png", path = FigDir, width = PlotWidth, height = PlotHeight)

SPEI_annual_bar(CF2, period.box=T,
                title=paste("SPEI values for", CFs[2], "climate future", sep = " " )) 
ggsave("Annual-bar-SPEI-CF2-gridmet.png", path = FigDir, width = PlotWidth, height = PlotHeight)


# Split into periods
drt3<-subset(drt3, Year >=1981 & Year <=2010)
min(drt3$SPEI)

Future.drt<-subset(all3, Year >= Yr-Range/2 & Year <= Yr+Range/2)
min(Future.drt$SPEI)

# Calculate drought characteristics
drt3$Drought=0
drt3$Drought[which(drt3$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(drt3)

# Create count of years within CF
length(drt3$Year)
drt3$count<-seq(1, length(drt3$Year),1) 

drt3$length<-0
drt3$length <- drt3$Drought * unlist(lapply(rle(drt3$Drought)$lengths, seq_len))
mean(drt3$length[drt3$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(drt3$length==1)
HistoricalDrought<-data.frame()
HistoricalDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
HistoricalDrought$Start = Sys.time(); HistoricalDrought$End = Sys.time()
HistoricalDrought$per<-as.factor("H")


# Calculate variables for each drought period
for (i in 1:length(D)){
  HistoricalDrought$DID[i]<-i
  HistoricalDrought$Start[i]<-strptime(drt3$Date[D[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$Year[i]<-drt3$Year[D[i]]
}

ND<- which((drt3$length == 0) * unlist(lapply(rle(drt3$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(drt3$Drought[length(drt3$Drought)]==1) ND[length(ND)+1]<-length(drt3$length)

###### !!!!!!!!!!! 
# If last row in drought df is a drought period - use next line of code. Otherwies proceed.
# ND[length(ND)+1]<-length(drt3$length) #had to add this step because last drought went until end of df so no end in ND

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  HistoricalDrought$End[i]<-strptime(drt3$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  HistoricalDrought$duration[i]<-drt3$length[ND[i]-1]
  HistoricalDrought$severity[i]<-sum(drt3$SPEI[D[i]:(ND[i]-1)])
  HistoricalDrought$peak[i]<-min(drt3$SPEI[D[i]:(ND[i]-1)])
}

## Freq
d<-which(drt3$length==1)
nd<-which((drt3$length == 0) * unlist(lapply(rle(drt3$length)$lengths, seq_len)) == 1)
if(length(nd)>length(d)) {nd=nd[2:length(nd)]}
for (j in 1:length(d)){
  HistoricalDrought$freq[which(HistoricalDrought$Year==drt3$Year[d[j]])] <-
    drt3$count[d[j+1]]-drt3$count[nd[j]]
}

####### Future
# Calculate drought characteristics
Future.drt$Drought=0
Future.drt$Drought[which(Future.drt$SPEI < truncation)] <- 1

# Drought Duration calculation
# 1 Create var for beginnign drought and var for end drought, then count months between
head(Future.drt)

# Create count of months within CF
length(Future.drt$CF)/length(unique(Future.drt$CF))
Future.drt$count<-rep(seq(1, length(Future.drt$CF)/length(unique(Future.drt$CF)), 
                          1),length(unique(Future.drt$CF))) # repeat # of CFs 

Future.drt$length<-0
Future.drt$length <- Future.drt$Drought * unlist(lapply(rle(Future.drt$Drought)$lengths, seq_len))
mean(Future.drt$length[Future.drt$length>0])

# To get duration, now just remove those that are not droughts and do calculations on length

# Give each drought period an ID
D<-which(Future.drt$length==1)
FutureDrought<-data.frame()
FutureDrought<-setNames(data.frame(matrix(ncol=10,nrow=length(D))),c("DID","Start","End","Year","per","CF","duration","severity","peak","freq"))
FutureDrought$Start = Sys.time(); FutureDrought$End = Sys.time()
FutureDrought$per<-as.factor("F")


# Calculate variables for each drought period
for (i in 1:length(D)){
  FutureDrought$DID[i]<-i
  FutureDrought$Start[i]<-strptime(Future.drt$Date[D[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$Year[i]<-Future.drt$Year[D[i]]
}

ND<- which((Future.drt$length == 0) * unlist(lapply(rle(Future.drt$length)$lengths, seq_len)) == 1)
if(ND[1]==1) ND<-ND[2:length(ND)]
if(Future.drt$Drought[length(Future.drt$Drought)]==1) ND[length(ND)+1]<-length(Future.drt$length)

#Duration # months SPEI < truncation; Severity # Sum(SPEI) when SPEI < truncation; Peak # min(SPEI) when SPEI < truncation

for (i in 1:length(ND)){
  FutureDrought$CF[i]<-as.character(Future.drt$CF[D[i]])
  FutureDrought$End[i]<-strptime(Future.drt$Date[ND[i]],format="%Y-%m-%d",tz="MST")
  FutureDrought$duration[i]<-Future.drt$length[ND[i]-1]
  FutureDrought$severity[i]<-sum(Future.drt$SPEI[D[i]:(ND[i]-1)])
  FutureDrought$peak[i]<-min(Future.drt$SPEI[D[i]:(ND[i]-1)])
}
FutureDrought$CF<-as.factor(FutureDrought$CF)

## Freq

CF.split<-split(Future.drt,Future.drt$CF)
for (i in 1:length(CF.split)){
  name=as.character(unique(CF.split[[i]]$CF))
  d<-which(CF.split[[i]]$length==1)
  nd<-which((CF.split[[i]]$length == 0) * unlist(lapply(rle(CF.split[[i]]$length)$lengths, seq_len)) == 1)
  if(length(nd)>length(d)) {nd=nd[2:length(nd)]}
  for (j in 1:length(d)){
    FutureDrought$freq[which(FutureDrought$CF==name & FutureDrought$Year==CF.split[[i]]$Year[d[j]])] <-
      CF.split[[i]]$count[d[j+1]]-CF.split[[i]]$count[nd[j]]
  }
}

head(HistoricalDrought)
head(FutureDrought)
Drought<-rbind(HistoricalDrought,FutureDrought)
write.csv(Drought,paste0(FigDir,"Drt.all.csv"),row.names=FALSE)  # csv with all drought events

Hist_char<-setNames(data.frame(matrix(ncol=6,nrow=1)),c("CF","per","Duration","Severity","Intensity","Frequency"))
Hist_char$CF<-"Historical"
Hist_char$per<-"H"
Hist_char$Frequency<-mean(HistoricalDrought$freq,na.rm=TRUE)
Hist_char$Duration<-mean(HistoricalDrought$duration)
Hist_char$Severity<-mean(HistoricalDrought$severity)
Hist_char$Intensity<-mean(HistoricalDrought$peak)
Hist_char$Drt.Free <- mean(rle(drt3$length)$lengths[which(rle(drt3$length)$values==0)])


Drought_char<-setNames(data.frame(matrix(ncol=6,nrow=length(levels(FutureDrought$CF)))),c("CF","per","Duration","Severity","Intensity","Frequency"))
Drought_char$CF<-levels(FutureDrought$CF)
Drought_char$per<-"F"
for (i in 1:length(Drought_char$CF)){
  name<-Drought_char$CF[i]
  Drought_char$Frequency[i]<-mean(FutureDrought$freq[which(FutureDrought$CF == name)],na.rm=TRUE)
  Drought_char$Duration[i]<-mean(FutureDrought$duration[which(FutureDrought$CF == name)])
  Drought_char$Severity[i]<-mean(FutureDrought$severity[which(FutureDrought$CF == name)])
  Drought_char$Intensity[i]<-mean(FutureDrought$peak[which(FutureDrought$CF == name)])
  Drought_char$Drt.Free[i]<-mean(rle(subset(Future.drt,CF==name)$length)$lengths[which(rle(subset(Future.drt,CF==name)$length)$values==0)])
}

Drought_char<-rbind(Hist_char,Drought_char) 

# csv for averages for each CF for hist and future periods
write.csv(Drought_char,paste0(FigDir,"Drought_char.csv"),row.names=FALSE)


########################################### BAR PLOTS ###############################################
#Drought duration barplot
Drought_all = Drought_char
Drought_all$CF = factor(Drought_all$CF, levels = c("Historical",CFs))
Drought_all <- merge(Drought_all, CF_GCM,by="CF",all.x=T)
Drought_all$GCM[is.na(Drought_all$GCM)]="Historical"
Drought_all$GCM <- factor(Drought_all$GCM,levels= c("Historical",GCMs))
colnames(Drought_all)[1]<-"CF1"
Drought_all$CF<-Drought_all$GCM

#Drought duration barplot
var_bar_plot(Drought_all,"Duration", colors3, "Average Drought Duration", "Years")
ggsave("Bar-DroughtDuration.png", path = FigDir, height=PlotHeight, width=PlotWidth)

#Drought severity barplot
var_bar_plot(Drought_all,"Severity", colors3, "Average Drought Severity", 
             "Severity (Intensity * Duration)")
ggsave("Bar-DroughtSeverity.png", path = FigDir, height=PlotHeight, width=PlotWidth)

#Drought intensity barplot
var_bar_plot(Drought_all,"Intensity", colors3, "Average Drought Intensity", 
             "Intensity (Minimum SPEI values)")
ggsave("Bar-DroughtIntensity.png", path = FigDir, height=PlotHeight, width=PlotWidth)

#Drought-free interval barplot
var_bar_plot(Drought_all,"Drt.Free", colors3, "Average Drought-Free Interval", 
             "Years")
ggsave("Bar-DroughtFrequency.png", path = FigDir, height=PlotHeight, width=PlotWidth)


####################################### REPORT FIGURES ##############################################
# Option 1
a <- SPEI_annual_bar(CF1, period.box=T,
                     title=GCMs[1]) 
b <- SPEI_annual_bar(CF2, period.box=T,
                     title=GCMs[2])

c <- var_bar_plot(Drought_all,"Duration", colors3, "Duration", "Years")
d <- var_bar_plot(Drought_all,"Drt.Free", colors3, "Drought-free\ninterval", 
                  "Years")
e<- var_bar_plot(Drought_all,"Severity", colors3, "Severity", 
                 "Severity (Intensity * Duration)")

spei.time <- grid_arrange_shared_legend(a + rremove("ylab") + rremove("x.text"),b +  rremove("ylab"),
                                        nrow=2,ncol=1,position="bottom")

spei.time <- annotate_figure(spei.time, left = textGrob("SPEI", rot = 90, vjust = 1, gp = gpar(cex = 2)))

drt.char <- grid.arrange(c+rremove("x.text"),d+rremove("x.text"),e,nrow=3,
                         top = textGrob("Average drought characteristics",gp=gpar(fontface="bold", col="black", fontsize=26,hjust=0.5)))

g <- grid.arrange(spei.time, drt.char,ncol = 2, clip = FALSE)
ggsave("Panel-DroughtCharacteristics-1.png",g, path = plotDir, height=PanelHeight, width=PanelWidth)


# Option 2
c <- c+ theme(legend.title=element_text(size=24),legend.text=element_text(size=22),legend.position = "bottom")
d <- d+ theme(legend.title=element_text(size=24),legend.text=element_text(size=22),legend.position = "bottom")
e <- e+ theme(legend.title=element_text(size=24),legend.text=element_text(size=22),legend.position = "bottom")

drt.char <-grid_arrange_shared_legend(c+ rremove("x.text"),d+ rremove("x.text"),e+ rremove("x.text"),
                                      ncol=3,nrow=1,position="bottom",
                                      top = textGrob("Average drought characteristics",gp=gpar(fontface="bold", col="black", fontsize=26,hjust=0.5)))
g <- grid.arrange(spei.time, drt.char,nrow=2,ncol = 1, clip = FALSE)
ggsave("Panel-DroughtCharacteristics-2.png",g, path = plotDir, height=PanelHeight, width=PanelWidth)   
