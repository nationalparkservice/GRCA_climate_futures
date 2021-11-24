library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ncdf4)
library(reshape2)


rm(list=ls())

######### INITIALS #########

##Color schemes

##Plot parameters

#Height and width 
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=22),                                                                   #Text size of legend title
                  legend.position = "bottom")                                                                            #Legend position


SiteID = "GRCA"

#Directory and RData file where daily data series is stored
DataDir = "C:/Users/achildress/OneDrive - DOI/Documents/Projects/GRCA/MACA/"
DataFile = "GRCA_init_parsed.RData"

#Year range for summarizing future climate (Year - Range/2) to (Year + Range/2)
Year = 2055 #Central year
Range = 30  #Number of years to summarize (should be at least 30)

load(paste0(DataDir,DataFile))

Baseline_all$Date = strptime(Baseline_all$Date, "%Y-%m-%d")
Future_all$Date = strptime(Future_all$Date, "%Y-%m-%d")

# # Subset Future_all to only be near future (2025-2055) and Baseline_all to only but until 2000
BA<-Baseline_all
Baseline_all$GCM<-paste(Baseline_all$GCM,".rcp45",sep="");BA$GCM<-paste(BA$GCM,".rcp85",sep="")
Baseline_all<-rbind(Baseline_all,BA);rm(BA)
ALL_HIST<-Baseline_all
Baseline_all$Year<-format(as.Date(Baseline_all$Date, format="%Y-%m-%d"),"%Y")
Baseline_all<-subset(Baseline_all,Year<2000)
Baseline_all$Year<-NULL

ALL_FUTURE<-Future_all  
Future_all$yr = Future_all$Date$year + 1900
Future_all = subset(Future_all, yr >= Year - (Range/2) & yr <= (Year + (Range/2)))

####Set Average values for all four weather variables, using all baseline years and all climate models
BaseMeanPr = mean(Baseline_all$PrecipCustom)
BaseMeanTmx = mean(Baseline_all$TmaxCustom)
BaseMeanTmn = mean(Baseline_all$TminCustom)

####Create Future/Baseline means data tables, with averages for all four weather variables, organized by GCM
Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                    ~ Future_all$GCM, Future_all, mean,na.rm=F))   # , Future_all$Wind
names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")    # , "Wind"
Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2

Baseline_Means = data.frame(aggregate(cbind(PrecipCustom, TmaxCustom, TminCustom)~GCM, 
                                      Baseline_all[which(Baseline_all$GCM %in% unique(Future_all$GCM)),], mean))    #  ,Baseline_all$Wind)
names(Baseline_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")  #  , "Wind")
Baseline_Means$TavgCustom = (Baseline_Means$TmaxCustom + Baseline_Means$TminCustom)/2

#### add delta columns in order to classify CFs
Future_Means$DeltaPr = (Future_Means$PrecipCustom - Baseline_Means$PrecipCustom)*365
Future_Means$DeltaTmx = Future_Means$TmaxCustom - Baseline_Means$TmaxCustom
Future_Means$DeltaTmn = Future_Means$TminCustom - Baseline_Means$TminCustom
Future_Means$DeltaTavg = Future_Means$TavgCustom - Baseline_Means$TavgCustom


######## END INITIALS ########

##### Scatterplots for presentation ######
## Scatterplots ##
head(Future_Means)
Longx<- "annual average temperature (F)"
Longy<- "annual average precipitation (in)"
x <- "DeltaTavg"
y <- "DeltaPr"

# No color
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr))
dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in 2055 by GCM run",sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  # geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) #change

ggsave(paste(SiteID,"-Scatter-",x,"--",y,".png",sep=""), width = 15, height = 9)

####### Scatterplot with CF color
FM<-Future_Means
# '%ni%' <- Negate('%in%')
FM$CFnew<-as.character(FM$CF)
# FM$CFnew[which(FM$CFnew %ni% FutureSubset)]<-"Not Selected"
FM$CFnew[which(FM$CFnew=="Central")]<-"Not Selected"
FM$CFnew<-factor(FM$CFnew,levels=c("Warm Wet","Hot Wet","Not Selected","Warm Dry","Hot Dry"))
levels(FM$CFnew)

ggplot(FM, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365)) +
  geom_text(aes(label=GCM,color=CFnew),position=position_jitter(0,.2)) + 
  geom_point(size=5,colour="black")+
  geom_point(aes(color=CFnew),size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=20), legend.title=element_text(size=20)) + 
  ###
  labs(title ="Changes in climate means centered on 2065 (2050-2080)\n relative to historical period (1950-2000) by GCM run", 
       x = paste("Change in ",Longx,sep=""), # Change
       y = paste("Change in ",Longy,sep="")) + #change
  scale_color_manual(name="Climate Futures", values=colors5) +
  scale_fill_manual(name="Climate Futures",values = colors5) + 
  guides(color=guide_legend(title="Climate Futures\n",override.aes = list(size=7))) +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(FM$DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(FM$DeltaTavg)),linetype=2)  #change

ggsave(paste(SiteID, "Scatter BY SCENARIO-",x,"--",y,".png",sep=""), width = 15, height = 9)


##############################3
#################3
df<-read.csv("C:/Users/achildress/Documents/GGCL_ALL.csv")
df$GCM <- paste(df$Model,df$RCP,sep=".")
Tavg25 = quantile(df$Tmean_F,0.25)
Tavg75 = quantile(df$Tmean_F,0.75)
Pr25 = quantile(df$Precip_in,0.25)
Pr75 = quantile(df$Precip_in,0.75)

head(Future_Means)
Longx<- "annual average temperature (F)"
Longy<- "annual average precipitation (in)"
x <- "DeltaTavg"
y <- "DeltaPr"

# No color
dualscatter = ggplot(df, aes(Tmean_F, Precip_in, xmin=Tavg25, xmax=Tavg75, ymin=Pr25, ymax=Pr75))
dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID," Changes in climate means in 2055 by GCM run",sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) +
  geom_hline(aes(yintercept=mean(Precip_in)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(Tmean_F)),linetype=2) #change

#~~~~~~~~~~~~~~
# Presetation only scatterplots
#~~~~~~~~~~~~~~
# Points only w/out
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
dualscatter  + geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID, "Changes in climate means in 2080 by GCM run\n", Longx," vs. ",Longy,sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  theme(legend.position="none") +
  xlim(0, max(Future_Means$DeltaTavg))
ggsave(paste(SiteID,"-Scatter-POINTS ONLY",x,"--",y,".png",sep=""), width = 15, height = 9)  

# Points only w/ box
dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365, xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
dualscatter  + geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title =paste(SiteID, "Changes in climate means in 2080 by GCM run\n", Longx," vs. ",Longy,sep=""), 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenariow",values = c("black")) +
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2)  #change
ggsave(paste(SiteID,"-Scatter-POINTS&BOX",x,"--",y,".png",sep=""), width = 15, height = 9)
