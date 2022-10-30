library(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ncdf4)
library(reshape2)


rm(list=ls())

######### INITIALS #########
plotDir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/TARs/GRCA_Climate_Report/GRCA_report_proper/Revised_Figs_ACR/" # AKD PlotDir
data.dir <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/TARs/GRCA_Climate_Report/GRCA Repo Data/"

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


SiteID = "GGCL"
CFs <- c("MRI-CGCM3.rcp85","MIROC-ESM-CHEM.rcp85")
# cols = c("#9A9EE5","#E10720")
col.RCP2 = c("#3030FF","#FFBF00") #medium blue & light orange
colors2<- c("#9A9EE5","#E10720") 

#Directory and RData file where daily data series is stored
DataDir = "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/GRCA_CCSP/"

df<-read.csv(paste0(DataDir,"GGCL_scatterplot_TmeanF_DJLPrcpIn.csv"))
df$GCM <- paste(df$Model,df$RCP,sep=".")
Tavg25 = quantile(df$Tmean_F,0.25)
Tavg75 = quantile(df$Tmean_F,0.75)
Pr25 = quantile(df$Precip_in,0.25)
Pr75 = quantile(df$Precip_in,0.75)

df$selected <- NA

df$selected[which(df$GCM == CFs[1])] <- CFs[1]
df$selected[which(df$GCM == CFs[2])] <- CFs[2]
df$selected = factor(df$selected,levels = CFs)
df$RCP = factor(df$RCP,levels = unique(df$RCP))

head(df)
Longx<- "annual average temperature (F)"
Longy<- "winter (DJF) precipitation (in)"
x <- "TmeanF"
y <- "PrcpIn_DJF"

# Colors
dualscatter = ggplot(df, aes(TmeanF, PrcpIn_DJF))
dualscatter  +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[1])]), y=mean(PrcpIn_DJF[which(GCM==CFs[1])])), shape=21, size=9, stroke=2, colour=colors2[1],fill=colors2[1]) +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[2])]), y=mean(PrcpIn_DJF[which(GCM==CFs[2])])), shape=21, size=9, stroke=2, colour=colors2[2],fill=colors2[2]) +
  geom_point(colour="black",size=4) +
  geom_point(aes(colour=RCP),size=3) +
  geom_text_repel(aes(label=GCM)) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title ="GGCL changes in climate means in 2055 by model run", 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Emissions", values=col.RCP2) 
  # scale_shape_manual(name="Emissions",values=c(17,19)) +
  # scale_fill_manual(name="Emissions",values = col.RCP2) +

ggsave(paste0(DataDir,"GGCL-Scatter-rcpCol-modelSelect.png"), width = 15, height = 9)

# circled models
dualscatter = ggplot(df, aes(TmeanF, PrcpIn_DJF))
dualscatter  + geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[1])]), y=mean(PrcpIn_DJF[which(GCM==CFs[1])])), shape=21, size=22, stroke=3, colour=colors2[1]) +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[2])]), y=mean(PrcpIn_DJF[which(GCM==CFs[2])])), shape=21, size=22, stroke=3, colour=colors2[2]) +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[1])]), y=mean(PrcpIn_DJF[which(GCM==CFs[1])])), shape=20, size=2, colour=colors2[1]) +
  geom_point(aes(x=mean(TmeanF[which(GCM==CFs[2])]), y=mean(PrcpIn_DJF[which(GCM==CFs[2])])), shape=20, size=2,  colour=colors2[2]) +
  theme(axis.text=element_text(size=18),
        axis.title.x=element_text(size=18,vjust=-0.2),
        axis.title.y=element_text(size=18,vjust=0.2),
        plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
  ###
  labs(title ="Changes in climate means in 2055 by model run", 
       x = paste("Changes in ",Longx,sep=""), # Change
       y = paste("Changes in ",Longy,sep="")) + #change
  scale_color_manual(name="Emissions", values=col.RCP2) 
# scale_shape_manual(name="Emissions",values=c(17,19)) +
# scale_fill_manual(name="Emissions",values = col.RCP2) +

ggsave(paste0(plotDir,"GGCL-Scatter-modelSelect.png"), width = 15, height = 9)



