# GRCA Regressions
library(plyr)
library(dplyr)
library(broom)
library(tidyr)
library(stringr)
library(ggplot2)


#read in data
tmax <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmax_hm.csv")
tmin <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/tmin_hm.csv")
prcp <- read.csv("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/prcp_hm.csv")

#create bins

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

# assign months and seasons 

tmax$month <- as.numeric(tmax$month)
tmax$year <- as.numeric(tmax$year)

tmax <- tmax %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))

tmin$month <- as.numeric(tmin$month)
tmin$year <- as.numeric(tmin$year)

tmin <- tmin %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))

prcp$month <- as.numeric(prcp$month)
prcp$year <- as.numeric(prcp$year)

prcp <- prcp %>% 
  mutate(
    season = case_when(
      month %in%  9:11 ~ "Fall",
      month %in%  c(12, 1, 2)  ~ "Winter",
      month %in%  3:5  ~ "Spring",
      TRUE ~ "Summer"))

# merge data frames
merge1 <- cbind(DataSet1 = tmax, DataSet2 = tmin) %>% 
 select(DataSet1.DEM_values_tmax, DataSet1.CellID,
        DataSet1.year,DataSet1.month, DataSet1.tempF,
        DataSet1.Elevation, DataSet1.season, DataSet2.tempF)

merge2 <- cbind(merge1, prcp) %>% 
  select(DataSet1.DEM_values_tmax, DataSet1.CellID,
         DataSet1.year,DataSet1.month, DataSet1.tempF,
         DataSet1.Elevation, DataSet1.season, DataSet2.tempF, inches)
  
grca_all_data <- merge2 %>% 
  rename(DEM =DataSet1.DEM_values_tmax, Cell_ID = DataSet1.CellID,
         Year = DataSet1.year, Month = DataSet1.month, TmaxF = DataSet1.tempF,
         Elevation = DataSet1.Elevation, Season = DataSet1.season, TminF = DataSet2.tempF, prcp_in = inches)

grca_all_data$sea_ele <- paste0(grca_all_data$Season, "_", grca_all_data$Elevation)
  
grca_all_data <- grca_all_data %>% 
  mutate(TaveF = ((TmaxF + TminF) / 2))

# running regressions (using this framework: http://r4stats.com/2017/04/18/group-by-modeling-in-r-made-easy/)


#
#output_tidy <- do(by_sea_ele,
#   tidy(
#     lm(TmaxF ~ Year, data = .)
#   ))
#
#output_glance <- do(by_sea_ele,
#                    glance(
#                      lm(TmaxF ~ Year, data = .)
#                    ))

by_sea_ele <- 
  group_by(grca_all_data, sea_ele)

by_annual <- 
  group_by(grca_all_data, Elevation)

# tmax

Tmax_ch_sea <- do(by_sea_ele,
        tidy(
          lm(TmaxF ~ Year, data = .)
        ))

Tmax_ch_sea <- Tmax_ch_sea %>%
  spread(term, estimate) %>%
  select(sea_ele, Year) %>% drop_na()

Tmax_ch_sea$change_dec <- (Tmax_ch_sea$Year * 10)

seasons <- word(Tmax_ch_sea$sea_ele, 1, sep = "_")
elevations <- word(Tmax_ch_sea$sea_ele, 2, sep = "_")

Tmax_ch_sea$season <- seasons
Tmax_ch_sea$elevation <- elevations
Tmax_ch_sea

Tmax_ch_ann <- do(by_annual,
                  tidy(
                    lm(TmaxF ~ Year, data = .)
                  ))

Tmax_ch_ann <- Tmax_ch_ann %>%
  spread(term, estimate) %>%
  select(Elevation, Year) %>% drop_na()

Tmax_ch_ann$change_dec <- (Tmax_ch_ann$Year * 10)

Tmax_ch_ann <- Tmax_ch_ann %>% 
  mutate(season = "Annual") %>% 
  rename(elevation = Elevation) %>% 
  select(-Year)

sea_ele_ann_tmax <- paste0(Tmax_ch_ann$season, "_", Tmax_ch_ann$elevation)
Tmax_ch_ann$sea_ele <- sea_ele_ann_tmax

Tmax_ch_all <- rbind(Tmax_ch_ann, Tmax_ch_sea)
Tmax_ch_all <- Tmax_ch_all %>% 
  select(-Year)

Tmax_ch_all$elevation <- factor(Tmax_ch_all$elevation, levels=c("338-1160","1161-1980","1981-2739"))

#convert to feet
Tmax_ch_all$elevation <- revalue(Tmax_ch_all$elevation, c("338-1160"="1109-3807", "1161-1980"="3808-6497", "1981-2739"="6498-8987"))


png("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/plots/new/tmax_change_heatmap.png",
    width = 900, height = 600)

ggplot(Tmax_ch_all, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=elevation, fill=change_dec)) +
  geom_tile(color="white", size=0.2) +
  geom_text(aes(label=round(change_dec,2))) +
  guides(fill=guide_legend(title = "Decadal Change in Tmax (\u00B0F)")) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Decadal Change in Tmax (\u00B0F) by Season (1895-2020)",
       x = "Season",
       y = "Elevation (ft)") +
  theme(plot.title = element_text(hjust=0.5))

dev.off()


# tave

Tave_ch_sea <- do(by_sea_ele,
                  tidy(
                    lm(TaveF ~ Year, data = .)
                  ))

Tave_ch_sea <- Tave_ch_sea %>%
  spread(term, estimate) %>%
  select(sea_ele, Year) %>% drop_na()

Tave_ch_sea$change_dec <- (Tave_ch_sea$Year * 10)

seasons <- word(Tave_ch_sea$sea_ele, 1, sep = "_")
elevations <- word(Tave_ch_sea$sea_ele, 2, sep = "_")

Tave_ch_sea$season <- seasons
Tave_ch_sea$elevation <- elevations
Tave_ch_sea

Tave_ch_ann <- do(by_annual,
                  tidy(
                    lm(TaveF ~ Year, data = .)
                  ))

Tave_ch_ann <- Tave_ch_ann %>%
  spread(term, estimate) %>%
  select(Elevation, Year) %>% drop_na()

Tave_ch_ann$change_dec <- (Tave_ch_ann$Year * 10)

Tave_ch_ann <- Tave_ch_ann %>% 
  mutate(season = "Annual") %>% 
  rename(elevation = Elevation) %>% 
  select(-Year)

sea_ele_ann_tave <- paste0(Tave_ch_ann$season, "_", Tave_ch_ann$elevation)
Tave_ch_ann$sea_ele <- sea_ele_ann_tave

Tave_ch_all <- rbind(Tave_ch_ann, Tave_ch_sea)
Tave_ch_all <- Tave_ch_all %>% 
  select(-Year)

Tave_ch_all$elevation <- factor(Tave_ch_all$elevation, levels=c("338-1160","1161-1980","1981-2739"))

#convert to feet
Tave_ch_all$elevation <- revalue(Tave_ch_all$elevation, c("338-1160"="1109-3807", "1161-1980"="3808-6497", "1981-2739"="6498-8987"))


png("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/plots/new/tave_change_heatmap.png",
    width = 900, height = 600)

ggplot(Tave_ch_all, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=elevation, fill=change_dec)) +
  geom_tile(color="white", size=0.2) +
  geom_text(aes(label=round(change_dec,2))) +
  guides(fill=guide_legend(title = "Decadal Change in Tave (\u00B0F)")) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Decadal Change in Tave (\u00B0F) by Season (1895-2020)",
       x = "Season",
       y = "Elevation (ft)") +
  theme(plot.title = element_text(hjust=0.5))

dev.off()

# tmin

Tmin_ch_sea <- do(by_sea_ele,
                  tidy(
                    lm(TminF ~ Year, data = .)
                  ))

Tmin_ch_sea <- Tmin_ch_sea %>%
  spread(term, estimate) %>%
  select(sea_ele, Year) %>% drop_na()

Tmin_ch_sea$change_dec <- (Tmin_ch_sea$Year * 10)

seasons <- word(Tmin_ch_sea$sea_ele, 1, sep = "_")
elevations <- word(Tmin_ch_sea$sea_ele, 2, sep = "_")

Tmin_ch_sea$season <- seasons
Tmin_ch_sea$elevation <- elevations
Tmin_ch_sea

Tmin_ch_ann <- do(by_annual,
                  tidy(
                    lm(TminF ~ Year, data = .)
                  ))

Tmin_ch_ann <- Tmin_ch_ann %>%
  spread(term, estimate) %>%
  select(Elevation, Year) %>% drop_na()

Tmin_ch_ann$change_dec <- (Tmin_ch_ann$Year * 10)

Tmin_ch_ann <- Tmin_ch_ann %>% 
  mutate(season = "Annual") %>% 
  rename(elevation = Elevation) %>% 
  select(-Year)

sea_ele_ann_tmin <- paste0(Tmin_ch_ann$season, "_", Tmin_ch_ann$elevation)
Tmin_ch_ann$sea_ele <- sea_ele_ann_tmin

Tmin_ch_all <- rbind(Tmin_ch_ann, Tmin_ch_sea)
Tmin_ch_all <- Tmin_ch_all %>% 
  select(-Year)

Tmin_ch_all$elevation <- factor(Tmin_ch_all$elevation, levels=c("338-1160","1161-1980","1981-2739"))

#convert to feet
Tmin_ch_all$elevation <- revalue(Tmin_ch_all$elevation, c("338-1160"="1109-3807", "1161-1980"="3808-6497", "1981-2739"="6498-8987"))


png("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/plots/new/tmin_change_heatmap.png",
    width = 900, height = 600)

ggplot(Tmin_ch_all, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=elevation, fill=change_dec)) +
  geom_tile(color="white", size=0.2) +
  geom_text(aes(label=round(change_dec,2))) +
  guides(fill=guide_legend(title = "Decadal Change in Tmin (\u00B0F)")) +
  scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Decadal Change in Tmin (\u00B0F) by Season (1895-2020)",
       x = "Season",
       y = "Elevation (ft)") +
  theme(plot.title = element_text(hjust=0.5))

dev.off()

# prcp

prcp_ch_sea <- do(by_sea_ele,
                  tidy(
                    lm(prcp_in ~ Year, data = .)
                  ))

prcp_ch_sea <- prcp_ch_sea %>%
  spread(term, estimate) %>%
  select(sea_ele, Year) %>% drop_na()

prcp_ch_sea$change_dec <- (prcp_ch_sea$Year * 10)

seasons <- word(prcp_ch_sea$sea_ele, 1, sep = "_")
elevations <- word(prcp_ch_sea$sea_ele, 2, sep = "_")

prcp_ch_sea$season <- seasons
prcp_ch_sea$elevation <- elevations
prcp_ch_sea

prcp_ch_ann <- do(by_annual,
                  tidy(
                    lm(prcp_in ~ Year, data = .)
                  ))

prcp_ch_ann <- prcp_ch_ann %>%
  spread(term, estimate) %>%
  select(Elevation, Year) %>% drop_na()

prcp_ch_ann$change_dec <- (prcp_ch_ann$Year * 10)

prcp_ch_ann <- prcp_ch_ann %>% 
  mutate(season = "Annual") %>% 
  rename(elevation = Elevation) %>% 
  select(-Year)

sea_ele_ann_prcp <- paste0(prcp_ch_ann$season, "_", prcp_ch_ann$elevation)
prcp_ch_ann$sea_ele <- sea_ele_ann_prcp

prcp_ch_all <- rbind(prcp_ch_ann, prcp_ch_sea)
prcp_ch_all <- prcp_ch_all %>% 
  select(-Year)

prcp_ch_all$elevation <- factor(prcp_ch_all$elevation, levels=c("338-1160","1161-1980","1981-2739"))

#convert to feet
prcp_ch_all$elevation <- revalue(prcp_ch_all$elevation, c("338-1160"="1109-3807", "1161-1980"="3808-6497", "1981-2739"="6498-8987"))


png("C:/Users/gknowlton/OneDrive - DOI/Documents/GRCA/heatmaps/plots/new/prcp_change_heatmap.png",
    width = 900, height = 600)

ggplot(prcp_ch_all, aes(x=factor(season, level = c('Annual', 'Spring', 'Summer', 'Fall', 'Winter')), y=elevation, fill=change_dec)) +
  geom_tile(color="white", size=0.2) +
  geom_text(aes(label=round(change_dec,4))) +
  guides(fill=guide_legend(title = "Decadal Change in Precipitation (in)")) +
  scale_fill_distiller(palette = "BrBG", trans = "reverse") +
  theme_bw(base_size=14) +
  labs(title = "Average Decadal Change in Precipitation (in) by Season (1895-2020)",
       x = "Season",
       y = "Elevation (ft)") +
  theme(plot.title = element_text(hjust=0.5))

dev.off()

