# GRCA Regressions
library(plyr)
library(dplyr)
library(broom)

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
  

# running regressions (using this framework: http://r4stats.com/2017/04/18/group-by-modeling-in-r-made-easy/)

by_sea_ele <- 
  group_by(grca_all_data, sea_ele)

do(by_sea_ele,
   glance(
     lm(TmaxF ~ Year, data = .)
   ))

do(by_sea_ele,
   tidy(
     lm(TmaxF ~ Year, data = .)
   ))


