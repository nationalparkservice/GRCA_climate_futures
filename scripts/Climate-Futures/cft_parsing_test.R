# Install 'cft' package from github

library(devtools)
install_github("earthlab/cft", force = TRUE)

# attach 'cft' package and check for available functions
library(cft)
ls(pos="package:cft")

start_time <- Sys.time()
inputs <- cft::available_data()

end_time <- Sys.time()

end_time - start_time

# variables
levels(as.factor(inputs$variable_names$Variable))

# available variable abbreviations
levels(as.factor(inputs$variable_names$`Variable abbreviation`))

# available scenarios
levels(as.factor(inputs$variable_names$Scenario))

# available models 
levels(as.factor(inputs$variable_names$Model))

# model abbreviations 
levels(as.factor(inputs$variable_names$`Model abbreviation`))

# filter models, scenarios and variables (all available)

# input_variables <- inputs$variable_names %>% 
#   filter(Variable %in% c("Maximum Relative Humidity", 
#                          "Maximum Temperature", 
#                          "Minimum Relative Humidity",          
#                          "Minimum Temperature",                 
#                          "Precipitation")) %>% 
#   filter(Scenario %in% c( "RCP 4.5", "RCP 8.5")) %>% 
#   filter(Model %in% c(
#     "Beijing Climate Center - Climate System Model 1.1",
#     "Beijing Normal University - Earth System Model",
#     "Canadian Earth System Model 2",                                                                
#     "Centre National de Recherches Météorologiques - Climate Model 5",                              
#     "Commonwealth Scientific and Industrial Research Organisation - Mk3.6.0",                       
#     "Community Climate System Model 4",                                                             
#     "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Generalized Ocean Layer Dynamics",
#     "Geophysical Fluid Dynamics Laboratory - Earth System Model 2 Modular Ocean",                   
#     "Hadley Global Environment Model 2 - Climate Chemistry 365 (day) ",                             
#     "Hadley Global Environment Model 2 - Earth System 365 (day)",                                   
#     "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Low Resolution",                     
#     "Institut Pierre Simon Laplace (IPSL) - Climate Model 5A - Medium Resolution",                  
#     "Institut Pierre Simon Laplace (IPSL) - Climate Model 5B - Low Resolution",                     
#     "Institute of Numerical Mathematics Climate Model 4",                                           
#     "Meteorological Research Institute - Coupled Global Climate Model 3",                           
#     "Model for Interdisciplinary Research On Climate - Earth System Model",                         
#     "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry",             
#     "Model for Interdisciplinary Research On Climate 5",                                            
#     "Norwegian Earth System Model 1 - Medium Resolution"  )) %>%
#   
#   pull("Available variable")
# 
# input_variables



# filter models, scenarios and variables (define here)

input_variables <- inputs$variable_names %>% 
  filter(Variable %in% c("Maximum Relative Humidity", 
                         "Maximum Temperature", 
                         "Minimum Relative Humidity",          
                         "Minimum Temperature",                 
                         "Precipitation")) %>% 
  filter(Scenario %in% c("RCP 8.5")) %>% 
  filter(Model %in% c(
    "Meteorological Research Institute - Coupled Global Climate Model 3",                           
    "Model for Interdisciplinary Research On Climate - Earth System Model - Chemistry")) %>%
  pull("Available variable")

input_variables

# Establish Area of Interest by Bounding Box

GGCL <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GRCA\\GRCA_maps\\GIS\\GGCL\\GGCL.shp") # greater grand canyon landscape
GGCL <- st_transform(GGCL, 4326) # CONUS Albers
Sp_ggcl <- as_Spatial(GGCL)

my_boundary <- data.frame(Sp_ggcl@bbox) # get bounding box

plot(Sp_ggcl)

# download data

start_time <- Sys.time()
center_point <- st_centroid(GGCL) %>% st_bbox(center_point)

Pulled_data_single_space_all_timepoints <- inputs$src %>% 
  hyper_filter(lat = lat <= c(center_point[4]+0.05) & lat >= c(center_point[2]-0.05)) %>% 
  hyper_filter(lon = lon <= c(center_point[3]+0.05) & lon >= c(center_point[1]-0.05)) %>%
  # hyper_filter(time = input_times$`Available times` ==  73048) %>% 
  hyper_tibble(select_var = input_variables) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")

end_time <- Sys.time()
print(end_time - start_time)

head(Pulled_data_single_space_all_timepoints)






