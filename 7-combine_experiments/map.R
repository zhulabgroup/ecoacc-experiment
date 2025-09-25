# TITLE:          Map for experiment locations
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Locations of all experiments
# DATA OUTPUT:    Map
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)
library(maps)
library(mapdata)
library(viridis)
library(raster)
library(ggmap)
library(sf)
library(ggrepel)


# Trying to get temp data mapped here; other map is below
# Set path to chelsa data
path_data_chelsa = "/Volumes/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")

# Set path to chelsa data
path_data = "/Volumes/seas-zhukai/datasets/boundary/NaturalEarth/ne_10m_land/"
setwd(path_data)
# Read in data
bound <- read_sf("ne_10m_land.shp")

data_masked <- mask(chelsa_bio1_data,bound) %>% # mask the unwanted areas
  crop(bound) # crop to shape

plot(data_masked) # the small picture





# Load data
us_map <- map_data("state")

# List of locations for each experiment
locations <- tibble(
  Experiment = c("TeRaCON", "B4WarmED CFC", "B4WarmED HWRC", "Oklahoma", "PHACE", "JRGCE"),
  lat = c(45, 46.7, 47.9, 35, 41.2, 37.4),
  long = c(-93, -92.5, -91.8, -97.5, -104.9, -122.2)
)
locations_dis <- tibble(
  Experiment = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE",
                 "KNZ","HYS","SGS","CHY"),
  Treatment = c("Warmed","Warmed","Warmed","Warmed","Warmed","Warmed",
                "Drought","Drought","Drought","Drought"),
  lat = c(45, 46.7, 47.9, 35, 41.2, 37.4, 39.1, 38.9, 40.8, 41.2),
  long = c(-93, -92.5, -91.8, -97.5, -104.9, -122.2,-96.6, -99.3, -104.8, -104.9)
)

png("map.png", units="in", width=8, height=5, res=300)
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = locations, aes(x = long, y = lat), size = 3,color="red",alpha=0.8) +
  coord_fixed(1.3) +
  geom_label_repel(data = locations,
                   aes(x = long, y = lat, label = Experiment),
                   min.segment.length = 0,
                   size = 3,
                   box.padding = 0.5,          # Increase the padding around the box
                   point.padding = 0.3,   
                   fill = "white",             # Background color of the label
                   color = "black",
                   nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
                   nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
                   segment.color = 'grey50') +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))
dev.off()

plot2<-ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = locations_dis, aes(x = long, y = lat, color = Treatment), size = 4) +
  coord_fixed(1.3) +
  scale_color_manual(values = c("orange","red")) +
  geom_label_repel(data = locations_dis,
                   aes(x = long, y = lat, label = Experiment),
                   size = 3,
                   box.padding = 0.5,          # Increase the padding around the box
                   point.padding = 0.3,   
                   fill = "white",             # Background color of the label
                   color = "black",
                   nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
                   nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
                   segment.color = 'grey50') +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))



# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(plot, paste(path_out,'map.rds'))
saveRDS(plot2, paste(path_out,'map_disequilibrium.rds'))
  

