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
  Experiment = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE"),
  lat = c(45, 46.7, 47.9, 35, 41.2, 37.4),
  long = c(-93, -92.5, -91.8, -97.5, -104.9, -122.2)
)

png("map.png", units="in", width=10, height=6, res=300)
plot <- ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = locations, aes(x = long, y = lat, color = Experiment), size = 4) +
  coord_fixed(1.3) +
  scale_color_viridis(discrete=T) +
  theme_minimal() +
  theme(legend.text = element_text(size=14),
        legend.title = element_text(size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))
dev.off()



# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc/data_for_plots/"
saveRDS(plot, paste(path_out,'map.rds'))
  

