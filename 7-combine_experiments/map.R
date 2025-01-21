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

# Load dviridisLite# Load data
us_map <- map_data("state")

# List of locations for each experiment
locations <- tibble(
  Experiment = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE"),
  lat = c(45, 46.7, 47.9, 35, 41.2, 37.4),
  long = c(-93, -92.5, -91.8, -97.5, -104.9, -122.2)
)

png("map.png", units="in", width=10, height=6, res=300)
ggplot() +
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
  

