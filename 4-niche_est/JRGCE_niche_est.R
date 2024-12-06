# TITLE:          JRGCE niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_jrgce.csv")


# Calculating the median temp and precip for each species
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(tmp)) %>%
  mutate(precip_niche = median(ppt)) %>%
  dplyr::select(-c(key, vpd))

# Function to plot all species occurrences w/ median niche estimate
plot_species_with_centroids <- function(data, species_name) {
  # Filter data for selected species
  species_data <- data %>%
    filter(species == species_name)
  
  ggplot(species_data, aes(x = tmp, y = ppt)) +
    geom_point() +
    geom_point(aes(x = temp_niche, y = precip_niche),  colour = "yellow", size = 4) + 
    theme_classic() +
    labs(title = species_name,
         x = "Mean Annual Temperature",
         y = "Mean Annual Precipitation")
}
plot_species_with_centroids(niche_est, "Danthonia californica")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(niche_est,paste(path_out,'jrgce_niche.csv'),row.names=F)
