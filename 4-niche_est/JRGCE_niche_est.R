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
path_home = "/home/kcdobson"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_GBIF_jrgce.csv")


# Calculating the median temp and precip for each species
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(tmp)) %>%
  mutate(precip_niche = median(ppt)) %>%
  dplyr::select(-c(key, vpd, tmp, ppt)) %>%
  distinct()

# Function to plot all species occurrences w/ median niche estimate
plot_species_with_centroids <- function(data, species_name) {
  # Filter data for selected species
  species_data <- data %>%
    filter(species == species_name)
  
  ggplot(species_data, aes(x = mean_annual_temp, y = mean_annual_precip)) +
    geom_point() +
    geom_point(aes(x = temp_niche, y = precip_niche),  colour = "yellow", size = 4) + 
    theme_classic() +
    labs(title = species_name,
         x = "Mean Annual Temperature",
         y = "Mean Annual Precipitation")
}
plot_species_with_centroids(niche_est, "Asclepias tuberosa")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(niche_est,paste(path_out,'niche_estimate_teracon.csv'),row.names=F)