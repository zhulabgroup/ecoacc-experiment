# TITLE:          TeRaCON niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Oct 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_6month_teracon_limited.csv") # "CHELSA_teracon_limited" in turbo only contains data for ecoregion 8, and 6month contains climate info for march-aug


# Calculating the median temp and precip for each species
chelsa_data$species[chelsa_data$species == "Elymus repens"] <- "Agropyron repens" # fixing spp name to match teracon
niche_est <- chelsa_data %>%
  mutate(species=replace_na(species, "Petalostemum villosum")) %>% # fixing spp name to match teracon
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))
# For the 6 month data
niche_est <- chelsa_data %>%
  mutate(species=replace_na(species, "Petalostemum villosum")) %>% # fixing spp name to match teracon
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(mean_annual_temp,mean_annual_precip,latitude,longitude)) %>%
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
#write.csv(niche_est,paste(path_out,'niche_estimate_teracon.csv'),row.names=F)
write.csv(niche_est,paste(path_out,'niche_estimate_6month_teracon_limited.csv'),row.names=F)
#write.csv(niche_est,paste(path_out,'niche_estimate_teracon_limited.csv'),row.names=F)
