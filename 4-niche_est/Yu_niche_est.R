# TITLE:          Yu niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           April 2025


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_knz.csv")

# Calculating the median temp and precip for each species
chelsa_data$species[chelsa_data$species == "Symphyotrichum oblongifolium"] <- "Aster oblongifolius" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Bouteloua dactyloides"] <- "Buchloe dactyloides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Carex inops"] <- "Carex heliophila" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Erigeron canadensis"] <- "Conyza canadensis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Eleocharis sp"] <- "Eleocharis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Juncus sp"] <- "Juncus" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Brickellia eupatorioides"] <- "Kuhnia eupatorioides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pediomelum tenuiflorum"] <- "Psoralea tenuiflora" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Mimosa quadrivalvis"] <- "Schrankia nuttallii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Packera plattensis"] <- "Senecio plattensis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Andropogon gerardi"] <- "Andropogon gerardii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Sporobolus compositus"] <- "Sporobolus asper" # fixing spp name to match phace

niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1,latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
write.csv(niche_est,paste(path_out,'knz_niche.csv'),row.names=F)
