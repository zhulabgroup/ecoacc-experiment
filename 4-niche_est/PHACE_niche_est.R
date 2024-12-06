# TITLE:          PHACE niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_phace.csv")

# Calculating the median temp and precip for each species
chelsa_data$species[chelsa_data$species == "Elymus smithii"] <- "Pascopyrum smithii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Festuca octoflora"] <- "Vulpia octoflora"
chelsa_data$species[chelsa_data$species == "Pelecyphora vivipara"] <- "Escobaria vivipara"
chelsa_data$species[chelsa_data$species == "Oreocarya thyrsiflora"] <- "Cryptantha thyrsiflora"
chelsa_data$species[chelsa_data$species == "Oreocarya suffruticosa"] <- "Cryptantha cinerea"
chelsa_data$species[chelsa_data$species == "Tomostima reptans"] <- "Draba reptans"
chelsa_data$species[chelsa_data$species == "Xanthisma spinulosum"] <- "Machaeranthera pinnatifida"
chelsa_data$species[chelsa_data$species == "Physaria montana"] <- "Lesquerella montana"
chelsa_data$species[chelsa_data$species == "Dieteria canescens"] <- "Machaeranthera canescens"

niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
write.csv(niche_est,paste(path_out,'phace_niche.csv'),row.names=F)
