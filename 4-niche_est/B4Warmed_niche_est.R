# TITLE:          B4Warmed niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Dec 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_b4warmed.csv")

# Calculating the median temp and precip for each species
chelsa_data$species[chelsa_data$species == "Populus tremuloides"] <- "Populus tremouloides" # fixing spp name to match b4warmed

# Calculating the median temp and precip for each species
niche_est_cfc <- chelsa_data %>%
  filter(site =="CFC") %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,site,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))
niche_est_hwrc <- chelsa_data %>%
  filter(site =="HWRC") %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,site,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(niche_est_cfc,paste(path_out,'b4warmed_cfc_niche.csv'),row.names=F)
write.csv(niche_est_hwrc,paste(path_out,'b4warmed_hwrc_niche.csv'),row.names=F)
