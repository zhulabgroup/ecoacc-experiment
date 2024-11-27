# TITLE:          TeRaCON niche calculation for different gbif sales
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)

# Load in data
chelsa_uscan <- read.csv(" CHELSA_teracon_uscan.csv")
chelsa_1000 <- read.csv(" CHELSA_teracon_1000.csv")
chelsa_500 <- read.csv(" CHELSA_teracon_500.csv")

# Calculating the median temp and precip for each species
chelsa_uscan$species[chelsa_uscan$species == "Elymus repens"] <- "Agropyron repens" # fixing spp name to match teracon
chelsa_uscan <- chelsa_uscan %>%
  mutate(species=replace_na(species, "Petalostemum villosum"))

chelsa_1000$species[chelsa_1000$species == "Elymus repens"] <- "Agropyron repens" # fixing spp name to match teracon
chelsa_1000$species[chelsa_1000$species == "Dalea villosa"] <- "Petalostemum villosum"

chelsa_500$species[chelsa_500$species == "Elymus repens"] <- "Agropyron repens" # fixing spp name to match teracon
chelsa_500$species[chelsa_500$species == "Dalea villosa"] <- "Petalostemum villosum"

niche_est_uscan <- chelsa_uscan %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))
niche_est_1000 <- chelsa_1000 %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))
niche_est_500 <- chelsa_500 %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(niche_est_uscan,paste(path_out,'teracon_niche_uscan.csv'),row.names=F)
write.csv(niche_est_1000,paste(path_out,'teracon_niche_1000.csv'),row.names=F)
write.csv(niche_est_500,paste(path_out,'teracon_niche_500.csv'),row.names=F)
