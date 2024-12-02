# TITLE:          PHACE niche calculation for different gbif sales
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Dec 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
setwd(path_data)

# Load in data
chelsa_uscan <- read.csv(" CHELSA_phace_uscan.csv")
chelsa_1000 <- read.csv(" CHELSA_phace_1000.csv")
chelsa_500 <- read.csv(" CHELSA_phace_500.csv")

# Calculating the median temp and precip for each species
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
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
write.csv(niche_est_uscan,paste(path_out,'phace_niche_uscan.csv'),row.names=F)
write.csv(niche_est_1000,paste(path_out,'phace_niche_1000.csv'),row.names=F)
write.csv(niche_est_500,paste(path_out,'phace_niche_500.csv'),row.names=F)
