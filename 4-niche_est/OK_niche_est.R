# TITLE:          Oklahoma niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_ok.csv")

# Calculating the median temp and precip for each species
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/OK/"
write.csv(niche_est,paste(path_out,'ok_niche.csv'),row.names=F)
