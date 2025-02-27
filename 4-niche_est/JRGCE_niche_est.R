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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_jrgce.csv")



# Calculating the median temp and precip for each species
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(X,ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1,latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
write.csv(niche_est,paste(path_out,'jrgce_niche.csv'),row.names=F)
