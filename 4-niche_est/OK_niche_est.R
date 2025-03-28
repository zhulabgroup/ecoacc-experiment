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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_ok.csv")

# Matching species names to those in the abundance data
chelsa_data$species[chelsa_data$species == "Trifolium purpureum "] <- "Trifolium purpureum"
chelsa_data$species[chelsa_data$species == "Muhlenbergia capillaris "] <- "Muhlenbergia capillaris"
chelsa_data$species[chelsa_data$species == "Symphyotrichum ericoides "] <- "Symphyotrichum ericoides"
chelsa_data$species[chelsa_data$species == "solidago drummondii"] <- "Solidago drummondii"
chelsa_data$species[chelsa_data$species == "Oxalis fontana"] <- "Oxalis stricta"
chelsa_data <- chelsa_data %>%
  mutate(species = ifelse(grepl("Viola", species), "Viola bicolor", species))

# Calculating the median temp and precip for each species
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1,latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
write.csv(niche_est,paste(path_out,'ok_niche.csv'),row.names=F)

