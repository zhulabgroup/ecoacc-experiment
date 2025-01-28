# TITLE:          JRGCE annual CHELSA data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     raw GBIF occurrence data
# DATA OUTPUT:    temp and precip data for each GBIF occurrence point
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(raster)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)
# Load in data
jrgce <- read.csv(" GBIF_thinned_jrgce.csv")
# Set path to chelsa data
path_data_chelsa = "/Volumes/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
chelsa_bio12_data <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")

# Extracting lat/long from GBIF data
gbif_coords <- jrgce %>%
  dplyr::select(Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude) %>%
  relocate(longitude)
coordinates(gbif_coords)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(chelsa_bio1_data)

# Assign the CRS to the SpatialPoints
proj4string(gbif_coords) <- crs_raster

# Extracting mean annual temp and precip data for each coordinate
chelsa_bio1 <- extract(chelsa_bio1_data, gbif_coords, df = T) 
chelsa_bio12 <- extract(chelsa_bio12_data, gbif_coords, df = T)

# Scaling data correctly
chelsa_bio1_trans <- chelsa_bio1 %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_trans <- chelsa_bio12 %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)

# Merging occurrence data with chelsa data
gbif_spp_occ <- jrgce %>%
  dplyr::select(species,Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude)
chelsa_gbif <- cbind(gbif_spp_occ, chelsa_bio1_trans, chelsa_bio12_trans)

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(chelsa_gbif,paste(path_out,'CHELSA_jrgce.csv'))
