# TITLE:          PHACE annual CHELSA data for different GBIF scales
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
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/data_for_testing/"
setwd(path_data)
# Load in data
jrgce_uscan <- read.csv(" gbif_jrgce_uscan.csv")
jrgce_1000 <- read.csv(" gbif_jrgce_1000.csv")
jrgce_500 <- read.csv(" gbif_jrgce_500.csv")
# Set path to chelsa data
path_data_chelsa = "/nfs/turbo/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
chelsa_bio12_data <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")

# Extracting lat/long from GBIF data
gbif_coords_uscan <- jrgce_uscan %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords_uscan)<-c("longitude", "latitude")

gbif_coords_1000 <- jrgce_1000 %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords_1000)<-c("longitude", "latitude")

gbif_coords_500 <- jrgce_500 %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords_500)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(chelsa_bio1_data)

# Assign the CRS to the SpatialPoints
proj4string(gbif_coords_uscan) <- crs_raster
proj4string(gbif_coords_1000) <- crs_raster
proj4string(gbif_coords_500) <- crs_raster

# Extracting mean annual temp and precip data for each coordinate
chelsa_bio1_uscan <- extract(chelsa_bio1_data, gbif_coords_uscan, df = T) 
chelsa_bio12_uscan <- extract(chelsa_bio12_data, gbif_coords_uscan, df = T)

chelsa_bio1_1000 <- extract(chelsa_bio1_data, gbif_coords_1000, df = T) 
chelsa_bio12_1000 <- extract(chelsa_bio12_data, gbif_coords_1000, df = T) 

chelsa_bio1_500 <- extract(chelsa_bio1_data, gbif_coords_500, df = T) 
chelsa_bio12_500 <- extract(chelsa_bio12_data, gbif_coords_500, df = T) 

# Scaling data correctly
chelsa_bio1_uscan_trans <- chelsa_bio1_uscan %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_uscan_trans <- chelsa_bio12_uscan %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)

chelsa_bio1_1000_trans <- chelsa_bio1_1000 %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_1000_trans <- chelsa_bio12_1000 %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)

chelsa_bio1_500_trans <- chelsa_bio1_500 %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_500_trans <- chelsa_bio12_500 %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)


# Merging occurrence data with chelsa data
gbif_spp_occ_uscan <- jrgce_uscan %>%
  dplyr::select(species,decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude)
chelsa_gbif_uscan <- cbind(gbif_spp_occ_uscan, chelsa_bio1_uscan_trans, chelsa_bio12_uscan_trans)

gbif_spp_occ_1000 <- jrgce_1000 %>%
  dplyr::select(species,decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude)
chelsa_gbif_1000 <- cbind(gbif_spp_occ_1000, chelsa_bio1_1000_trans, chelsa_bio12_1000_trans)

gbif_spp_occ_500 <- jrgce_500 %>%
  dplyr::select(species,decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude)
chelsa_gbif_500 <- cbind(gbif_spp_occ_500, chelsa_bio1_500_trans, chelsa_bio12_500_trans)

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/data_for_testing/"
write.csv(chelsa_gbif_uscan,paste(path_out,'CHELSA_jrgce_uscan.csv'))
write.csv(chelsa_gbif_1000,paste(path_out,'CHELSA_jrgce_1000.csv'))
write.csv(chelsa_gbif_500,paste(path_out,'CHELSA_jrgce_500.csv'))
