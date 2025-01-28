# TITLE:          B4Warmed annual CHELSA data for GBIF
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw GBIF occurrence data
# DATA OUTPUT:    temp and precip data for each GBIF occurrence point
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(raster)

# Set path to turbo to get data
path_data_gbif = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data_gbif)
# Read in data
gbif_data_cfc <- read.csv(" GBIF_thinned_b4warmed_cfc.csv")
gbif_data_hwrc <- read.csv(" GBIF_thinned_b4warmed_hwrc.csv")
# Set path to chelsa data
path_data_chelsa = "/Volumes/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
chelsa_bio12_data <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")

# Extracting lat/long from GBIF data
gbif_coords_cfc <- gbif_data_cfc %>%
  dplyr::select(Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude) %>%
  relocate(longitude)
coordinates(gbif_coords_cfc)<-c("longitude", "latitude")

gbif_coords_hwrc <- gbif_data_hwrc %>%
  dplyr::select(Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude) %>%
  relocate(longitude)
coordinates(gbif_coords_hwrc)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(chelsa_bio1_data)

# Assign the CRS to the SpatialPoints
proj4string(gbif_coords_cfc) <- crs_raster
proj4string(gbif_coords_hwrc) <- crs_raster

# Extracting mean annual temp and precip data for each coordinate
chelsa_bio1_ex_cfc <- extract(chelsa_bio1_data, gbif_coords_cfc, df = T) 
chelsa_bio12_ex_cfc <- extract(chelsa_bio12_data, gbif_coords_cfc, df = T) 

chelsa_bio1_ex_hwrc <- extract(chelsa_bio1_data, gbif_coords_hwrc, df = T) 
chelsa_bio12_ex_hwrc <- extract(chelsa_bio12_data, gbif_coords_hwrc, df = T) 

# Scaling data correctly
chelsa_bio1_ex_trans_cfc <- chelsa_bio1_ex_cfc %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_ex_trans_cfc <- chelsa_bio12_ex_cfc %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)

chelsa_bio1_ex_trans_hwrc <- chelsa_bio1_ex_hwrc %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_ex_trans_hwrc <- chelsa_bio12_ex_hwrc %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)

# Merging occurrence data with chelsa data
gbif_spp_occ_cfc <- gbif_data_cfc %>%
  dplyr::select(species,Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude)
chelsa_gbif_cfc <- cbind(gbif_spp_occ_cfc, chelsa_bio1_ex_trans_cfc, chelsa_bio12_ex_trans_cfc)
chelsa_gbif_cfc$site <- "CFC"

gbif_spp_occ_hwrc <- gbif_data_hwrc %>%
  dplyr::select(species,Latitude,Longitude) %>%
  rename(latitude = Latitude) %>%
  rename(longitude = Longitude)
chelsa_gbif_hwrc <- cbind(gbif_spp_occ_hwrc, chelsa_bio1_ex_trans_hwrc, chelsa_bio12_ex_trans_hwrc)
chelsa_gbif_hwrc$site <- "HWRC"

# Merge CFC and HWRC sites
chelsa_gbif <- rbind(chelsa_gbif_cfc,chelsa_gbif_hwrc)

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(chelsa_gbif,paste(path_out,'CHELSA_b4warmed.csv'))
