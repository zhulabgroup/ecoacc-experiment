# TITLE:          TeRaCON annual CHELSA data for GBIF
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     raw GBIF occurrence data
# DATA OUTPUT:    temp and precip data for each GBIF occurrence point
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)
library(raster)

# Set path to turbo to get data
path_data_gbif = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data_gbif)
# Read in data
gbif_data <- read.csv(" gbif_teracon.csv") # "GBIF_teracon_limited" data in turbo is only for ecoregion 8
# Set path to chelsa data
path_data_chelsa = "/nfs/turbo/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
chelsa_bio12_data <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")

# Extracting lat/long from GBIF data
gbif_coords <- gbif_data %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(chelsa_bio1_data)
crs_raster <- crs(chelsa_bio12_data)

# Assign the CRS to the SpatialPoints
proj4string(gbif_coords) <- crs_raster

# Extracting mean annual temp and precip data for each coordinate
chelsa_bio1_ex <- extract(chelsa_bio1_data, gbif_coords, df = T) 
chelsa_bio12_ex <- extract(chelsa_bio12_data, gbif_coords, df = T) 

# Scaling data correctly
chelsa_bio1_ex_trans <- chelsa_bio1_ex %>%
  mutate(mean_annual_temp = CHELSA_bio1_1981.2010_V.2.1*0.1-273.15)
chelsa_bio12_ex_trans <- chelsa_bio12_ex %>%
  mutate(mean_annual_precip = CHELSA_bio12_1981.2010_V.2.1*0.1)


# Merging occurrence data with chelsa data
gbif_spp_occ <- gbif_data %>%
  dplyr::select(species,decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude)
chelsa_gbif <- cbind(gbif_spp_occ, chelsa_bio1_ex_trans, chelsa_bio12_ex_trans)

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(chelsa_gbif,paste(path_out,'CHELSA_teracon.csv'))
#write.csv(chelsa_gbif,paste(path_out,'CHELSA_teracon_limited.csv')) # when using the limited gbif data (loaded in above)
