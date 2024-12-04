# TITLE:          Pairwise comparisons of cliamte variables
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     raw GBIF occurrence data
# DATA OUTPUT:    temp and precip data for each GBIF occurrence point
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(raster)
library(PerformanceAnalytics)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
setwd(path_data)
# Load in data
phace_500 <- read.csv(" gbif_phace_500.csv")

path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)
# Load in data
tera_500 <- read.csv(" gbif_teracon_500.csv")

# Set path to chelsa data
path_data_chelsa = "/nfs/turbo/seas-zhukai/datasets/climate/CHELSA/climatology/"
setwd(path_data_chelsa)
# Read in data
chelsa_bio1_data <- raster("CHELSA_bio1_1981-2010_V.2.1.tif")
chelsa_bio2_data <- raster("CHELSA_bio2_1981-2010_V.2.1.tif")
chelsa_bio3_data <- raster("CHELSA_bio3_1981-2010_V.2.1.tif")
chelsa_bio4_data <- raster("CHELSA_bio4_1981-2010_V.2.1.tif")
chelsa_bio5_data <- raster("CHELSA_bio5_1981-2010_V.2.1.tif")
chelsa_bio6_data <- raster("CHELSA_bio6_1981-2010_V.2.1.tif")
chelsa_bio7_data <- raster("CHELSA_bio7_1981-2010_V.2.1.tif")
chelsa_bio8_data <- raster("CHELSA_bio8_1981-2010_V.2.1.tif")
chelsa_bio9_data <- raster("CHELSA_bio9_1981-2010_V.2.1.tif")
chelsa_bio10_data <- raster("CHELSA_bio10_1981-2010_V.2.1.tif")
chelsa_bio11_data <- raster("CHELSA_bio11_1981-2010_V.2.1.tif")
chelsa_bio12_data <- raster("CHELSA_bio12_1981-2010_V.2.1.tif")
chelsa_bio13_data <- raster("CHELSA_bio13_1981-2010_V.2.1.tif")
chelsa_bio14_data <- raster("CHELSA_bio14_1981-2010_V.2.1.tif")
chelsa_bio15_data <- raster("CHELSA_bio15_1981-2010_V.2.1.tif")
chelsa_bio16_data <- raster("CHELSA_bio16_1981-2010_V.2.1.tif")
chelsa_bio17_data <- raster("CHELSA_bio17_1981-2010_V.2.1.tif")
chelsa_bio18_data <- raster("CHELSA_bio18_1981-2010_V.2.1.tif")
chelsa_bio19_data <- raster("CHELSA_bio19_1981-2010_V.2.1.tif")

# Extracting coords from gbif data
gbif_coords_500 <- phace_500 %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords_500)<-c("longitude", "latitude")

gbif_coords_500_tera <- tera_500 %>%
  dplyr::select(decimalLatitude,decimalLongitude) %>%
  rename(latitude = decimalLatitude) %>%
  rename(longitude = decimalLongitude) %>%
  relocate(longitude)
coordinates(gbif_coords_500_tera)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(chelsa_bio1_data)

# Assign the CRS to the SpatialPoints
proj4string(gbif_coords_500) <- crs_raster
proj4string(gbif_coords_500_tera) <- crs_raster

# Extracting climate data for each coords
chelsa_bio1_500 <- extract(chelsa_bio1_data, gbif_coords_500, df = T) 
chelsa_bio2_500 <- extract(chelsa_bio2_data, gbif_coords_500, df = T)
chelsa_bio3_500 <- extract(chelsa_bio3_data, gbif_coords_500, df = T) 
chelsa_bio4_500 <- extract(chelsa_bio4_data, gbif_coords_500, df = T)
chelsa_bio5_500 <- extract(chelsa_bio5_data, gbif_coords_500, df = T) 
chelsa_bio6_500 <- extract(chelsa_bio6_data, gbif_coords_500, df = T) 
chelsa_bio7_500 <- extract(chelsa_bio7_data, gbif_coords_500, df = T) 
chelsa_bio8_500 <- extract(chelsa_bio8_data, gbif_coords_500, df = T) 
chelsa_bio9_500 <- extract(chelsa_bio9_data, gbif_coords_500, df = T) 
chelsa_bio10_500 <- extract(chelsa_bio10_data, gbif_coords_500, df = T) 
chelsa_bio11_500 <- extract(chelsa_bio11_data, gbif_coords_500, df = T) 
chelsa_bio12_500 <- extract(chelsa_bio12_data, gbif_coords_500, df = T) 
chelsa_bio13_500 <- extract(chelsa_bio13_data, gbif_coords_500, df = T) 
chelsa_bio14_500 <- extract(chelsa_bio14_data, gbif_coords_500, df = T) 
chelsa_bio15_500 <- extract(chelsa_bio15_data, gbif_coords_500, df = T) 
chelsa_bio16_500 <- extract(chelsa_bio16_data, gbif_coords_500, df = T) 
chelsa_bio17_500 <- extract(chelsa_bio17_data, gbif_coords_500, df = T) 
chelsa_bio18_500 <- extract(chelsa_bio18_data, gbif_coords_500, df = T) 
chelsa_bio19_500 <- extract(chelsa_bio19_data, gbif_coords_500, df = T) 

chelsa_bio1_500_tera <- extract(chelsa_bio1_data, gbif_coords_500_tera, df = T) 
chelsa_bio2_500_tera <- extract(chelsa_bio2_data, gbif_coords_500_tera, df = T)
chelsa_bio3_500_tera <- extract(chelsa_bio3_data, gbif_coords_500_tera, df = T) 
chelsa_bio4_500_tera <- extract(chelsa_bio4_data, gbif_coords_500_tera, df = T)
chelsa_bio5_500_tera <- extract(chelsa_bio5_data, gbif_coords_500_tera, df = T) 
chelsa_bio6_500_tera <- extract(chelsa_bio6_data, gbif_coords_500_tera, df = T) 
chelsa_bio7_500_tera <- extract(chelsa_bio7_data, gbif_coords_500_tera, df = T) 
chelsa_bio8_500_tera <- extract(chelsa_bio8_data, gbif_coords_500_tera, df = T) 
chelsa_bio9_500_tera <- extract(chelsa_bio9_data, gbif_coords_500_tera, df = T) 
chelsa_bio10_500_tera <- extract(chelsa_bio10_data, gbif_coords_500_tera, df = T) 
chelsa_bio11_500_tera <- extract(chelsa_bio11_data, gbif_coords_500_tera, df = T) 
chelsa_bio12_500_tera <- extract(chelsa_bio12_data, gbif_coords_500_tera, df = T) 
chelsa_bio13_500_tera <- extract(chelsa_bio13_data, gbif_coords_500_tera, df = T) 
chelsa_bio14_500_tera <- extract(chelsa_bio14_data, gbif_coords_500_tera, df = T) 
chelsa_bio15_500_tera <- extract(chelsa_bio15_data, gbif_coords_500_tera, df = T) 
chelsa_bio16_500_tera <- extract(chelsa_bio16_data, gbif_coords_500_tera, df = T) 
chelsa_bio17_500_tera <- extract(chelsa_bio17_data, gbif_coords_500_tera, df = T) 
chelsa_bio18_500_tera <- extract(chelsa_bio18_data, gbif_coords_500_tera, df = T) 
chelsa_bio19_500_tera <- extract(chelsa_bio19_data, gbif_coords_500_tera, df = T) 

# Merging
# Custom cbind function to avoid duplicate columns
custom_cbind <- function(...) {
  # Combine all inputs with cbind
  combined <- do.call(cbind, list(...))
  
  # Get the names of the columns
  col_names <- names(combined)
  
  # Identify the first occurrence of each column name
  first_occurrence <- !duplicated(sub("\\..*", "", col_names))
  
  # Subset the combined data to keep only the first occurrence of each column name
  combined <- combined[, first_occurrence]
  
  return(combined)
}
chelsa_all <- custom_cbind(chelsa_bio1_500,chelsa_bio2_500,chelsa_bio3_500,chelsa_bio4_500,
                           chelsa_bio5_500,chelsa_bio6_500,chelsa_bio7_500,chelsa_bio8_500,
                           chelsa_bio9_500,chelsa_bio10_500,chelsa_bio11_500,chelsa_bio12_500,
                           chelsa_bio13_500,chelsa_bio14_500,chelsa_bio15_500,chelsa_bio16_500,
                           chelsa_bio17_500,chelsa_bio18_500,chelsa_bio19_500)
chelsa_all <- chelsa_all[,2:20]

chelsa_all_tera <- custom_cbind(chelsa_bio1_500_tera,chelsa_bio2_500_tera,chelsa_bio3_500_tera,chelsa_bio4_500_tera,chelsa_bio5_500_tera,chelsa_bio6_500_tera,chelsa_bio7_500_tera,chelsa_bio8_500_tera,chelsa_bio9_500_tera,chelsa_bio10_500_tera,chelsa_bio11_500_tera,chelsa_bio12_500_tera,chelsa_bio13_500_tera,chelsa_bio14_500_tera,chelsa_bio15_500_tera,chelsa_bio16_500_tera,chelsa_bio17_500_tera,chelsa_bio18_500_tera,chelsa_bio19_500_tera)
chelsa_all_tera <- chelsa_all_tera[,2:20]

# Subsetting out a random 750 rows for the pairwise correlations
n <- 500
# Set seed for reproducibility
set.seed(123)
# Generate a random sample of row indices
random_rows <- sample(nrow(chelsa_all), n)
random_rows_tera <- sample(nrow(chelsa_all_tera), n)

# Subset the dataframe using the random row indices
chelsa_subset <- chelsa_all[random_rows, ]
chelsa_subset_tera <- chelsa_all_tera[random_rows_tera, ]


# Compute correlation matrix
chart.Correlation(chelsa_subset, histogram=TRUE, pch=19)

