# TITLE:          MAT for each experiment location
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data
# DATA OUTPUT:    MAT estimates for each year at each location
# PROJECT:        EcoAcc
# DATE:           Feb 2025


# Load packages
library(tidyverse)
library(raster)

# Set path to chelsa data
path_data = "/Volumes/seas-zhukai/datasets/climate/TerraClimate/annual_2024_update/metric/"
setwd(path_data)



### Import MAT tif files for all years
# Initialize an empty list to store the raster files
raster_list <- list()
# Define the years
years <- 1998:2024
# Base file path and file name pattern
file_pattern <- "MAT_1_24degree_%d.tif"
# Loop over each year and month
for (year in years) {
  # Construct the file name
  file_name <- sprintf(file_pattern, year)
  file_path <- file.path(path_data, file_name)
  
  # Check if the file exists before reading
  if (file.exists(file_path)) {
    # Read the raster file
    raster_data <- raster(file_path)
    
    # Store the raster data in the list
    raster_list[[paste(year)]] <- raster_data
  } else {
    warning(paste("File does not exist:", file_path))
  }
  }



### Coordinates for the locations of each experiment
locations <- tibble(
  #Experiment = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE"),
  longitude = c(-93, -92.5, -91.8, -97.5, -104.9, -122.2),
  latitude = c(45, 46.7, 47.9, 35, 41.2, 37.4)
)
coordinates(locations)<-c("longitude", "latitude")

# Define the CRS from the raster layer
crs_raster <- crs(raster_data)

# Assign the CRS to the SpatialPoints
proj4string(locations) <- crs_raster



### Extracting mean annual temp for each coordinate
# Initialize a list to store the extracted data
extracted_data_list <- list()
# Loop through each raster in the raster_list
for (name in names(raster_list)) {
  raster_data <- raster_list[[name]]
  
  # Extract data for the given coordinates
  extracted_data <- extract(raster_data, locations, df = TRUE)
  
  # Add the extracted data to the list
  # You can name the data frames using the corresponding raster name
  extracted_data_list[[name]] <- extracted_data
}

# Combine the average yearly data frames into a single data frame if needed
extracted_data_df <- bind_rows(extracted_data_list)
extracted_data_df <- extracted_data_df %>%
  pivot_longer(cols = -c(ID),
               names_to = "year",
               values_to = "MAT") %>%
  filter(!is.na(MAT))
  
# Locations of experiments in dataframe
exp_ID <- tibble(
  site = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE"),
  ID = c(1, 2, 3, 4, 5, 6)
)

# Merge with temperature data
MAT_exp <- left_join(extracted_data_df,exp_ID,by="ID")
MAT_exp <- MAT_exp %>%
  dplyr::select(site, year, MAT)

# Fixing values in year column
MAT_exp$year <- sub(".*_(\\d{4})$", "\\1", MAT_exp$year)




### Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
write.csv(MAT_exp,paste(path_out,'MAT.csv'),row.names=F)
