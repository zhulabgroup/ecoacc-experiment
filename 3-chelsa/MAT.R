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
path_data_chelsa = "/Volumes/seas-zhukai/datasets/climate/CHELSA/monthly/tas/raw/"
setwd(path_data_chelsa)



### Initialize an empty list to store the raster files
raster_list <- list()
# Define the months and years
months <- sprintf("%02d", 1:12)  # all months
years <- 1998:2023
# Base file path and file name pattern
file_pattern <- "CHELSA_tas_%s_%d_V.2.1.tif"  # %s for month, %d for year
# Loop over each year and month
for (year in years) {
  for (month in months) {
    # Construct the file name
    file_name <- sprintf(file_pattern, month, year)
    file_path <- file.path(path_data_chelsa, file_name)
    
    # Check if the file exists before reading
    if (file.exists(file_path)) {
      # Read the raster file
      raster_data <- raster(file_path)
      
      # Store the raster data in the list
      raster_list[[paste(year, month, sep = "_")]] <- raster_data
    } else {
      warning(paste("File does not exist:", file_path))
    }
  }
}


### Lat and long of each experiment's location
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



### Take MAT for each year
# Function to extract year from source name
extract_year <- function(source_name) {
  parts <- unlist(strsplit(source_name, "_"))[1]
  return(parts)
}
# Initialize a list to store yearly data frames
yearly_data_list <- list()
# Loop through each source in the list
for (source in names(extracted_data_list)) {
  # Extract the year from the source name
  year <- extract_year(source)
  
  # Get the data frame
  data <- extracted_data_list[[source]]
  
  # Add the year as a column
  data$Year <- year
  
  # Add the data frame to the yearly list, combining by year
  # If the year already exists in the list, bind the rows; otherwise, create a new entry
  if (year %in% names(yearly_data_list)) {
    yearly_data_list[[as.character(year)]] <- full_join(yearly_data_list[[as.character(year)]], data, by="ID")
  } else {
    yearly_data_list[[as.character(year)]] <- data
  }
}
# Function to clean data frames in yearly_data_list
clean_year_columns <- function(df) {
  # Identify year columns using regular expressions
  year_columns <- grep("^Year", colnames(df), value = TRUE)
  
  if (length(year_columns) > 1) {
    # If multiple year columns exist, keep only the first one
    df <- df %>% dplyr::select(-one_of(year_columns[-1]))
    
    # Rename the remaining year column to "Year" if needed
    colnames(df)[colnames(df) == year_columns[1]] <- "Year"
  }
  
  return(df)
}
# Apply cleaning function to each data frame in yearly_data_list
yearly_data_list <- lapply(yearly_data_list, clean_year_columns)



### Create a final list to store average yearly data
average_yearly_data_list <- list()
# Loop through each year in the yearly data list
for (year in names(yearly_data_list)) {
  yearly_data <- yearly_data_list[[year]]
  
  # Melt the data to long format
  long_data <- yearly_data %>%
    pivot_longer(
      cols = starts_with("CHELSA_tas_"),
      names_to = "Month",
      values_to = "Temperature"
    )
  
  # Calculate the average temperature for each ID
  avg_data <- long_data %>%
    group_by(ID) %>%
    summarize(Average_Temperature = mean(Temperature, na.rm = TRUE)) %>%
    ungroup()
  
  # Add the year column
  avg_data$Year <- as.numeric(year)
  
  # Store the averaged data in the final list
  average_yearly_data_list[[year]] <- avg_data
}

# Combine the average yearly data frames into a single data frame if needed
average_yearly_df <- bind_rows(average_yearly_data_list)

# Scaling data correctly
avg_temp <- average_yearly_df %>%
  mutate(MAT = Average_Temperature*0.1-273.15)

# Locations of experiments
exp_ID <- tibble(
  Experiment = c("TeRaCON", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "PHACE", "JRGCE"),
  ID = c(1, 2, 3, 4, 5, 6)
)

# Merge with temperature data
MAT_exp <- left_join(avg_temp,exp_ID,by="ID")
MAT_exp <- MAT_exp %>%
  dplyr::select(Experiment, Year, MAT)



### Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/"
write.csv(MAT_exp,paste(path_out,'MAT.csv'),row.names=F)
  
  
  
  
