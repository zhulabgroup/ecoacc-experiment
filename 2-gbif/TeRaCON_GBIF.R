# TITLE:          TeRaCON GBIF occurrences
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     clean teracon data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data for each species in TeRaCON dataset
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)

# Read in data
teracon_data <- read.csv(" teracon_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(teracon_data$species)
species_list1 <- species_list[1:10]
species_list2 <- species_list[11:16]
test_species <- c("Dalea villosa","Lespedeza capitata") # Species that was missing in original GBIF search (Petalostemum villosum)

# 1000 km around experiment location
bbox_1000 <- c(-106, 36, -80, 54)

# Function to get occurrence data for each spp from GBIF, then cleaning those coordinates
occurrences <- function(spp, bbox) {
  
  list_of_occ <- list()
  # Construct the WKT string for the bounding box
  bbox_wkt <- paste0("POLYGON((", 
                     bbox[1], " ", bbox[2], ", ",
                     bbox[3], " ", bbox[2], ", ",
                     bbox[3], " ", bbox[4], ", ",
                     bbox[1], " ", bbox[4], ", ",
                     bbox[1], " ", bbox[2], "))")
  
  for(i in 1:length(spp)){
    key <- name_backbone(spp[[i]])$usageKey
    # Add the spatial predicate for bounding box
    gbif_download <- occ_download(
      pred("taxonKey", key), 
      pred_within(bbox_wkt),
      format = "SIMPLE_CSV"
    )
    occ_download_wait(gbif_download)
    d <- occ_download_get(gbif_download) %>%
      occ_download_import()
    d <- d %>%
      filter(!is.na(decimalLongitude)) %>%
      filter(!is.na(decimalLatitude))
    flags <- clean_coordinates(x = d,
                               lon = "decimalLongitude",
                               lat = "decimalLatitude",
                               countries = "countryCode",
                               species = "species",
                               tests = c("capitals", "centroids",
                                         "duplicates", "equal", "gbif",
                                         "institutions", "seas", "zeros"))
    d_cleaned <- d[flags$.summary,]
    
    list_of_occ[[i]] <- d_cleaned
  }
  return(list_of_occ)
}
# Run function
spp_occurrences1 <- occurrences(species_list1, bbox_1000)
spp_occurrences2 <- occurrences(species_list2, bbox_1000)
test_spp_occurrences <- occurrences(test_species, bbox_1000)

# Pulling lat and long from GBIF
GBIF_species1 <- do.call(rbind.data.frame, spp_occurrences1)
GBIF_species2 <- do.call(rbind.data.frame, spp_occurrences2)
test_GBIF_species <- do.call(rbind.data.frame, test_spp_occurrences)

# Merging the two lists
GBIF_species <- rbind(GBIF_species1, GBIF_species2)

# Checking distribution of occurrences for each species
world <- map_data("world")
distb_occ <- function(data,spp){
  
  spp_data <- data %>%
    filter(species == spp)
  
  ggplot() +
    geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region),
      color = "lightgrey", fill = "darkgrey", size = 0.1
    ) +
    geom_point(
      data = spp_data,
      aes(decimalLongitude, decimalLatitude),
      alpha = 0.7,
      color = "red",
      size=2
    ) +
    theme_classic() +
    labs(x = "Longitude",y = "Latitude") + 
    theme(axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))
}
distb_occ(GBIF_species,"Bouteloua gracilis")



# Accounting for spatial autocorrelation
# Split the dataset by species
species_list <- split(GBIF_species, GBIF_species$species)
test_GBIF_species <- test_GBIF_species %>%
  filter(species == "Dalea villosa")
species_list2 <- split(test_GBIF_species, test_GBIF_species$species)

# Initialize a list to store results
thinned_results <- list()

# Loop through each species and apply thinning
for (species_name in names(species_list2)) {
  cat("Processing species:", species_name, "\n")
  
  species_data <- species_list2[[species_name]]
  
  # Thin data for the current species
  thinned_species <- thin(
    loc.data = species_data,
    lat.col = "decimalLatitude",
    long.col = "decimalLongitude",
    spec.col = "species",
    thin.par = 1,   # Minimum distance between points in kilometers
    reps = 1,        # Number of times to repeat the thinning
    locs.thinned.list.return = TRUE,
    write.files = FALSE,
    write.log.file = FALSE
  )
  
  # Add a species column to the thinned data and store in the list
  thinned_data <- thinned_species[[1]]
  thinned_data$species <- species_name
  thinned_results[[species_name]] <- thinned_data
}

# Combine all thinned data frames into one data frame
thinned_results_df <- do.call(rbind, thinned_results)
row.names(thinned_results_df) <- NULL

# Matching names to teracon data
thinned_results_df$species[thinned_results_df$species == "Dalea villosa"] <- "Petalostemum villosum"

# Merge with the other GBIF data
gbif_data <- gbif_data %>%
  dplyr::select(-c(X))
thinned_results_df <- rbind(gbif_data,thinned_results_df)



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
write.csv(GBIF_species,paste(path_out,'GBIF_teracon.csv'),row.names=F)
write.csv(thinned_results_df,paste(path_out,'GBIF_thinned_teracon.csv'),row.names=F)




### Old code to subset out ecoregion 8 for TeRaCON GBIF data
# This data is within the archived_data on Turbo
gbif_data_limits <- GBIF_species %>%
  filter(decimalLatitude <= 45 & decimalLatitude >= 29) %>%
  filter(decimalLongitude >= -95 & decimalLongitude <= -70)
