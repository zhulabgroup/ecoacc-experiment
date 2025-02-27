# TITLE:          PHACE GBIF occurrences
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean PHACE data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data for each species in PHACE dataset
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)
library(spThin)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)

# Read in data
phace_data <- read.csv(" phace_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(phace_data$species)
species_list1 <- species_list[1:25]
species_list2 <- species_list[26:53]
test_species <- "Astragalus"

# Testing the impacts of spp. occurrences scale on results
# Decided to ultimately use a 1000km buffer around the US and Canada; below on line 86 subsets out the 1000km buffer
# bounding box limits = c(min_longitude, min_latitude, max_longitude, max_latitude)
# U.S. and Canada
bbox_uscan <- c(-141, 24, -53, 83)
bbox_1000 <- c(-119, 32, -93, 50)

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
    #key <- name_backbone(spp[[i]])$usageKey
    key <- name_backbone(name=spp[[i]],rank="genus")$usageKey
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
spp_occurrences1 <- occurrences(species_list1, bbox_uscan)
spp_occurrences2 <- occurrences(species_list2, bbox_uscan)

# Pulling lat and long from GBIF
GBIF_species1 <- do.call(rbind.data.frame, spp_occurrences1)
GBIF_species2 <- do.call(rbind.data.frame, spp_occurrences2)

# Merging the two lists
GBIF_species <- rbind(GBIF_species1, GBIF_species2)

# Filtering 1000km from the US and Canada data
gbif_data_1000 <- GBIF_species %>%
  filter(decimalLatitude <= 50 & decimalLatitude >= 32) %>%
  filter(decimalLongitude >= -117 & decimalLongitude <= -93)



### Genus only occurrences
# Getting the genus key for Astragalus
name_suggest("Astragalus") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key <- 2933951

# Construct the WKT string for the bounding box
bbox_wkt <- paste0("POLYGON((", 
                   bbox_1000[1], " ", bbox_1000[2], ", ",
                   bbox_1000[3], " ", bbox_1000[2], ", ",
                   bbox_1000[3], " ", bbox_1000[4], ", ",
                   bbox_1000[1], " ", bbox_1000[4], ", ",
                   bbox_1000[1], " ", bbox_1000[2], "))")

# Add the spatial predicate for bounding box
gbif_download <- occ_download(
    pred("taxonKey", genus_key), 
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



### Accounting for spatial autocorrelation
# Split the dataset by species
species_list <- split(GBIF_species, GBIF_species$species)
species_list2 <- split(d_cleaned, d_cleaned$genus) # Genus-only

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

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
write.csv(gbif_data_1000,paste(path_out,'GBIF_phace.csv'))
write.csv(thinned_results_df,paste(path_out,'GBIF_thinned_phace.csv'))



### Checking distribution of occurrences for each species
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
distb_occ(GBIF_species,"Allium textile")



### Old code for the bounding box for ecoregion 9
# The data for ecoregion 9 GBIF occurrences is in the archived data on Turbo
bbox <- c(-105, 25, -95, 49)
