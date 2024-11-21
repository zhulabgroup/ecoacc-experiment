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

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)

# Read in data
phace_data <- read.csv(" phace_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(phace_data$species)
species_list1 <- species_list[1:25]
species_list2 <- species_list[26:53]

# Many occurrences exist outside of the U.S.
# Because PHACE is in ecoregion 9 (EPA), I'm limiting the occurrences to a rough bounding box around that ecoregion
# Northwest corner: approximately 49.0° N, 105.0° W (near the border of Canada and Montana/North Dakota)
# Northeast corner: approximately 49.0° N, 95.0° W (near the border of Canada and Minnesota)
# Southwest corner: approximately 25.0° N, 105.0° W (along the Texas/New Mexico border)
# Southeast corner: approximately 25.0° N, 95.0° W (along the Texas gulf coast)
# bounding box limits = c(min_longitude, min_latitude, max_longitude, max_latitude)
bbox <- c(-105, 25, -95, 49)

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
spp_occurrences1 <- occurrences(species_list1, bbox)
spp_occurrences2 <- occurrences(species_list2, bbox)

# Pulling lat and long from GBIF
GBIF_species1 <- do.call(rbind.data.frame, spp_occurrences1)
GBIF_species2 <- do.call(rbind.data.frame, spp_occurrences2)

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
distb_occ(GBIF_species,"Allium textile")


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
write.csv(GBIF_species,paste(path_out,'GBIF_phace_limited.csv'))
