# TITLE:          Oklahoma GBIF occurrences
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     clean OK data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data for each species in TeRaCON dataset
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)

# Read in data
ok_data <- read.csv(" ok_clean.csv")

# Fixing species names
ok_data_spp <- ok_data %>%
  filter(!(is.na(species))) %>%
  filter(!(species == "Eleocharis spp")) %>%
  filter(!(species == "Cyperus spp"))

# Removing spaces at the end of species names and capitalizing genus
ok_data_spp$species <- gsub(" $", "", ok_data_spp$species)
ok_data_spp$species[ok_data_spp$species == "solidago drummondii"] <- "Solidago drummondii"

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(ok_data_spp$species)

# 1000 km around experiment location
bbox_1000 <- c(-109, 26, -87, 44)

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
spp_occurrences <- occurrences(spp = species_list)

# Pulling lat and long from GBIF
GBIF_species <- do.call(rbind.data.frame, spp_occurrences)

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


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/OK/"
write.csv(GBIF_species,paste(path_out,'GBIF_ok.csv'))

