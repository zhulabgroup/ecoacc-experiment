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
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Read in data
teracon_data <- read.csv(" teracon_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(teracon_data$species)
species_list1 <- species_list[1:10]
species_list2 <- species_list[11:16]

# Function to get occurrence data for each spp from GBIF, then cleaning those coordinates
occurrences <- function(spp) {
  
  list_of_occ <- list()
  
  for(i in 1:length(spp)){
    key <- name_backbone(spp[[i]])$usageKey
    gbif_download <- occ_download(pred("taxonKey", key),format = "SIMPLE_CSV")
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
                                         "institutions", "seas","zeros"))
    d_cleaned <- d[flags$.summary,]
    
    list_of_occ[[i]] <- d_cleaned
  }
  return(list_of_occ)
}
# Run function
spp_occurrences1 <- occurrences(spp = species_list1)
spp_occurrences2 <- occurrences(spp = species_list2)

# Pulling lat and long from GBIF
GBIF_species1 <- do.call(rbind.data.frame, spp_occurrences1)
GBIF_species2 <- do.call(rbind.data.frame, spp_occurrences2)

# Merging the two lists
GBIF_species <- rbind(GBIF_species1, GBIF_species2)

# Many occurrences outside of the U.S.
# Because TeRaCON is in ecoregion 8 (EPA), I'm limiting the occurrences to a rough bounding box around that ecoregion
# Northwest corner: approximately 45.0° N, 95.0° W (Western boundary in Minnesota)
# Northeast corner: approximately 45.0° N, 70.0° W (Northeastern boundary in Maine)
# Southwest corner: approximately 29.0° N, 95.0° W (Southern boundary in Eastern Texas)
# Southeast corner: approximately 29.0° N, 80.0° W (Southern boundary in Florida)
# Latitude limits: <45 but >29
# Longitude limits: <95 but >70
gbif_data_limits <- GBIF_species %>%
  filter(decimalLatitude <= 45 & decimalLatitude >= 29) %>%
  filter(decimalLongitude >= -95 & decimalLongitude <= -70)

# Checking distribution of occurrences for each species
gbif_data_limits <- read.csv(" GBIF_teracon_limited.csv") # reading in data if I just want to make the map
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
distb_occ(gbif_data_limits,"Bouteloua gracilis")


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(GBIF_species,paste(path_out,'GBIF_teracon.csv'))
write.csv(gbif_data_limits,paste(path_out,'GBIF_teracon_limited.csv'))
