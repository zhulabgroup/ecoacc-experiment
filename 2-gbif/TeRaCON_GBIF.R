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

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
path_home = "/home/kcdobson"
setwd(path_data)

# Read in data
teracon_data <- read.csv(" teracon_clean.csv")

### stopped updating here ###
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

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(GBIF_species,paste(path_out,'GBIF_teracon.csv'))
