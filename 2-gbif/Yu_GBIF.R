# TITLE:          Yu GBIF occurrences
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     clean Yu data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data for each species in Yu dataset
# PROJECT:        EcoAcc
# DATE:           March 2025

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)
library(spThin)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)

# Read in data
yu_data <- read.csv(" yu_clean.csv")


### Pulling out species names
# Create a list for species names with spaces
species_names <- yu_data %>%
  filter(str_detect(species, " "))

species_names_knz <- species_names %>%
  filter(site == "KNZ")
species_names_hys <- species_names %>%
  filter(site == "HYS")
species_names_sgs <- species_names %>%
  filter(site == "SGS")
species_names_chy <- species_names %>%
  filter(site == "CHY")

species_list_knz <- unique(species_names_knz$species)
species_list_hys <- unique(species_names_hys$species)
species_list_sgs <- unique(species_names_sgs$species)
species_list_chy <- unique(species_names_chy$species)


# Create a list for species names without spaces
genus_names <- yu_data %>%
  filter(!str_detect(species, " "))

genus_names_knz <- genus_names %>%
  filter(site == "KNZ")
genus_names_hys <- genus_names %>%
  filter(site == "HYS")
genus_names_sgs <- genus_names %>%
  filter(site == "SGS")
genus_names_chy <- genus_names %>%
  filter(site == "CHY")

genus_list_knz <- unique(genus_names_knz$species)
genus_list_hys <- unique(genus_names_hys$species)
genus_list_sgs <- unique(genus_names_sgs$species)
genus_list_chy <- unique(genus_names_chy$species)



### 1000 km around each experiment location
knz_bbox_1000 <- c(-102, 35, -91, 44)
hys_bbox_1000 <- c(-105, 34, -94, 43)
chy_bbox_1000 <- c(-111, 37, -99, 46)
sgs_bbox_1000 <- c(-111, 36, -99, 45)



### Function to get occurrence data for each spp from GBIF, then cleaning those coordinates
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
spp_occurrences_knz <- occurrences(species_list_knz, knz_bbox_1000)
spp_occurrences_hys <- occurrences(species_list_hys, hys_bbox_1000)
spp_occurrences_chy <- occurrences(species_list_chy, chy_bbox_1000)
spp_occurrences_sgs <- occurrences(species_list_sgs, sgs_bbox_1000)







