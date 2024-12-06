# TITLE:          B4Warmed GBIF occurrences
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean B4Warmed data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data for each species in B4W dataset
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)

# Read in data
b4_data <- read.csv(" b4warmed_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
# Separate lists for the CFC and HWRC sites
b4_cfc <- b4_data %>%
  filter(site == "CFC") %>%
  filter(!is.na(species))
b4_hwrc <- b4_data %>%
  filter(site == "HWRC") %>%
  filter(!is.na(species))
species_list_cfc <- unique(b4_cfc$species)
species_list_hwrc <- unique(b4_hwrc$species)

# Testing the impacts of spp. occurrences scale on results
# bounding box limits = c(min_longitude, min_latitude, max_longitude, max_latitude)
# 1000km around CFC
bbox_cfc_1000 <- c(-105, 38, -80, 58)
bbox_hwrc_1000 <- c(-105, 39, -79, 57)

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
spp_occurrences_cfc <- occurrences(species_list_cfc, bbox_cfc_1000)
spp_occurrences_hwrc <- occurrences(species_list_hwrc, bbox_hwrc_1000)

# Pulling lat and long from GBIF
gbif_spp_cfc <- do.call(rbind.data.frame, spp_occurrences_cfc)
gbif_spp_hwrc <- do.call(rbind.data.frame, spp_occurrences_hwrc)

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
distb_occ(gbif_spp_cfc,"Abies balsamea")
distb_occ(gbif_spp_hwrc,"Abies balsamea")


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(gbif_spp_cfc,paste(path_out,'GBIF_b4warmed_cfc.csv'))
write.csv(gbif_spp_hwrc,paste(path_out,'GBIF_b4warmed_hwrc.csv'))


