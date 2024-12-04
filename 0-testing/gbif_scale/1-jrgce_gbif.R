# TITLE:          JRGCE GBIF scale of occurrences test
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean JRGCE data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data at varying scales for each species in JRGCE dataset
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(terra)
library(rgbif)
library(CoordinateCleaner)
library(maps)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)

# Read in data
jrgce_data <- read.csv(" jrgce_clean.csv")

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(jrgce_data$species)
species_list <- species_list[species_list != "Festuca DUMMY"] # removing non-spp name
species_list <- species_list[species_list != "Avena DUMMY"] # removing non-spp name
print(species_list)

# Testing the impacts of spp. occurrences scale on results
# bounding box limits = c(min_longitude, min_latitude, max_longitude, max_latitude)
# U.S. and Canada
bbox_uscan <- c(-141, 24, -53, 83)

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
spp_occurrences_uscan <- occurrences(species_list, bbox_uscan)

# Pulling lat and long from GBIF
gbif_spp_uscan <- do.call(rbind.data.frame, spp_occurrences_uscan)

# Filtering 1000km and 500km from the US and Canada data
gbif_data_1000 <- gbif_spp_uscan %>%
  filter(decimalLatitude <= 46 & decimalLatitude >= 28) %>%
  filter(decimalLongitude >= -134 & decimalLongitude <= -111)
gbif_data_500 <- gbif_spp_uscan %>%
  filter(decimalLatitude <= 42 & decimalLatitude >= 33) %>%
  filter(decimalLongitude >= -128 & decimalLongitude <= -117)

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
distb_occ(gbif_spp_uscan,"Avena fatua")
distb_occ(gbif_data_1000,"Avena fatua")
distb_occ(gbif_data_500,"Avena fatua")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/data_for_testing/"
write.csv(gbif_spp_uscan,paste(path_out,'gbif_jrgce_uscan.csv'))
write.csv(gbif_data_1000,paste(path_out,'gbif_jrgce_1000.csv'))
write.csv(gbif_data_500,paste(path_out,'gbif_jrgce_500.csv'))
