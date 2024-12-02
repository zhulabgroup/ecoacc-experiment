# TITLE:          TeRaCON GBIF scale of occurrences test
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     clean teracon data read in to get species list
# DATA OUTPUT:    raw GBIF occurrence data at varying scales for each species in TeRaCON dataset
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
species_list <- species_list[species_list != "Total Planted Species"] # removing non-spp name
index_to_change <- which(species_list == "Petalostemum villosum") # fixing spp name
species_list[index_to_change] <- "Dalea villosa"


# Testing the impacts of spp. occurrences scale on results
# bounding box limits = c(min_longitude, min_latitude, max_longitude, max_latitude)
# U.S. and Canada
bbox_uscan <- c(-141, 24, -53, 83)
# 1000 km around experiment location
bbox_1000 <- c(-106, 36, -80, 54)
# 500 km around experiment location
bbox_500 <- c(-98, 41, -89, 50)
# Ecoregion 8
bbox_eco8 <- c(-95, 29, -70, 45)

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
spp_occurrences_1000 <- occurrences(species_list, bbox_1000)
spp_occurrences_500 <- occurrences(species_list, bbox_500)
spp_occurrences_eco8 <- occurrences(species_list, bbox_eco8)

# Pulling lat and long from GBIF
gbif_spp_uscan <- do.call(rbind.data.frame, spp_occurrences_uscan)
gbif_spp_1000 <- do.call(rbind.data.frame, spp_occurrences_1000)
gbif_spp_500 <- do.call(rbind.data.frame, spp_occurrences_500)


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
distb_occ(gbif_spp_500,"Achillea millefolium")



# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(gbif_spp_uscan,paste(path_out,'gbif_teracon_uscan.csv'))
write.csv(gbif_spp_1000,paste(path_out,'gbif_teracon_1000.csv'))
write.csv(gbif_spp_500,paste(path_out,'gbif_teracon_500.csv'))

