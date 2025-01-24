# TITLE:          JRGCE GBIF occurrences and CHELSA data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean JRGCE data read in
# DATA OUTPUT:    GBIF and CHELSA data for spp in JRGCE
# PROJECT:        EcoAcc
# DATE:           Nov 2024


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
# Decided to ultimately use a 1000km buffer around the US and Canada; below on line 82 subsets out the 1000km buffer
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

# Filtering 1000km from the US and Canada data
gbif_data_1000 <- gbif_spp_uscan %>%
  filter(decimalLatitude <= 46 & decimalLatitude >= 28) %>%
  filter(decimalLongitude >= -134 & decimalLongitude <= -111)

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
distb_occ(gbif_data_1000,"Avena fatua")



# Accounting for spatial autocorrelation
# Split the dataset by species
species_list <- split(gbif_data_1000, gbif_data_1000$species)

# Initialize a list to store results
thinned_results <- list()

# Loop through each species and apply thinning
for (species_name in names(species_list)) {
  cat("Processing species:", species_name, "\n")
  
  species_data <- species_list[[species_name]]
  
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
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(gbif_data_1000,paste(path_out,'GBIF_jrgce.csv'))
write.csv(thinned_results_df,paste(path_out,'GBIF_thinned_jrgce.csv'))





### Old code to import GBIF data from Zhu, Song et al. Grassland paper
# This data is in the archived data for JRGCE
# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-grassland-cfp/intermediate/climate-niche/"
path_clean_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_clean_data)

# Read in data
gbif_jrgce <- readRDS("gbif-chelsa.rds") #GBIF and CHELSA data were already downloaded for grassland project
jrgce <- read.csv(" jrgce_clean.csv")

# Make a species list; we only want to keep spp in the gbif data that are in the jrgce data
spp_list <- unique(jrgce$species)

# Filter the gbif data to those species
gbif_jrgce_filtered <- gbif_jrgce %>%
  filter(species %in% spp_list)

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(gbif_jrgce_filtered,paste(path_out,'CHELSA_GBIF_jrgce.csv'),row.names=F)
