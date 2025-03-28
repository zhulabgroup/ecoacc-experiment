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
library(spThin)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)

# Read in data
ok_data <- read.csv(" ok_clean.csv")

# Fixing species names (genus-only occurrence download is below)
ok_data_spp <- ok_data %>%
  filter(!(is.na(species))) %>%
  filter(!(species == "Eleocharis sp")) %>%
  filter(!(species == "Cyperus sp"))

# Removing spaces at the end of species names if needed
ok_data_spp$species <- gsub(" $", "", ok_data_spp$species)

# Making a list of the species in our experimental data set for GBIF occurrences
species_list <- unique(ok_data_spp$species)
species_list1 <- species_list[1:36]
species_list2 <- species_list[37:65]
species_list3 <- species_list[68:100] # note: Rubus calycinoides and Rubus pentalobus (66 and 67) don't run
species_list4 <- species_list[101:110]
species_list5 <- species_list[c(111:118,120)] # note: 119 (Plantago asiatica), 121 (Tragia cannabina) and 122 (Trifolium purpureum) don't run
species_list6 <- c("Bromus japonicus","Dactylis glomerata","Apocynum cannabinum","Stenaria nigricans") # Selecting spp not found in original meta-data
test_species <- c("Rubus rolfei","Plantago asiatica","Tragia cannabina","Trifolium purpureum") # Testing spp that didn't work above; testing with synonyms        
"Rubus calycinoides"
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
spp_occurrences1 <- occurrences(species_list1, bbox_1000)
spp_occurrences2 <- occurrences(species_list2, bbox_1000)
spp_occurrences3 <- occurrences(species_list3, bbox_1000)
spp_occurrences4 <- occurrences(species_list4, bbox_1000)
spp_occurrences5 <- occurrences(species_list5, bbox_1000)
spp_occurrences6 <- occurrences(species_list6, bbox_1000)
test_spp_occurrences <- occurrences(test_species, bbox_1000)

# Pulling lat and long from GBIF
GBIF_species1 <- do.call(rbind.data.frame, spp_occurrences1)
GBIF_species2 <- do.call(rbind.data.frame, spp_occurrences2)
GBIF_species3 <- do.call(rbind.data.frame, spp_occurrences3)
GBIF_species4 <- do.call(rbind.data.frame, spp_occurrences4)
GBIF_species5 <- do.call(rbind.data.frame, spp_occurrences5)
GBIF_species6 <- do.call(rbind.data.frame, spp_occurrences6)

# Merging into one file
gbif1 <- read.csv(" GBIF_ok_species1.csv")
gbif2 <- read.csv(" GBIF_ok_species2.csv")
gbif3 <- read.csv(" GBIF_ok_species3.csv")
gbif4 <- read.csv(" GBIF_ok_species4.csv")
gbif5 <- read.csv(" GBIF_ok_species5.csv")

GBIF_species6$X <- NA
GBIF_species <- rbind(gbif1,gbif2,gbif3,gbif4,gbif5,GBIF_species6)

# Fixing species names to match the raw data
GBIF_species$species[GBIF_species$species == "Ambrosia trifida"] <- "Ambrosia trida"
GBIF_species$species[GBIF_species$species == "Andropogon gerardi"] <- "Andropogon gerardii"
GBIF_species$species[GBIF_species$species == "Oenothera serrulata"] <- "Calylophus serrulatus"
GBIF_species$species[GBIF_species$species == "Erigeron canadensis"] <- "Conyza canadensis"
GBIF_species$species[GBIF_species$species == "Spermacoce ocymifolia"] <- "Diodia ocymifolia"
GBIF_species$species[GBIF_species$species == "Hexasepalum teres"] <- "Diodia teres"
GBIF_species$species[GBIF_species$species == "Oenothera curtiflora"] <- "Gaura parviflora"
GBIF_species$species[grepl("Grindelia papposa", GBIF_species$scientificName)] <- "Grindelia papposa"
GBIF_species$species[grepl("Haplopappus ciliatus", GBIF_species$scientificName)] <- "Haplopappus ciliatus"
GBIF_species$species[grepl("Hedyotis nigricans", GBIF_species$scientificName)] <- "Hedyotis nigricans"
GBIF_species$species[GBIF_species$species == "Heterotheca subaxillaris"] <- "Heterotheca latifolia"
GBIF_species$species[GBIF_species$species == "Melilotus officinalis"] <- "Melilotus alba"
GBIF_species$species[GBIF_species$species == "Mimosa quadrivalvis"] <- "Mimosa nuttallii"
GBIF_species$species[grepl("Mimosa rupertiana", GBIF_species$scientificName)] <- "Mimosa rupertiana"
GBIF_species$species[GBIF_species$species == "Muhlenbergia capillaris"] <- "Muhlenbergia capillaris "
GBIF_species$species[GBIF_species$species == "Solidago drummondii"] <- "solidago drummondii"
GBIF_species$species[GBIF_species$species == "Solidago ludoviciana"] <- "Solidago ludiviciana"
GBIF_species$species[GBIF_species$species == "Oenothera glaucifolia"] <- "Stenosiphon linifolius"
GBIF_species$species[GBIF_species$species == "Nassella tenuissima"] <- "Stipa tenuissima"
GBIF_species$species[GBIF_species$species == "Symphyotrichum ericoides"] <- "Symphyotrichum ericoides "
GBIF_species$species[GBIF_species$species == "Symphyotrichum ontarionis"] <- "Symphyotrichum ontarionsis"
GBIF_species$species[GBIF_species$species == "Trifolium purpureum"] <- "Trifolium purpureum "
GBIF_species$species[GBIF_species$species == "Viola rafinesquei"] <- "Viola bicolor"
GBIF_species$species[GBIF_species$species == "Pediomelum tenuiflorum"] <- "Psoralidium tenuiflorum"
GBIF_species$species[GBIF_species$genus == "Strobilanthes"] <- "Ruellia hirta"



### Genus only occurrences
# Getting the genus key for Astragalus
name_suggest("Eleocharis") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key1 <- 2716791
name_suggest("Cyperus") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key2 <- 2713455

# Construct the WKT string for the bounding box
bbox_wkt <- paste0("POLYGON((", 
                   bbox_1000[1], " ", bbox_1000[2], ", ",
                   bbox_1000[3], " ", bbox_1000[2], ", ",
                   bbox_1000[3], " ", bbox_1000[4], ", ",
                   bbox_1000[1], " ", bbox_1000[4], ", ",
                   bbox_1000[1], " ", bbox_1000[2], "))")

# Add the spatial predicate for bounding box
gbif_download <- occ_download(
  pred("taxonKey", genus_key2), # run for each genus
  pred_within(bbox_wkt),
  format = "SIMPLE_CSV"
)
occ_download_wait(gbif_download)

d1 <- occ_download_get(gbif_download) %>% # first genus
  occ_download_import()
d1 <- d1 %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))
flags1 <- clean_coordinates(x = d1,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "duplicates", "equal", "gbif",
                                     "institutions", "seas", "zeros"))
d1_cleaned <- d1[flags1$.summary,]

d2 <- occ_download_get(gbif_download) %>% # second genus
  occ_download_import()
d2 <- d2 %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))
flags2 <- clean_coordinates(x = d2,
                            lon = "decimalLongitude",
                            lat = "decimalLatitude",
                            countries = "countryCode",
                            species = "species",
                            tests = c("capitals", "centroids",
                                      "duplicates", "equal", "gbif",
                                      "institutions", "seas", "zeros"))
d2_cleaned <- d2[flags2$.summary,]



### Accounting for spatial autocorrelation
# Split the dataset by species
species_list <- split(GBIF_species, GBIF_species$species)
species_list <- species_list[-1]
d1_cleaned$species[d1_cleaned$species == ""] <- "Eleocharis"
species_list2 <- split(d1_cleaned, d1_cleaned$species)
d2_cleaned$species[d2_cleaned$species == ""] <- "Cyperus"
species_list3 <- split(d2_cleaned, d2_cleaned$species)

# Initialize a list to store results
thinned_results <- list()
thinned_results2 <- list()

# Loop through each species and apply thinning
for (species_name in names(species_list3)) {
  cat("Processing species:", species_name, "\n")
  
  species_data <- species_list3[[species_name]]
  
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
  thinned_results2[[species_name]] <- thinned_data
}

# Combine all thinned data frames into one data frame
thinned_results_df <- do.call(rbind, thinned_results)
row.names(thinned_results_df) <- NULL
thinned_results_df2 <- do.call(rbind, thinned_results2)
row.names(thinned_results_df2) <- NULL

# Fixing genus-only name
thinned_results_df <- thinned_results_df %>%
  mutate(species = ifelse(grepl("Elaeocharis", species), "Eleocharis sp", species)) %>%
  mutate(species = ifelse(grepl("Eleocharis", species), "Eleocharis sp", species)) %>%
  mutate(species = ifelse(grepl("Bulbostylis", species), "Eleocharis sp", species))
thinned_results_df2 <- thinned_results_df2 %>%
  mutate(species = ifelse(grepl("Cyperus", species), "Cyperus sp", species))

# Merge with the other GBIF data
thinned_results_df <- rbind(gbif_data,thinned_results_df,thinned_results_df2)



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
write.csv(GBIF_species1,paste(path_out,'GBIF_ok_species1.csv')) # these intermediate files were moved to archive
write.csv(GBIF_species2,paste(path_out,'GBIF_ok_species2.csv'))
write.csv(GBIF_species3,paste(path_out,'GBIF_ok_species3.csv'))
write.csv(GBIF_species4,paste(path_out,'GBIF_ok_species4.csv'))
write.csv(GBIF_species5,paste(path_out,'GBIF_ok_species5.csv'))
write.csv(GBIF_species,paste(path_out,'GBIF_ok.csv'), row.names=F)
write.csv(thinned_results_df,paste(path_out,'GBIF_thinned_ok.csv'), row.names=F)




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
distb_occ(GBIF_species5,"Croton monanthogynus")

