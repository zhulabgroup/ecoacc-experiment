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
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
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
species_names_hys$species[species_names_hys$species == "Castille japurpurea"] <- "Castilleja purpurea"
species_names_sgs <- species_names %>%
  filter(site == "SGS")
species_names_chy <- species_names %>%
  filter(site == "CHY")

species_list_knz <- unique(species_names_knz$species)
species_list_knz1 <- species_list_knz[1:6]
species_list_knz2 <- species_list_knz[8:40] # spp not working: Sporobolus asper
species_list_knz3 <- species_list_knz[41:64]
species_list_knz4 <- species_list_knz[65:78]
species_list_hys <- unique(species_names_hys$species)
species_list_hys1 <- species_list_hys[1:30]
species_list_hys2 <- species_list_hys[31:50]
species_list_hys3 <- species_list_hys[51:79]
species_list_hys4 <- species_list_hys[80:104]
species_list_sgs <- unique(species_names_sgs$species)
species_list_sgs1 <- species_list_sgs[1:30]
species_list_sgs2 <- species_list_sgs[31:62]
species_list_chy <- unique(species_names_chy$species)
species_list_chy1 <- species_list_chy[1:35]
species_list_chy2 <- species_list_chy[36:75]


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
    # Print the current species
    cat("Working on species:", spp[[i]], "\n")
    
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
spp_occurrences_knz4 <- occurrences(species_list_knz4, knz_bbox_1000)
sporobolus_asper <- occurrences("Sporobolus asper", knz_bbox_1000)
spp_occurrences_hys4 <- occurrences(species_list_hys4, hys_bbox_1000)
hys_spp_check <- occurrences(c("Draba reptans","Nothocalais cuspidata","Schedonnardus paniculatus"), hys_bbox_1000)
spp_occurrences_sgs2 <- occurrences(species_list_sgs2, sgs_bbox_1000)
spp_occurrences_chy1 <- occurrences(species_list_chy1, chy_bbox_1000)

# Pulling lat and long from GBIF
GBIF_species_knz4 <- do.call(rbind.data.frame, spp_occurrences_knz4)
GBIF_species_sasper <- do.call(rbind.data.frame, sporobolus_asper)
GBIF_species_hys4 <- do.call(rbind.data.frame, spp_occurrences_hys4)
GBIF_species_chy1 <- do.call(rbind.data.frame, spp_occurrences_chy1)
GBIF_species_sgs2 <- do.call(rbind.data.frame, spp_occurrences_sgs2)



### Genus only occurrences
## KNZ genera
name_suggest("Juncus") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key1 <- 2701072
name_suggest("Eleocharis") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key2 <- 2716791

## HYS genera
name_suggest("Triodanis") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key1 <- 7780626
name_suggest("Tradescantia") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key2 <- 2765096
name_suggest("Polygonum") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key3 <- 2889077
name_suggest("Polygala") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key4 <- 3191366
name_suggest("Panicum") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key5 <- 2705064
name_suggest("Helianthus") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key6 <- 3119134
name_suggest("Eriogonum") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key7 <- 8381158
name_suggest("Croton") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key8 <- 3057454
name_suggest("Chloris") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key9 <- 2702742
name_suggest("Astragalus") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key10 <- 2933951
name_suggest("Asclepias") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key11 <- 3170229

## SGS genera
name_suggest("Oenothera") # Check for correct key by pasting it at the end here: https://www.gbif.org/occurrence/search?taxon_key=3248170
genus_key1 <- 3188799


# Construct the WKT string for the bounding box
bbox_wkt_sgs <- paste0("POLYGON((", 
                   sgs_bbox_1000[1], " ", sgs_bbox_1000[2], ", ",
                   sgs_bbox_1000[3], " ", sgs_bbox_1000[2], ", ",
                   sgs_bbox_1000[3], " ", sgs_bbox_1000[4], ", ",
                   sgs_bbox_1000[1], " ", sgs_bbox_1000[4], ", ",
                   sgs_bbox_1000[1], " ", sgs_bbox_1000[2], "))")

# Add the spatial predicate for bounding box
gbif_download <- occ_download(
  pred("taxonKey", genus_key1), # run for each genus
  pred_within(bbox_wkt_sgs),
  format = "SIMPLE_CSV"
)
occ_download_wait(gbif_download)

d1 <- occ_download_get(gbif_download) %>%
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



### Accounting for spatial autocorrelation
# Split the dataset by species
species_split_knz <- split(GBIF_species_knz, GBIF_species_knz$species)
species_split_sasper <- split(GBIF_species_sasper, GBIF_species_sasper$species)
species_split_hys <- split(GBIF_species_hys, GBIF_species_hys$species)
species_split_hys <- species_split_hys[2:114]
species_split_chy <- split(GBIF_species_chy, GBIF_species_chy$species)
species_split_sgs <- split(GBIF_species_sgs, GBIF_species_sgs$species)
# Genera
d1_cleaned$species[d1_cleaned$species == ""] <- "Oenothera"
species_list1 <- split(d1_cleaned, d1_cleaned$species)

# Initialize a list to store results
thinned_results_knz <- list()
thinned_results_sasper <- list()
thinned_results_hys <- list()
thinned_results_chy <- list()
thinned_results_sgs <- list()
# KNZ genera
thinned_results_juncus <- list()
thinned_results_eleocharis <- list()
# HYS genera
thinned_results_triodanis <- list()
thinned_results_Tradescantia <- list()
thinned_results_Polygonum <- list()
thinned_results_Polygala <- list()
thinned_results_Panicum <- list()
thinned_results_Helianthus <- list()
thinned_results_Eriogonum <- list()
thinned_results_Croton <- list()
thinned_results_Chloris <- list()
thinned_results_Astragalus <- list()
thinned_results_Asclepias <- list()
# SGS genera
thinned_results_Oenothera <- list()

# Loop through each species and apply thinning
for (species_name in names(species_split_sasper)) {
  cat("Processing species:", species_name, "\n")
  
  species_data <- species_split_sasper[[species_name]]
  
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
  thinned_results_sasper[[species_name]] <- thinned_data
}

# Combine all thinned data frames into one data frame
thinned_results_sasper_df <- do.call(rbind, thinned_results_sasper)
row.names(thinned_results_sasper_df) <- NULL

# Fixing genus-only name
# KNZ
thinned_results_juncus_df <- thinned_results_juncus_df %>%
  mutate(species = ifelse(grepl("Juncus", species), "Juncus sp", species))
thinned_results_eleocharis_df <- thinned_results_eleocharis_df %>%
  mutate(species = ifelse(grepl("Eleocharis", species), "Eleocharis sp", species))
# HYS
thinned_results_df <- thinned_results_df %>%
  mutate(species = ifelse(grepl("Triodanis", species), "Triodanis sp", species))
thinned_results_Tradescantia_df <- thinned_results_Tradescantia_df %>%
  mutate(species = ifelse(grepl("Tradescantia", species), "Tradescantia sp", species))
thinned_results_Polygonum_df <- thinned_results_Polygonum_df %>%
  mutate(species = ifelse(grepl("Polygonum", species), "Polygonum sp", species))
thinned_results_Polygala_df <- thinned_results_Polygala_df %>%
  mutate(species = ifelse(grepl("Polygala", species), "Polygala sp", species))
thinned_results_Panicum_df <- thinned_results_Panicum_df %>%
  mutate(species = ifelse(grepl("Panicum", species), "Panicum sp", species))
thinned_results_Helianthus_df <- thinned_results_Helianthus_df %>%
  mutate(species = ifelse(grepl("Helianthus", species), "Helianthus sp", species))
thinned_results_Eriogonum_df <- thinned_results_Eriogonum_df %>%
  mutate(species = ifelse(grepl("Eriogonum", species), "Eriogonum sp", species))
thinned_results_Croton_df <- thinned_results_Croton_df %>%
  mutate(species = ifelse(grepl("Croton", species), "Croton sp", species))
thinned_results_Chloris_df <- thinned_results_Chloris_df %>%
  mutate(species = ifelse(grepl("Chloris", species), "Chloris sp", species))
thinned_results_Astragalus_df <- thinned_results_Astragalus_df %>%
  mutate(species = ifelse(grepl("Astragalus", species), "Astragalus sp", species))
thinned_results_df <- thinned_results_df %>%
  mutate(species = ifelse(grepl("Asclepias", species), "Asclepias sp", species))
# SGS
thinned_results_Oenothera_df <- thinned_results_Oenothera_df %>%
  mutate(species = ifelse(grepl("Anogra", species), "Oenothera sp", species)) %>%
  mutate(species = ifelse(grepl("Oenothera", species), "Oenothera sp", species)) %>%
  mutate(species = ifelse(grepl("Gaura", species), "Oenothera sp", species))

# Merge with the other GBIF data
thinned_results_df <- rbind(GBIF_knz,thinned_results_juncus_df,thinned_results_eleocharis_df)
thinned_results_df <- rbind(GBIF_hys,thinned_results_triodanis_df,thinned_results_Tradescantia_df,
                            thinned_results_Polygonum_df,thinned_results_Polygala_df,thinned_results_Panicum_df,
                            thinned_results_Helianthus_df,thinned_results_Eriogonum_df,thinned_results_Croton_df,
                            thinned_results_Chloris_df,thinned_results_Astragalus_df,thinned_results_Asclepias_df)
thinned_results_df <- rbind(thinned_results_sgs_df,thinned_results_Oenothera_df)


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
write.csv(thinned_results_df,paste(path_out,'GBIF_knz.csv'),row.names=F)
write.csv(GBIF_species_chy1,paste(path_out,'GBIF_chy1.csv'),row.names=F)




##### Fig: Checking distribution of occurrences for each species
library(maps)
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
      aes(Longitude, Latitude),
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
# Plot occurrence maps for abundant species in each experiment
distb_occ(thinned_results_df,"Aristida purpurea")
