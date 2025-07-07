# TITLE:          Extracting TRY trait data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     TRY trait data for all species
# DATA OUTPUT:    Clean trait data
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)
library(rtry)
library(readr)

# Set path to data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc-experiment/TRY_data/raw/"
setwd(path_data)
# Load in data
try <- read_tsv("species_try_traits.txt",col_names = TRUE)

# Selecting trait values, latitude, and longitude
try_filt <- try %>%
  filter(
    grepl("59|60", DataID) | !is.na(TraitID) & is.numeric(TraitID)
  )

# Pull out lat and long per observation
lat_long <- try_filt %>%
  select(ObservationID,DataID,DataName,OrigValueStr) %>%
  filter(DataID %in% c(59,60)) %>%
  pivot_wider(
    id_cols = ObservationID,
    names_from = DataName,
    values_from = OrigValueStr
  )

# Merge lat and long back in with trait data
try_merge <- try_filt %>%
  left_join(lat_long, by = "ObservationID") %>%
  filter(!is.na(TraitID))

# Keep only measurements from the U.S.
try_conus <- try_merge %>%
  filter(
    Latitude >= 24.396308 & Latitude <= 49.384358,  # Filter latitude for CONUS
    Longitude >= -125.0 & Longitude <= -66.93457     # Filter longitude for CONUS
  )
# Really cuts down dataframe, using global for now

# Take the average trait value for each species and trait
trait_avg <- try_merge %>%
  mutate(StdValueNum = as.numeric(StdValue)) %>%
  filter(!is.na(StdValueNum)) %>%
  group_by(AccSpeciesName, TraitID, TraitName) %>%
  summarize(mean_trait_val = mean(StdValueNum, na.rm = TRUE), .groups = 'drop') 


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc-experiment/TRY_data/"
write.csv(try_filt,paste(path_out,'exp_species_traits_all.csv'), row.names=F)
write.csv(trait_avg,paste(path_out,'exp_species_traits.csv'), row.names=F)
