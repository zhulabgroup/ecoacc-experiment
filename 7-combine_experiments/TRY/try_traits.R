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
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TRY_data/raw/"
setwd(path_data)
# Load in data
try <- read_tsv("species_try_traits.txt",col_names = TRUE)

# Remove rows w/o trait information
try_filt <- try %>%
  filter(!is.na(TraitID))

# Take the average trait value for each species and trait
trait_avg <- try_filt %>%
  mutate(OrigValueNum = as.numeric(OrigValueStr)) %>%
  filter(!is.na(OrigValueNum)) %>%
  group_by(AccSpeciesName, TraitID, TraitName) %>%
  summarize(mean_trait_val = mean(OrigValueNum, na.rm = TRUE), .groups = 'drop') 


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TRY_data/"
write.csv(trait_avg,paste(path_out,'exp_species_traits.csv'), row.names=F)
