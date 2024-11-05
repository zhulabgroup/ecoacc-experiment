# TITLE:          JRGCE GBIF occurrences and CHELSA data
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean JRGCE data read in
# DATA OUTPUT:    GBIF and CHELSA data for spp in JRGCE
# PROJECT:        EcoAcc
# DATE:           Nov 2024

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
