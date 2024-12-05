# TITLE:          B4Warmed data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned B4Warmed experiment data
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/datasets/vegetation/B4Warmed/"
setwd(path_data)

# Read in data
b4_data <- read.csv("2008-2021_B4W_Census_All_wBiomass_02122024.csv")
b4_spp <- read.csv("b4warmed_species_codes.csv")

# Selecting columns to keep
colnames(b4_data)
b4_data_sub <- b4_data[,c(2,6:8,12:14,37,46)] # for % cover

# Fixing species names
b4_data_sub <- left_join(b4_data_sub, b4_spp, by="species")
b4_data_sub <- b4_data_sub[,-c(7)]

# Fixing column names
b4_named <- b4_data_sub %>%
  rename(temp_treatment = heat_trt) %>%
  rename(water_treatment = water_trt) %>%
  rename(plot = plot_id) %>%
  rename(year = census_year) %>%
  rename(rel_abun = current_year_leader_growth_length_cm_) %>%
  rename(ab_biomass = PRed.Formula.C.log.measured.stemM_2023_est) %>%
  rename(species = species_name)


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(b4_named,paste(path_out,'b4warmed_clean.csv'), row.names=F)
