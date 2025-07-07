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
path_data = "/Volumes/seas-zhukai/datasets/vegetation/B4Warmed/"
setwd(path_data)

# Read in data
b4_data <- read.csv("2008-2021_B4W_Census_All_wBiomass_02122024.csv")
b4_spp <- read.csv("b4warmed_species_codes.csv")

# Selecting columns to keep
colnames(b4_data)
b4_data_sub <- b4_data[,c(2,5:8,12:14,46)]

# Fixing species names
b4_data_sub <- left_join(b4_data_sub, b4_spp, by="species")
b4_data_sub <- b4_data_sub[,-c(8)]

# Fixing column names and back transforming log
b4_named <- b4_data_sub %>%
  rename(temp_treatment = heat_trt) %>%
  rename(water_treatment = water_trt) %>%
  rename(plot = plot_id) %>%
  rename(year = census_year) %>%
  rename(ab_biomass_log = PRed.Formula.C.log.measured.stemM_2023_est) %>%
  rename(species = species_name) %>%
  mutate(ab_biomass = exp(ab_biomass_log))
b4_named <- b4_named[,-c(8)]
b4_named$temp_treatment[b4_named$temp_treatment == "oldAmbient"] <- "amb"

# Calculating relative abundance and fixing column names
b4_abun <- b4_named %>%
  filter(!is.na(year)) %>%
  group_by(year,plot,canopy_condition,site,water_treatment,temp_treatment, species) %>%
  summarize(species_biomass = sum(ab_biomass, na.rm=T)) %>%
  group_by(year,plot,canopy_condition,site,water_treatment,temp_treatment) %>%
  mutate(total_biomass = sum(species_biomass, na.rm=T)) %>%
  ungroup %>%
  mutate(rel_abun = species_biomass/total_biomass) 

# Selecting only plots with open canopy and subsetting to wanted columns for each site
b4_cfc_abun <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "CFC") %>%
  dplyr::select(year,plot,temp_treatment,species,rel_abun)
b4_cfc_biomass <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "CFC") %>%
  rename(ab_biomass = species_biomass) %>%
  dplyr::select(year,plot,temp_treatment,species,ab_biomass)
b4_hwrc_abun <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "HWRC") %>%
  dplyr::select(year,plot,temp_treatment,species,rel_abun)
b4_hwrc_biomass <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "HWRC") %>%
  rename(ab_biomass = species_biomass) %>%
  dplyr::select(year,plot,temp_treatment,species,ab_biomass)



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
write.csv(b4_cfc_abun,paste(path_out,'b4warmed_cfc_clean.csv'), row.names=F)
write.csv(b4_hwrc_abun,paste(path_out,'b4warmed_hwrc_clean.csv'), row.names=F)
write.csv(b4_cfc_biomass,paste(path_out,'b4warmed_cfc_ecosystem_dat_clean.csv'), row.names=F)
write.csv(b4_hwrc_biomass,paste(path_out,'b4warmed_hwrc_ecosystem_dat_clean.csv'), row.names=F)
