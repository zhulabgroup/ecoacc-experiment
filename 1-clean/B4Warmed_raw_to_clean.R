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
b4_data_sub <- b4_data[,c(2,6:8,12:14,46)]

# Fixing species names
b4_data_sub <- left_join(b4_data_sub, b4_spp, by="species")
b4_data_sub <- b4_data_sub[,-c(7)]

# Fixing column names and back transforming log
b4_named <- b4_data_sub %>%
  rename(temp_treatment = heat_trt) %>%
  rename(water_treatment = water_trt) %>%
  rename(plot = plot_id) %>%
  rename(year = census_year) %>%
  rename(ab_biomass_log = PRed.Formula.C.log.measured.stemM_2023_est) %>%
  rename(species = species_name) %>%
  mutate(ab_biomass = exp(ab_biomass_log))
b4_named <- b4_named[,-c(7)]
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

# Total biomass only
b4_biomass <- b4_abun[,-c(7,8,10)]
b4_biomass <- b4_biomass %>%
  distinct()

# Selecting only plots with open canopy
b4_abun <- b4_abun %>%
  filter(canopy_condition == "Open")
b4_biomass <- b4_biomass %>%
  filter(canopy_condition == "Open")

test <- b4_abun %>%
  group_by(year,plot,site) %>%
  summarize(total_rel_abun = sum(rel_abun))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(b4_abun,paste(path_out,'b4warmed_clean.csv'), row.names=F)
write.csv(b4_biomass,paste(path_out,'b4warmed_ecosystem_dat_clean.csv'), row.names=F)
