# TITLE:          PHACE data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned PHACE experiment data
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/PHACE"
setwd(path_data)

# Read in data
phace_data <- read.csv("PHACE biomass by species for Kara.csv", fileEncoding = "Latin1")

# Wide to long for biomass values for relative abundance calculation
phace_data_long <- phace_data %>%
  pivot_longer(cols = -c(YEAR,PLOT,COLOR,REP,BLOCK,CO2,carbon.dioxide,TEMP,temperature,Treatment,Aboveground.biomass..g.per.m2.),
               names_to = "species", values_to = "biomass") %>%
  filter(!(species == "Astragalus.Spp.")) # Remove genus-only spp name
phace_data_long$species[phace_data_long$species == "MachaerantheraÊpinnatifida"] <- "Machaeranthera pinnatifida" # Fix spp name

# Calculating relative abundance and fixing column names
phace_abun <- phace_data_long %>%
  group_by(YEAR,PLOT) %>%
  mutate(total_biomass = sum(biomass)) %>%
  mutate(rel_abun = biomass/total_biomass) %>%
  rename_all(~ str_to_lower(.)) %>%
  rename(temp_treatment = temperature) %>%
  rename(co2_treatment = carbon.dioxide)

# Use gsub to replace periods with spaces
phace_abun$species <- gsub("\\.", " ", phace_abun$species)

# Subsetting data to begin in 2007 when the warming treatment began & removing spp w/ only a genus
phace_rel_abun <- phace_abun %>%
  filter(year >= 2007) %>%
  select(year,plot,co2_treatment,temp_treatment,species,rel_abun)

# Selecting biomass data
phace_biomass <- phace_abun %>%
  filter(year >= 2007) %>%
  select(year,plot,co2_treatment,temp_treatment,species,total_biomass)


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
write.csv(phace_rel_abun,paste(path_out,'phace_clean.csv'), row.names=F)
write.csv(phace_biomass,paste(path_out,'phace_ecosystem_dat_clean.csv'), row.names=F)


