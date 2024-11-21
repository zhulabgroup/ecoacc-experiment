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
path_data = "/nfs/turbo/seas-zhukai/datasets/vegetation/PHACE"
setwd(path_data)

# Read in data
phace_data <- read.csv("PHACE biomass by species for Kara.csv", fileEncoding = "Latin1")

# Removing Astralgus spp from total biomass since we can't use genus-only spp names for community comp
phace_rm_spp <- phace_data %>%
  mutate(total_biomass = Aboveground.biomass..g.per.m2. - Astragalus.Spp.)

# Subsetting to only total biomass for ecosystem response data
phace_eco <- phace_rm_spp[,c(1:10,66)]

# Fixing column names and subsetting to data >=2007 when warming treatment began
phace_eco <- phace_eco %>%
  rename_all(~ str_to_lower(.)) %>%
  rename(temp_treatment = temperature) %>%
  rename(co2_treatment = carbon.dioxide) %>%
  filter(year >= 2007)

# Wide to long for biomass values for relative abundance calculation
phace_data_long <- phace_data %>%
  pivot_longer(cols = -c(YEAR,PLOT,COLOR,REP,BLOCK,CO2,carbon.dioxide,TEMP,temperature,Treatment,Aboveground.biomass..g.per.m2.),
               names_to = "species", values_to = "biomass") %>%
  rename(total_biomass = Aboveground.biomass..g.per.m2.)

# Calculating relative abundance and fixing column names
phace_abun <- phace_data_long %>%
  mutate(rel_abun = biomass/total_biomass) %>%
  rename_all(~ str_to_lower(.)) %>%
  rename(temp_treatment = temperature) %>%
  rename(co2_treatment = carbon.dioxide)

# Use gsub to replace periods with spaces
phace_abun$species <- gsub("\\.", " ", phace_abun$species)

# Subsetting data to begin in 2007 when the warming treatment began & removing spp w/ only a genus
phace_abun <- phace_abun %>%
  filter(year >= 2007) %>%
  filter(!(species == "Astragalus Spp "))

# Checking and fixing species names
unique(phace_abun$species)
phace_abun$species[phace_abun$species == "MachaerantheraÊpinnatifida"] <- "Machaeranthera pinnatifida"


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
write.csv(phace_abun,paste(path_out,'phace_clean.csv'), row.names=F)
write.csv(phace_eco,paste(path_out,'phace_ecosystem_dat_clean.csv'), row.names=F)


