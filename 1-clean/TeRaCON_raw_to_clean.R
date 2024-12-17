# TITLE:          TeRaCON data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned TeRaCON experiment data
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/TeRaCON"
setwd(path_data)

# Read in data
teracon_data <- read.csv("teracon harvest file for Kara Dobson_241021.csv")

# Selecting columns to keep
colnames(teracon_data)
teracon_data_sub <- teracon_data[,c(1:9,11,27,109:125)] # for % cover
teracon_data_sub_eco <- teracon_data[,c(1:9,11,27,55,68:69,74,76,78,80,82,84)] # for ecosystem response
teracon_data_sub_eco_blue <- teracon_data[,c(1:9,11,27,42,55,68:69,74,76,78,80,82,84)]

# Removing first test row
teracon_data_sub <- teracon_data_sub[2:1153,]
teracon_data_sub_eco <- teracon_data_sub_eco[2:1153,]
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue[2:1153,]

# Wide to long for just percent cover values
teracon_data_long <- teracon_data_sub %>%
  pivot_longer(cols = -c(Sampling.., year, Season, Ring, Plot, CO2.Treatment, Nitrogen.Treatment, C.and.N.treatment, Water.Treatment, Temp.Treatment,Mean.Amb.May.June.July.Temp..F.),
               names_to = "Species", values_to = "Percent_cover")

# Fixing species names
transform_species_name <- function(name) {
  # Remove the prefix
  name <- sub("^X\\.\\.cover\\.", "", name)
  # Replace periods with spaces
  name <- gsub("\\.", " ", name)
  return(name)
}

# Apply the transformation to the Species column
teracon_data_long$Species <- sapply(teracon_data_long$Species, transform_species_name)

# Scaling temperature data from F to C
teracon_data_long <- teracon_data_long %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)
teracon_data_sub_eco <- teracon_data_sub_eco %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)

# Subsetting to no big bluestem
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue %>%
  mutate(ab_biomass = AbovegroundTotal.Biomass..g.m.2. - Andropogon.gerardi..g.m.2.)
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue[,-c(12,13)]

# Harmonizing column names - these should match across all experiments
teracon_data_long <- teracon_data_long %>%
  rename(plot = Plot) %>%
  rename(species = Species) %>%
  rename(percent_cover = Percent_cover) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment)
teracon_data_sub_eco <- teracon_data_sub_eco %>%
  rename(plot = Plot) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment) %>%
  rename(ab_biomass = AbovegroundTotal.Biomass..g.m.2.) %>%
  rename(bl_biomass = Total.root.biomass.0.20..g.m.2.) %>%
  rename(total_biomass = Total.Biomass) %>%
  rename(root_ingrowth = Annual.Total.Root.Ingrowth..g.m.2.) %>%
  rename(total_n = Whole.Plot.Total.N..g.m.2.) %>%
  rename(bl_n = Belowground.N..total....g.m.2.) %>%
  rename(bl_c = Belowground.Carbon...) %>%
  rename(ab_n = Aboveground.N..total....g.m.2.) %>%
  rename(ab_c = Aboveground.Carbon...) %>%
  mutate(biomass_plus_root = ab_biomass+root_ingrowth)
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue %>%
  rename(plot = Plot) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment) %>%
  rename(bl_biomass = Total.root.biomass.0.20..g.m.2.) %>%
  rename(total_biomass = Total.Biomass) %>%
  rename(root_ingrowth = Annual.Total.Root.Ingrowth..g.m.2.) %>%
  rename(total_n = Whole.Plot.Total.N..g.m.2.) %>%
  rename(bl_n = Belowground.N..total....g.m.2.) %>%
  rename(bl_c = Belowground.Carbon...) %>%
  rename(ab_n = Aboveground.N..total....g.m.2.) %>%
  rename(ab_c = Aboveground.Carbon...) %>%
  mutate(biomass_plus_root = ab_biomass+root_ingrowth)
  

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(teracon_data_long,paste(path_out,'teracon_clean.csv'))
write.csv(teracon_data_sub_eco,paste(path_out,'teracon_ecosystem_dat_clean.csv'))
path_out = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(teracon_data_sub_eco_blue,paste(path_out,'teracon_ecosystem_dat_clean_noblue.csv'))
