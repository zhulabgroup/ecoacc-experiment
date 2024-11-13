# TITLE:          BioCON data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned BioCON
# PROJECT:        EcoAcc
# DATE:           Nov 2024


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/datasets/vegetation/TeRaCON"
path_home = "/home/kcdobson"
setwd(path_data)

# Read in data
biocon <- read.csv("BioCON Master Harvest_240820_for PR.csv")

# Selecting columns to keep
colnames(biocon)
biocon_sub <- biocon[,c(2,9,11:15,17,33,38:39,41,116:131)] # for % cover

# Removing first test row
biocon_sub <- biocon_sub[2:19293,]

# Wide to long for just percent cover values
biocon_long <- biocon_sub %>%
  pivot_longer(cols = -c(year,Plot,Season,CO2.Treatment,Nitrogen.Treatment,C.and.N.treatment,
                         Water.Treatment,Temp.Treatment,Mean.Amb.May.June.July.Temp..F.,CountOfSpecies,CountOfGroup,Experiment),
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
biocon_long$Species <- sapply(biocon_long$Species, transform_species_name)

# Scaling temperature data from F to C
biocon_long <- biocon_long %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)

# Harmonizing column names - these should match across all experiments
biocon_long <- biocon_long %>%
  rename(plot = Plot) %>%
  rename(species = Species) %>%
  rename(percent_cover = Percent_cover) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment)

# Selecting plots >1 species planted, >1 species group, and is part of the main experiment
biocon_amb <- biocon_long %>%
  filter(CountOfSpecies > 1) %>%
  filter(CountOfGroup > 1) %>%
  filter(Experiment == "M") %>%
  filter(co2_treatment == "Camb" &
           n_treatment == "Namb") %>%
  filter(water_treatment == "H2Oamb" |
           water_treatment == "") %>%
  filter(temp_treatment == "HTamb" |
           temp_treatment == "") %>%
  filter(!(is.na(percent_cover)))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(biocon_amb,paste(path_out,'biocon_clean.csv'))
