# TITLE:          JRGCE ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     JRGCE data containing ecosystem responses
# DATA OUTPUT:    Plots of changes in ecosystem responses over time
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)

# Load in data
eco_jrgce <- read.csv(" jrgce_ecosystem_dat_clean.csv")
# Calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_jrgce %>%
  filter(!is.na(temp_treatment)) %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass, na.rm=T)) %>%
  pivot_longer(cols = c(mean_ab_bio), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = warmed - ambient) %>%
  dplyr::select(year, variable, sensitivity)
eco_grouped2 <- eco_jrgce %>%
  filter(!is.na(temp_treatment)) %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass, na.rm=T))



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
write.csv(eco_grouped,paste(path_out,'eco_response_jrgce.csv'))
write.csv(eco_grouped2,paste(path_out,'eco_response_overall_jrgce.csv'))

