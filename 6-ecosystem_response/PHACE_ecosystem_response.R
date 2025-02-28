# TITLE:          PHACE ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     PHACE data containing ecosystem responses
# DATA OUTPUT:    summarized ecosystem response data
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)

# Load in data
eco_phace <- read.csv(" phace_ecosystem_dat_clean.csv")
# Calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_phace %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(total_biomass)) %>%
  pivot_longer(cols = c(mean_ab_bio), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = warmed - ambient) %>%
  dplyr::select(year, variable, sensitivity)
eco_grouped_overall <- eco_phace %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(total_biomass))


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
write.csv(eco_grouped,paste(path_out,'eco_response_phace.csv'),row.names=F)
write.csv(eco_grouped_overall,paste(path_out,'eco_response_overall_phace.csv'),row.names=F)

