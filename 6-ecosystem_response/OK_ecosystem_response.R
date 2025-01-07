# TITLE:          Oklahoma ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     OK data containing ecosystem responses
# DATA OUTPUT:    summarized ecosystem response data
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)

# Load in data
eco_ok <- read.csv(" ok_ecosystem_dat_clean.csv")

# Calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_ok$temp_treatment[eco_ok$temp_treatment == "C"] <- "ambient"
eco_ok$temp_treatment[eco_ok$temp_treatment == "W"] <- "warmed"
eco_grouped <- eco_ok %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass)) %>%
  pivot_longer(cols = c(mean_ab_bio), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = warmed - ambient) %>%
  dplyr::select(year, variable, sensitivity)
eco_grouped_overall <- eco_ok %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/OK/"
write.csv(eco_grouped,paste(path_out,'eco_response_ok.csv'))
write.csv(eco_grouped_overall,paste(path_out,'eco_response_overall_ok.csv'))

