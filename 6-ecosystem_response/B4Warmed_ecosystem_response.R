# TITLE:          B4Warmed ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     B4Warmed data containing ecosystem responses
# DATA OUTPUT:    summarized ecosystem response data
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)

# Load in data
eco_b4 <- read.csv(" b4warmed_ecosystem_dat_clean.csv")

# Calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_b4 %>%
  group_by(site,year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(total_biomass)) %>%
  pivot_longer(cols = c(mean_ab_bio), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity_high_temp = `3.4` - amb) %>%
  mutate(sensitivity_med_temp = `1.7` - amb) %>%
  dplyr::select(year, variable, sensitivity_high_temp,sensitivity_med_temp)
eco_grouped_overall <- eco_b4 %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(total_biomass))


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(eco_grouped,paste(path_out,'eco_response_b4warmed.csv'))
write.csv(eco_grouped_overall,paste(path_out,'eco_response_overall_b4warmed.csv'))


