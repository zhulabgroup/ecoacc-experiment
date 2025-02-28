# TITLE:          TeRaCON ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Teracon data containing ecosystem responses
# DATA OUTPUT:    Plots of changes in ecosystem responses over time
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
eco_teracon <- read.csv(" teracon_ecosystem_dat_clean.csv")


# Filter data to August (harvest) and calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_teracon %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n)) %>%
  pivot_longer(cols = c(mean_ab_bio, mean_bl_bio, mean_total_bio,mean_ab_and_root,
                        mean_total_n, mean_bl_c, mean_bl_n,
                        mean_ab_c,mean_ab_n), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = warmed - ambient) %>%
  dplyr::select(year, variable, sensitivity)

eco_grouped_overall <- eco_teracon %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n))



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
write.csv(eco_grouped,paste(path_out,'eco_response_teracon.csv'))
write.csv(eco_grouped_overall,paste(path_out,'eco_response_overall_teracon.csv'))

