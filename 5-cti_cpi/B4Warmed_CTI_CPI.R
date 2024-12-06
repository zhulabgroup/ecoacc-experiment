# TITLE:          B4Warmed CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" b4warmed_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
b4 <- read.csv(" b4warmed_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_data <- left_join(b4, niche_est, by = c("site","species"))
full_abun_data <- full_abun_data %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI
CTI <- full_abun_data %>%
  group_by(site,year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(site,year,plot,temp_treatment,CTI) %>%
  group_by(site,year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity_high_temp = `3.4` - amb) %>%
  mutate(sensitivity_med_temp = `1.7` - amb)

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  group_by(site,year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(CTI,paste(path_out,'CTI_b4warmed.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_b4warmed.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_b4warmed.csv'))
