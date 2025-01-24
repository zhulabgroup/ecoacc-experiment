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
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)

# Load in data
niche_est_cfc <- read.csv(" b4warmed_cfc_niche.csv")
niche_est_cfc <- niche_est_cfc %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()

niche_est_hwrc <- read.csv(" b4warmed_hwrc_niche.csv")
niche_est_hwrc <- niche_est_hwrc %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()

b4_cfc <- read.csv(" b4warmed_cfc_clean.csv")
b4_hwrc <- read.csv(" b4warmed_hwrc_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_data_cfc <- left_join(b4_cfc, niche_est_cfc, by = c("species"))
full_abun_data_cfc <- full_abun_data_cfc %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_data_hwrc <- left_join(b4_hwrc, niche_est_hwrc, by = c("species"))
full_abun_data_hwrc <- full_abun_data_hwrc %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI
CTI_cfc <- full_abun_data_cfc %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()
CTI_hwrc <- full_abun_data_hwrc %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()


# Calculating CTI sensitivity (warmed - ambient)
CTI_sens_cfc <- CTI_cfc %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity_high_temp = `3.4` - amb) %>%
  mutate(sensitivity_med_temp = `1.7` - amb)
CTI_sens_hwrc <- CTI_hwrc %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity_high_temp = `3.4` - amb) %>%
  mutate(sensitivity_med_temp = `1.7` - amb)

# CTI and CPI combined
CTI_CPI_cfc <- full_abun_data_cfc %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")
CTI_CPI_hwrc <- full_abun_data_hwrc %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
write.csv(CTI_cfc,paste(path_out,'CTI_b4warmed_cfc.csv'))
write.csv(CTI_sens_cfc,paste(path_out,'CTI_sens_b4warmed_cfc.csv'))
write.csv(CTI_CPI_cfc,paste(path_out,'CTI_CPI_b4warmed_cfc.csv'))
write.csv(CTI_hwrc,paste(path_out,'CTI_b4warmed_hwrc.csv'))
write.csv(CTI_sens_hwrc,paste(path_out,'CTI_sens_b4warmed_hwrc.csv'))
write.csv(CTI_CPI_hwrc,paste(path_out,'CTI_CPI_b4warmed_hwrc.csv'))
