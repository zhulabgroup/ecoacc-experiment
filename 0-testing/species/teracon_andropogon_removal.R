# TITLE:          TeRaCON CTI and CPI calculation with no Andropogon
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI and CPI calculations with dominant species removal (Andropogon)
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" teracon_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_no_andro <- full_abun_data %>%
  filter(!(species == "Andropogon gerardi"))

# CTI
CTI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  filter(!(CTI == "NaN")) %>%
  distinct()

# CTI Sensitivity
CTI_sens_filt <- CTI_filt %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

# CPI
CPI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3) %>%
  filter(!(CPI == "NaN"))

# CTI and CPI combined
CTI_CPI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  filter(!(CTI == "NaN")) %>%
  filter(!(CPI == "NaN")) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(CTI_filt,paste(path_out,'CTI_teracon_nobluestem.csv'))
write.csv(CTI_sens_filt,paste(path_out,'CTI_sens_teracon_nobluestem.csv'))
write.csv(CTI_CPI_filt,paste(path_out,'CTI_CPI_teracon_nobluestem.csv'))


