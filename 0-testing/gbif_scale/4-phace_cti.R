# TITLE:          PHACE CTI and CPI calculation for different gbif scales
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
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
setwd(path_data)
# Load in data
niche_uscan <- read.csv(" phace_niche_uscan.csv")
niche_1000 <- read.csv(" phace_niche_1000.csv")
niche_500 <- read.csv(" phace_niche_500.csv")
niche_uscan <- niche_uscan %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
niche_1000 <- niche_1000 %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
niche_500 <- niche_500 %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
phace <- read.csv(" phace_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_uscan <- left_join(phace, niche_uscan, by = "species")
full_abun_uscan <- full_abun_uscan %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

full_abun_1000 <- left_join(phace, niche_1000, by = "species")
full_abun_1000 <- full_abun_1000 %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

full_abun_500 <- left_join(phace, niche_500, by = "species")
full_abun_500 <- full_abun_500 %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))


# Calculating CTI
CTI_uscan <- full_abun_uscan %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()

CTI_1000 <- full_abun_1000 %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()

CTI_500 <- full_abun_500 %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3) %>%
  distinct()

# CTI sensitivity (warmed - ambient)
CTI_sens_uscan <- CTI_uscan %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

CTI_sens_1000 <- CTI_1000 %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

CTI_sens_500 <- CTI_500 %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

# Calculating CPI
CPI_uscan <- full_abun_uscan %>%
  group_by(year,plot,water_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4) - 3)

CPI_1000 <- full_abun_1000 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4) - 3)

CPI_500 <- full_abun_500 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4) - 3)

# CTI and CPI combined
CTI_CPI_uscan <- full_abun_uscan %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

CTI_CPI_1000 <- full_abun_1000 %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

CTI_CPI_500 <- full_abun_500 %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
write.csv(CTI_uscan,paste(path_out,'CTI_phace_uscan.csv'), row.names = F)
write.csv(CTI_sens_uscan,paste(path_out,'CTI_sens_phace_uscan.csv'), row.names = F)
write.csv(CTI_CPI_uscan,paste(path_out,'CTI_CPI_phace_uscan.csv'), row.names = F)

write.csv(CTI_1000,paste(path_out,'CTI_phace_1000.csv'), row.names = F)
write.csv(CTI_sens_1000,paste(path_out,'CTI_sens_phace_1000.csv'), row.names = F)
write.csv(CTI_CPI_1000,paste(path_out,'CTI_CPI_phace_1000.csv'), row.names = F)

write.csv(CTI_500,paste(path_out,'CTI_phace_500.csv'), row.names = F)
write.csv(CTI_sens_500,paste(path_out,'CTI_sens_phace_500.csv'), row.names = F)
write.csv(CTI_CPI_500,paste(path_out,'CTI_CPI_phace_500.csv'), row.names = F)
