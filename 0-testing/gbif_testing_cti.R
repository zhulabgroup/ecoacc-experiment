# TITLE:          TeRaCON CTI and CPI calculation for different gbif scales
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)
# Load in data
niche_uscan <- read.csv(" teracon_niche_uscan.csv")
niche_1000 <- read.csv(" teracon_niche_1000.csv")
niche_500 <- read.csv(" teracon_niche_500.csv")
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
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_uscan <- left_join(teracon, niche_uscan, by = "species")
full_abun_uscan <- full_abun_uscan %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

full_abun_1000 <- left_join(teracon, niche_1000, by = "species")
full_abun_1000 <- full_abun_1000 %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

full_abun_500 <- left_join(teracon, niche_500, by = "species")
full_abun_500 <- full_abun_500 %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))


# Calculating CTI
CTI_uscan <- full_abun_uscan %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  distinct()

CTI_1000 <- full_abun_1000 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  distinct()

CTI_500 <- full_abun_500 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  distinct()

# CTI sensitivity (warmed - ambient)
CTI_sens_uscan <- CTI_uscan %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

CTI_sens_1000 <- CTI_1000 %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

CTI_sens_500 <- CTI_500 %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

# Calculating CPI
CPI_uscan <- full_abun_uscan %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)

CPI_1000 <- full_abun_1000 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)

CPI_500 <- full_abun_500 %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)

# CTI and CPI combined
CTI_CPI_uscan <- full_abun_uscan %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

CTI_CPI_1000 <- full_abun_1000 %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

CTI_CPI_500 <- full_abun_500 %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(CTI_uscan,paste(path_out,'CTI_teracon_uscan.csv'), row.names = F)
write.csv(CTI_sens_uscan,paste(path_out,'CTI_sens_teracon_uscan.csv'), row.names = F)
write.csv(CTI_CPI_uscan,paste(path_out,'CTI_CPI_teracon_uscan.csv'), row.names = F)

write.csv(CTI_1000,paste(path_out,'CTI_teracon_1000.csv'), row.names = F)
write.csv(CTI_sens_1000,paste(path_out,'CTI_sens_teracon_1000.csv'), row.names = F)
write.csv(CTI_CPI_1000,paste(path_out,'CTI_CPI_teracon_1000.csv'), row.names = F)

write.csv(CTI_500,paste(path_out,'CTI_teracon_500.csv'), row.names = F)
write.csv(CTI_sens_500,paste(path_out,'CTI_sens_teracon_500.csv'), row.names = F)
write.csv(CTI_CPI_500,paste(path_out,'CTI_CPI_teracon_500.csv'), row.names = F)



