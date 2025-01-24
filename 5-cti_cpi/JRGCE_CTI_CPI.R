# TITLE:          JRGCE CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for jrgce data
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" jrgce_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
jrgce <- read.csv(" jrgce_clean.csv")

# Combining jrgce abundance data with niche estimate data
full_abun_data <- left_join(jrgce, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI
CTI <- full_abun_data %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3) %>%
  distinct()

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

# Calculating CPI
CPI <- full_abun_data %>%
  group_by(year,plot,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")

# Models (note: move to new script at some point)
cti_mod <- lmerTest::lmer(CTI ~ temp_treatment*as.factor(year) + (1|plot), data=CTI)
anova(cti_mod)  
emm <- emmeans(cti_mod, ~ temp_treatment * year)
summary(emm)
pairs(emm, by = "year")


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(CTI,paste(path_out,'CTI_jrgce.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_jrgce.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_jrgce.csv'))
