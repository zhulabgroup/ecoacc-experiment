# TITLE:          Oklahoma CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for Oklahoma
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" ok_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
ok <- read.csv(" ok_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_data <- left_join(ok, niche_est, by = "species")
spp_w_no_data <- full_abun_data %>% # check to see what species are missing data; these were not found in gbif
  filter(is.na(temp_niche)) %>%
  filter(is.na(precip_niche))
full_abun_data <- full_abun_data %>% # removing spp w/o niche information
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))



### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/climate/IEM/Monthly_temps/"
setwd(path_data)
# Load in data
iem <- read.csv("iem_OK_monthly_temps.csv")
iem <- iem %>%
  rename(year = X) %>%
  mutate(MAT = (ANN-32)*5/9) %>%
  dplyr::select(year, MAT)
iem$year <- as.integer(iem$year)

# Merging with PHACE data
full_abun_data <- left_join(full_abun_data, iem, by = "year")

# Coding MAT from warmed plots to be hotter
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed",
  full_abun_data$MAT + 1.1,
  full_abun_data$MAT
)



# Calculating CTI
CTI <- full_abun_data %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3,
          disequilib = CTI - MAT) %>%
  distinct()

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

# Calculating CPI
CPI <- full_abun_data %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4) - 3)

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
write.csv(CTI,paste(path_out,'CTI_ok.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_ok.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_ok.csv'))
