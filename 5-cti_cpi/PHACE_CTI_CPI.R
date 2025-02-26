# TITLE:          PHACE CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
niche_est <- read.csv(" phace_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
phace <- read.csv(" phace_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_data <- left_join(phace, niche_est, by = "species")
full_abun_data$site <- "PHACE"



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
mat <- read.csv(" MAT.csv")
# Merging with data
full_abun_data <- left_join(full_abun_data, mat, by = c("site","year"))

# Coding MAT from warmed plots to be 2.25 hotter in the dataframe (PHACE warmed year-round; 1.5 during day and 3 during night)
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed",
  full_abun_data$MAT + 2.25,
  full_abun_data$MAT
)



### Calculations
# Calculating disequilibrium
dis <- full_abun_data %>%
  filter(year == min(year)) %>%
  group_by(year,plot,temp_treatment,MAT) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          disequilib = CTI - MAT) %>%
  distinct()

# Calculating CTI
CTI <- full_abun_data %>%
  group_by(year,plot,temp_treatment,MAT) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4),
          disequilib = CTI - MAT,
          historical_disequilib = CTI - 7.45) %>%
  distinct()

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
write.csv(CTI,paste(path_out,'CTI_phace.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_phace.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_phace.csv'))
