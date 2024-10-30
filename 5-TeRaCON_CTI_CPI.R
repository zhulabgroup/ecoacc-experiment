# TITLE:          TeRaCON CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
path_home = "/home/kcdobson"
setwd(path_data)

# Load in data
niche_est <- read.csv(" niche_estimate_teracon.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  rename(species = Species)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")

# Calculating CTI and CPI
CTI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,Plot,Temp.Treatment) %>%
  summarise(CTI = sum(Percent_cover * temp_niche, na.rm = TRUE) / sum(Percent_cover, na.rm = TRUE))
CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,Plot,Temp.Treatment) %>%
  summarise(CPI = sum(Percent_cover * precip_niche, na.rm = TRUE) / sum(Percent_cover, na.rm = TRUE))

# Plot CTI
ggplot(CTI, aes(x = year, y = CTI, color = Temp.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = Temp.Treatment, group = Temp.Treatment)) +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CPI
ggplot(CPI, aes(x = year, y = CPI, color = Temp.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = Temp.Treatment, group = Temp.Treatment)) +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))