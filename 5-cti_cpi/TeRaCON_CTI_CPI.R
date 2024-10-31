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
  rename(species = Species) %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(Percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI and CPI
CTI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,Plot,scaled_temp,Temp.Treatment) %>%
  reframe(CTI = sum(Percent_cover * temp_niche) / sum(Percent_cover),
          CTI_var = sum(Percent_cover * temp_niche^2) / sum(Percent_cover) - CTI^2,
          CTI_sd = sqrt(CTI_var))
CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,Plot,Water.Treatment) %>%
  reframe(CPI = sum(Percent_cover * precip_niche) / sum(Percent_cover),
          CPI_var = sum(Percent_cover * precip_niche^2) / sum(Percent_cover) - CPI^2,
          CPI_sd = sqrt(CPI_var))

# Plot CTI
ggplot(CTI, aes(x = year, y = CTI, color = Temp.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = Temp.Treatment, group = Temp.Treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CTI std dev
ggplot(CTI, aes(x = year, y = CTI_sd, color = Temp.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = Temp.Treatment, group = Temp.Treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CPI
ggplot(CPI, aes(x = year, y = CPI, color = Water.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = Water.Treatment, group = Water.Treatment)) +
  theme_minimal() +
  scale_color_manual(values = c("H2Oamb" = "blue", "H2Oneg" = "red"))

# Plot CPI std dev
ggplot(CPI, aes(x = year, y = CPI_sd, color = Water.Treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = Water.Treatment, group = Water.Treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("H2Oamb" = "blue", "H2Oneg" = "red"))
