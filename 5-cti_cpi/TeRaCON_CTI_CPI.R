# TITLE:          TeRaCON CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

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
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI and CPI
CTI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot,scaled_temp,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * temp_niche^2) / sum(percent_cover) - CTI^2,
          CTI_sd = sqrt(CTI_var))
CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * precip_niche^2) / sum(percent_cover) - CPI^2,
          CPI_sd = sqrt(CPI_var))

# Plot CTI
ggplot(CTI, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  #geom_jitter(alpha = 0.2,
  #            position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  geom_smooth() +
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CTI std dev
ggplot(CTI, aes(x = year, y = CTI_sd, color = temp_treatment, group=temp_treatment)) +
  #geom_jitter(alpha = 0.2,
  #            position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  geom_smooth()
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CPI
ggplot(CPI, aes(x = year, y = CPI, color = water_treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = water_treatment, group = water_treatment)) +
  theme_minimal() +
  scale_color_manual(values = c("H2Oamb" = "blue", "H2Oneg" = "red"))

# Plot CPI std dev
ggplot(CPI, aes(x = year, y = CPI_sd, color = water_treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = median,
               fun.min = median,
               fun.max = median,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = water_treatment, group = water_treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("H2Oamb" = "blue", "H2Oneg" = "red"))



# Models
### note to self: do emmeans next to see all pairwise comparisons
cti_mod <- lmerTest::lmer(CTI_sd ~ temp_treatment*as.factor(year) + (1|plot), data=CTI)
anova(cti_mod)  
contrast.cti <- contrast(emmeans(cti_mod, ~temp_treatment*year), "pairwise", simple = "each", combine = F, adjust = "mvt")
contrast.cti

