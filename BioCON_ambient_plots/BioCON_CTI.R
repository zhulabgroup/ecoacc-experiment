# TITLE:          BioCON CTI
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon (same spp as biocon)
# DATA OUTPUT:    CTI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024


# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" niche_estimate_teracon.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
biocon <- read.csv(" biocon_clean.csv")

# Combining biocon abundance data with niche estimate data
full_abun_data <- left_join(biocon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI
CTI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  distinct()

# Plot CTI
ggplot(CTI, aes(x = year, y = CTI)) +
  geom_jitter(alpha = 0.2) +  # Add jittered points
  geom_line(data = biocon[!is.na(biocon$mean_C_temp_summer),], aes(x = year,y = (mean_C_temp_summer/2))) +
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "crossbar",
               width = 0.4,
               color="red") +
  theme_minimal()


# Models
mod1 <- lm(CTI ~ as.factor(year), data = CTI)
anova(mod1)
summary(mod1)
emm <- emmeans(mod1, ~ year)
pairs(emm, adjust = "tukey")
