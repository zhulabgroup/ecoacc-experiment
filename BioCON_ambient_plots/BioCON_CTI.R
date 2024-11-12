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
# Some plots only have 1 species; removing plots w/ the same CTI value every year
plots_to_remove <- CTI %>% 
  filter(!(CTI == "NaN")) %>%
  group_by(plot) %>%
  filter(n_distinct(CTI) == 1) %>%
  distinct(plot)
CTI_filtered <- CTI %>%
  filter(!plot %in% plots_to_remove$plot)

# Plot CTI
ggplot(CTI_filtered, aes(x = year, y = CTI)) +
  geom_jitter(alpha = 0.1) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "crossbar",
               width = 0.4,
               color="red") +
  theme_minimal()
