# TITLE:          Combined plots for all experiments 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(ggpubr)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)

# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
NPP_jrgce <- read.csv(" eco_response_jrgce.csv")

# CTI figures
CTI_tera_plot <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
CTI_jrgce_plot <- ggplot(CTI_sens_jrgce, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 2)) +
  theme_bw()

# Biomass figures
NPP_teracon <- NPP_teracon %>%
  filter(variable == "mean_ab_bio")
npp_tera_plot <- ggplot(NPP_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
npp_jrgce_plot <- ggplot(NPP_jrgce, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 2)) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_tera_plot,npp_tera_plot,
          CTI_jrgce_plot,npp_jrgce_plot,
          ncol = 2, nrow=2, common.legend = T, legend = "right")


