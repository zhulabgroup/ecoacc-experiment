# TITLE:          TeRaCON ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Teracon data containing ecosystem responses
# DATA OUTPUT:    
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
path_home = "/home/kcdobson"
setwd(path_data)

# Load in data
eco_teracon <- read.csv(" teracon_ecosystem_dat_clean.csv")
# Filter data to August (harvest) and group
eco_grouped <- eco_teracon %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  summarise(mean_ab_bio = mean(ab_biomass),
            scaled_temp = mean_C_temp_summer*10)

# Plot
ggplot(eco_grouped, aes(x = year, y = mean_ab_bio, color = temp_treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
               
               