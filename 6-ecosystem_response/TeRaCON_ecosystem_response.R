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
# Filter data to August (harvest) and calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_teracon %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n)) %>%
  pivot_longer(cols = c(mean_ab_bio, mean_bl_bio, mean_total_bio,
                        mean_total_n, mean_bl_c, mean_bl_n,
                        mean_ab_c,mean_ab_n), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = HTelv - HTamb) %>%
  select(year, variable, sensitivity)


# Function to plot changes in function over time (warmed - ambient) for a given ecosystem response var
sens_plot <- function(df, response_var) {
  
  df <- df %>%
    filter(variable == response_var)
  
  ggplot(df, aes(x = year, y = sensitivity)) +
    geom_smooth() +
    theme_bw()
}
sens_plot(eco_grouped,"mean_bl_n")
