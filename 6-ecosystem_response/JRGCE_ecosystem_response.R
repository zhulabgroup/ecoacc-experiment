# TITLE:          JRGCE ecosystem response date 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     JRGCE data containing ecosystem responses
# DATA OUTPUT:    Plots of changes in ecosystem responses over time
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)

# Load in data
eco_jrgce <- read.csv(" jrgce_ecosystem_dat_clean.csv")
# Filter data to August (harvest) and calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped <- eco_jrgce %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "H"), "warmed", "ambient")) %>%
  filter(!is.na(temp_treatment)) %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass)) %>%
  pivot_longer(cols = c(mean_ab_bio), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = warmed - ambient) %>%
  select(year, variable, sensitivity)
eco_grouped2 <- eco_jrgce %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "H"), "warmed", "ambient")) %>%
  filter(!is.na(temp_treatment)) %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass))

# Function to plot changes in function over time (warmed - ambient) for a given ecosystem response var
sens_plot <- function(df, response_var) {
  
  df <- df %>%
    filter(variable == response_var)
  
  ggplot(df, aes(x = year, y = sensitivity)) +
    geom_smooth() +
    labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
    scale_x_continuous(breaks = seq(1998, 2014, by = 2)) +
    theme_bw()
}
sens_plot(eco_grouped,"mean_ab_bio")



# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(eco_grouped,paste(path_out,'eco_response_jrgce.csv'))
write.csv(eco_grouped2,paste(path_out,'eco_response_overall_jrgce.csv'))

