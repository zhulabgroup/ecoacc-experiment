# TITLE:          Yu CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           April 2025

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)
# Load in data
niche_est <- read.csv(" knz_niche.csv")
data <- read.csv(" knz_clean.csv")

# Combining phace abundance data with niche estimate data
full_abun_data <- left_join(data, niche_est, by = "species")
full_abun_data$site <- "KNZ"



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
map <- read.csv(" MAP.csv")
# Merging with data
full_abun_data <- left_join(full_abun_data, map, by = c("site","year"))



#### Coding MAP in drought to equal the amount shown in Yu 2025 Nature
# KNZ
full_abun_data$MAP <- ifelse(
  full_abun_data$treatment == "Drought" & full_abun_data$year == 2014,
  383,
  full_abun_data$MAP
)
full_abun_data$MAP <- ifelse(
  full_abun_data$treatment == "Drought" & full_abun_data$year == 2015,
  572,
  full_abun_data$MAP
)
full_abun_data$MAP <- ifelse(
  full_abun_data$treatment == "Drought" & full_abun_data$year == 2016,
  534,
  full_abun_data$MAP
)
full_abun_data$MAP <- ifelse(
  full_abun_data$treatment == "Drought" & full_abun_data$year == 2017,
  374,
  full_abun_data$MAP
)



### Calculations
# Calculating initial disequilibrium
dis <- full_abun_data %>%
  filter(year == min(year)) %>%
  group_by(year,block,subplot,treatment,MAP) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          disequilib = CPI - MAP) %>%
  distinct()

# Calculating CPI
CPI <- full_abun_data %>%
  group_by(year,block,subplot,treatment,MAP) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4),
          disequilib = CPI - MAP) %>%
  distinct()

# Calculating CPI sensitivity (warmed - ambient)
CPI_sens <- CPI %>% # Calculating SE of diff bwtn means
  dplyr::select(year, block,subplot, treatment, CPI) %>%
  group_by(year, treatment) %>%
  summarize(mean_CPI = mean(CPI), sd_CPI = sd(CPI), n = n()) %>%  # Calculate mean, SD, and sample size
  pivot_wider(names_from = treatment, values_from = c(mean_CPI, sd_CPI, n)) %>%
  mutate(
    sensitivity = `mean_CPI_Drought` - `mean_CPI_Control`,  # Sensitivity as the difference in means
    SE_diff = sqrt((`sd_CPI_Drought`^2 / `n_Drought`) + (`sd_CPI_Control`^2 / `n_Control`)),# Standard error of the difference
  )



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
write.csv(CPI,paste(path_out,'CPI_knz.csv'),row.names=F)
write.csv(CPI_sens,paste(path_out,'CPI_sens_knz.csv'),row.names=F)
