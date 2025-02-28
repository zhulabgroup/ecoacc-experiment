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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" teracon_niche.csv")
teracon <- read.csv(" teracon_clean.csv")

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data$site <- "TeRaCON"



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
mat <- read.csv(" MAT.csv")
# Merging with data
full_abun_data <- left_join(full_abun_data, mat, by = c("site","year"))

# Coding MAT from warmed plots to be hotter
# 2.3 warming level, warming for 7 months of the year
2.3/1.71 # Add 1.35 to MAT (12/7 = 1.71)
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed",
  full_abun_data$MAT + 1.35,
  full_abun_data$MAT
)



### Calculations
# Calculating CTI & disequilibrium
CTI <- full_abun_data %>%
  group_by(year,plot,temp_treatment,MAT) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3,
          disequilib = CTI - MAT) %>%
  distinct()

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = warmed - ambient)

# Calculating CPI
CPI <- full_abun_data %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CPI_var = sum(rel_abun * (precip_niche - CPI)^2) / sum(rel_abun),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(rel_abun * (precip_niche - CPI)^3) / (sum(rel_abun) * CPI_sd^3),
          CPI_kurt = sum(rel_abun * (precip_niche - CPI)^4) / (sum(rel_abun) * CPI_sd^4) - 3)

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(rel_abun * precip_niche) / sum(rel_abun),
          CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
write.csv(CTI,paste(path_out,'CTI_teracon.csv'),row.names=F)
write.csv(CTI_sens,paste(path_out,'CTI_sens_teracon.csv'),row.names=F)
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_teracon.csv'),row.names=F)

