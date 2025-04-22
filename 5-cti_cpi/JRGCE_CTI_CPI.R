# TITLE:          JRGCE CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for jrgce data
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" jrgce_niche.csv")
jrgce <- read.csv(" jrgce_clean.csv")

# Combining jrgce abundance data with niche estimate data
full_abun_data <- left_join(jrgce, niche_est, by = "species")
full_abun_data$site <- "JRGCE"



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
mat <- read.csv(" MAT.csv")
# Merging with data
full_abun_data <- left_join(full_abun_data, mat, by = c("site","year"))



# Coding MAT from warmed plots to be hotter
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed" & full_abun_data$year >= 1999 & full_abun_data$year <= 2002,
  full_abun_data$MAT + 1,
  full_abun_data$MAT
)
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed" & full_abun_data$year >= 2003 & full_abun_data$year <= 2009,
  full_abun_data$MAT + 1.5,
  full_abun_data$MAT
)
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed" & full_abun_data$year >= 2010 & full_abun_data$year <= 2014,
  full_abun_data$MAT + 2,
  full_abun_data$MAT
)



# Calculating CTI
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
CTI_sens <- CTI %>% # Calculating SE of diff bwtn means
  dplyr::select(year, plot, temp_treatment, CTI) %>%
  group_by(year, temp_treatment) %>%
  summarize(mean_cti = mean(CTI), sd_cti = sd(CTI), n = n()) %>%  # Calculate mean, SD, and sample size
  pivot_wider(names_from = temp_treatment, values_from = c(mean_cti, sd_cti, n)) %>%
  mutate(
    sensitivity = `mean_cti_warmed` - `mean_cti_ambient`,  # Sensitivity as the difference in means
    SE_diff = sqrt((`sd_cti_warmed`^2 / `n_warmed`) + (`sd_cti_ambient`^2 / `n_ambient`))  # Standard error of the difference
  )
CTI_sens$sens_scaled <- NA
CTI_sens$SE_diff_scaled <- NA
CTI_sens$sens_scaled <- ifelse(
  CTI_sens$year >= 1999 & CTI_sens$year <= 2002,
  CTI_sens$sensitivity/1,
  CTI_sens$sens_scaled
)
CTI_sens$sens_scaled <- ifelse(
  CTI_sens$year >= 2003 & CTI_sens$year <= 2009,
  CTI_sens$sensitivity/1.5,
  CTI_sens$sens_scaled
)
CTI_sens$sens_scaled <- ifelse(
  CTI_sens$year >= 2010 & CTI_sens$year <= 2014,
  CTI_sens$sensitivity/2,
  CTI_sens$sens_scaled
)
CTI_sens$SE_diff_scaled <- ifelse(
  CTI_sens$year >= 1999 & CTI_sens$year <= 2002,
  CTI_sens$SE_diff/1,
  CTI_sens$SE_diff_scaled
)
CTI_sens$SE_diff_scaled <- ifelse(
  CTI_sens$year >= 2003 & CTI_sens$year <= 2009,
  CTI_sens$SE_diff/1.5,
  CTI_sens$SE_diff_scaled
)
CTI_sens$SE_diff_scaled <- ifelse(
  CTI_sens$year >= 2010 & CTI_sens$year <= 2014,
  CTI_sens$SE_diff/2,
  CTI_sens$SE_diff_scaled
)


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
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
write.csv(CTI,paste(path_out,'CTI_jrgce.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_jrgce.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_jrgce.csv'))
