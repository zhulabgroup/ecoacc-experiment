# TITLE:          Oklahoma CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for Oklahoma
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Jan 2025

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" ok_niche.csv")
ok <- read.csv(" ok_clean.csv")
mat_sensors <- read.csv(" ok_MAT_sensors.csv")



### Comparing species names
species_list_abun <- unique(ok$species)
species_list_niche <- unique(niche_est$species)
diff <- setdiff(species_list_abun, species_list_niche)

### Combining phace abundance data with niche estimate data
full_abun_data <- left_join(ok, niche_est, by = "species")
full_abun_data <- full_abun_data %>% # removing spp w/o niche information
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_data$site <- "Oklahoma"

### Merging with temp sensor data
# Temperature sensors were only present in U (unclipped) plots, so here I'm duplicating the data for C (clipped) plots
# Create a new dataframe with "C" plots
c_plots <- mat_sensors %>%
  # Duplicate only the rows that need "C" counterparts
  filter(grepl("^U", plot)) %>%
  # Replace the leading "U" with "C" in the plot column
  mutate(plot = sub("^U", "C", plot))
# Combine the original dataframe with the new "C" plots dataframe
combined_mat_sensors <- bind_rows(mat_sensors, c_plots)

# Merge with abundance data
full_abun_data <- left_join(full_abun_data, combined_mat_sensors, by = c("year","plot"))





# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
mat <- read.csv(" MAT.csv")
# Merging with data
full_abun_data <- left_join(full_abun_data, mat, by = c("site","year"))

# Coding MAT from warmed plots to be hotter
full_abun_data$MAT <- ifelse(
  full_abun_data$temp_treatment == "warmed",
  full_abun_data$MAT + 1.1,
  full_abun_data$MAT
)

# Fixing column names
full_abun_data <- full_abun_data %>%
  rename(MAT_sensors = mat)



# Calculating CTI
CTI <- full_abun_data %>%
  group_by(year,plot,temp_treatment,MAT,MAT_sensors) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3,
          disequilib = CTI - MAT,
          disequilib_sensors = CTI - MAT_sensors) %>%
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
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
write.csv(CTI,paste(path_out,'CTI_ok.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_ok.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_ok.csv'))
