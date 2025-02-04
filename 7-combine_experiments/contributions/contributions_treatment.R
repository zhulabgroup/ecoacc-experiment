# TITLE:          Calculating contribution to CTI (warming effect)
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    Species individual contributions to CTI
# PROJECT:        EcoAcc
# DATE:           Feb 2025



### Load packages
library(tidyverse)
library(stringr)

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
niche_est_phace <- niche_est_phace %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
phace <- read.csv(" phace_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
niche_est_tera <- read.csv(" teracon_niche.csv")
niche_est_tera <- niche_est_tera %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
tera <- read.csv(" teracon_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)
# Load in data
niche_est_b4 <- read.csv(" b4warmed_niche.csv")
niche_est_b4 <- niche_est_b4 %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
b4_cfc <- read.csv(" b4warmed_cfc_clean.csv")
b4_hwrc <- read.csv(" b4warmed_hwrc_clean.csv")
# Separate dataframes for CFC and HWRC
niche_est_cfc<- niche_est_b4 %>%
  filter(site == "CFC")
niche_est_hwrc <- niche_est_b4 %>%
  filter(site == "HWRC")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)
# Load in data
niche_est_ok <- read.csv(" ok_niche.csv")
niche_est_ok <- niche_est_ok %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
ok <- read.csv(" ok_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)
# Load in data
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
niche_est_jrgce <- niche_est_jrgce %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
jrgce <- read.csv(" jrgce_clean.csv")



### Making sure all years are present for all species
# Note: add temp_treatment info in here
phace <- phace %>%
  complete(species, year = 2007:2013,
           plot = c(22, 25, 12, 8, 14, 20, 30, 13, 1, 4, 26, 27, 11, 7, 3, 19, 21, 18, 9, 2),
           fill = list(rel_abun = 0))
tera <- tera %>%
  complete(species, year = 2012:2023,
           plot = c(7, 11, 17, 21, 26, 34, 48, 50, 66, 84, 87, 92, 97, 106, 117, 119, 128, 131,
                    133, 149, 165, 166, 171, 177, 185, 186, 224, 226, 230, 232, 233, 243, 245,
                    264, 274, 278, 280, 282, 293, 299, 308, 320, 324, 341, 356, 357, 360, 361),
           fill = list(rel_abun = 0))
b4_cfc <- b4_cfc %>%
  complete(species, year = 2008:2021,
           plot = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3","F4",
                    "F5", "F6", "F7", "F8"),
           fill = list(rel_abun = 0))
b4_hwrc <- b4_hwrc %>%
  complete(species, year = 2008:2021,
           plot = c("J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "L1", "L2", "L3", "L4",
                    "L5", "L6", "L7", "L8"),
           fill = list(rel_abun = 0))
ok <- ok %>%
  complete(species, year = 2000:2013,
           plot = c("UC1", "UC2", "UC3", "UC4", "UC5", "UC6", "UW1", "UW2", "UW3", "UW4", "UW5", "UW6", "CC1", "CC2", "CC3", "CC4", "CC5",
                    "CC6", "CW1", "CW2", "CW3", "CW4", "CW5", "CW6"),
           fill = list(rel_abun = 0))
jrgce <- jrgce %>%
  complete(species, year = 1999:2014,
           plot = c(1, 10, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 11, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 12, 120,
                    121, 122, 123, 124, 125, 126, 127, 128, 129, 13, 131, 134, 136, 137, 139, 14, 142, 144, 15, 16, 17, 18, 19, 2, 20, 21,
                    22, 23, 24, 25, 26, 27, 28, 29, 3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 5,
                    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 7, 70, 71, 72, 73, 74, 75, 76, 77, 78,
                    79, 8, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 9, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99),
           fill = list(rel_abun = 0))

# Merging niche lists into one
niche_est_phace$site <- "PHACE"
niche_est_tera$site <- "TeRaCON"
niche_est_cfc$site <- "B4Warmed CFC"
niche_est_hwrc$site <- "B4Warmed HWRC"
niche_est_ok$site <- "Oklahoma"
niche_est_jrgce$site <- "JRGCE"
dat_niche <- rbind(niche_est_phace, niche_est_tera, niche_est_cfc, niche_est_hwrc, niche_est_ok, niche_est_jrgce)

# Combining phace abundance data with niche estimate data
phace$site <- "PHACE"
tera$site <- "TeRaCON"
b4_cfc$site <- "B4Warmed CFC"
b4_hwrc$site <- "B4Warmed HWRC"
ok$site <- "Oklahoma"
jrgce$site <- "JRGCE"
full_abun_data_phace <- left_join(phace, dat_niche, by = c("site","species"))
full_abun_data_tera <- left_join(tera, dat_niche, by = c("site","species"))
full_abun_data_cfc <- left_join(b4_cfc, dat_niche, by = c("site","species"))
full_abun_data_hwrc <- left_join(b4_hwrc, dat_niche, by = c("site","species"))
full_abun_data_ok <- left_join(ok, dat_niche, by = c("site","species"))
full_abun_data_jrgce <- left_join(jrgce, dat_niche, by = c("site","species"))

# Specifying warmed vs ambient treatment for b4warmed
full_abun_data_cfc <- full_abun_data_cfc %>%
  filter(!(temp_treatment == 1.7)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))
full_abun_data_hwrc <- full_abun_data_hwrc %>%
  filter(!(temp_treatment == 1.7)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))

# Put data into a list
dataframes_list <- list(full_abun_data_phace, full_abun_data_tera, full_abun_data_cfc, full_abun_data_hwrc, full_abun_data_ok, full_abun_data_jrgce)
names(dataframes_list) <- c("PHACE","TeRaCON","B4Warmed CFC","B4Warmed HWRC","Oklahoma","JRGCE")



### Calculating relative abundance
process_niche_data <- function(df) {
  df %>%
    filter(!is.na(temp_niche)) %>%
    group_by(year, plot) %>%
    mutate(total_cover = sum(rel_abun,na.rm=T)) %>%
    mutate(rel_abund = rel_abun / total_cover) %>%
    filter(!is.na(rel_abund) & rel_abund != "NaN") %>%
    ungroup()
}
rel_abun_list <- lapply(dataframes_list, process_niche_data)



### Calculating initial CTI per site to use as our measure of center for the niche data
calculate_initial_CTI <- function(data) {
  # Calculate the baseline CTI for the initial year
  initial_year <- min(data$year)
  
  # Calculate CTI baseline
  initial_cti <- data %>%
    filter(!is.na(temp_niche)) %>%
    filter(year == initial_year) %>%
    group_by(plot) %>%
    summarise(
      cti_baseline = sum(rel_abund * temp_niche) / sum(rel_abund)
    )
  
  return(initial_cti)
}
cti_initial <- lapply(rel_abun_list, calculate_initial_CTI)

## Merging abundance data with initial CTI
merged_list <- Map(function(df1, df2) {
  left_join(df1, df2, by = "plot")
},
rel_abun_list,
cti_initial)

## Centering niche data with initial cti per site, year, and plot
centered_list <- lapply(merged_list, function(df) {
  df %>%
    mutate(temp_niche_center = temp_niche - cti_baseline)
})



### Calculating CTI
# Using raw temp niche values
calculate_CTI <- function(data) {
  data %>%
    group_by(year,temp_treatment) %>%
    reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
    distinct() %>%
    group_by(year,temp_treatment) %>%
    summarize(mean_cti = mean(CTI)) %>%
    pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
    mutate(CTI_diff = warmed - ambient)
}
cti_results <- lapply(centered_list, calculate_CTI)

# Using mean-centered niche values
calculate_CTI_center <- function(data) {
  data %>%
    group_by(year,temp_treatment) %>%
    reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
    distinct() %>%
    group_by(year,temp_treatment) %>%
    summarize(mean_cti = mean(CTI)) %>%
    pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
    mutate(CTI_diff = warmed - ambient)
}
cti_results_center <- lapply(centered_list, calculate_CTI_center)



### Matching abundance data w/ CTI data
abun_calc <- function(data) {
  data %>%
    group_by(year, species, temp_treatment) %>%
    summarize(rel_abun_avg = mean(rel_abun)) %>%
    pivot_wider(names_from = temp_treatment, values_from = rel_abun_avg) %>%
    mutate(rel_abun_diff = warmed - ambient)
}
abun_list <- lapply(centered_list, abun_calc)

# Pull out niche data per species
niche_list <- lapply(centered_list, function(df) {
  df %>%
    select(species,temp_niche,temp_niche_center) %>%
    group_by(species) %>%
    mutate(temp_niche_center = mean(temp_niche_center)) %>%
    distinct()
})

# Joining that with abun data
final_list <- map2(abun_list, niche_list, ~ left_join(.x, .y, by = "species"))



### Compute linear slopes using covariance and variance
## CTI
# Slope values are the same whether using raw or centered temp niche values
calculate_CTI_slope <- function(data) {
  cov(data$year, data$CTI_diff) / var(data$year)
}
CTI_slope_list <- lapply(cti_results, calculate_CTI_slope)

## Species
calculate_slopes <- function(abun_data) {
  slopes <- list()
  
  # Unique species names
  unique_species <- unique(abun_data$species)
  
  # Calculate the slope for each species
  for (species in unique_species) {
    # Subset the data for the current species
    species_data <- abun_data[abun_data$species == species, ]
    
    # Extract time and corresponding values
    time <- species_data$year
    value <- species_data$rel_abun_diff
    
    # Calculate the slope
    slope <- cov(time, value) / var(time)
    
    # Store the slope in the list
    slopes[[species]] <- slope
  }
  
  return(slopes)
}
all_slopes <- lapply(final_list, calculate_slopes)



### Calculate the variance of the slopes using residuals
## CTI
calculate_CTI_variance <- function(data) {
  cti_variances <- list()
  
  # Calculate variance
  resid_CTI <- residuals(lm(data$CTI_diff ~ data$year))
  var <- sum(resid_CTI^2) / (length(data$CTI_diff) - 2) / sum((data$year - mean(data$year))^2)
  
  # Store variance in list
  cti_variances <- var
  return(cti_variances)
}
CTI_variance_list <- lapply(cti_results, calculate_CTI_variance)

## Species
calculate_variances <- function(abun_data) {
  variances <- list()
  
  # Unique species names
  unique_species <- unique(abun_data$species)
  
  # Calculate the variance for each species
  for (species in unique_species) {
    # Subset the data for the current species
    species_data <- abun_data[abun_data$species == species, ]
    
    # Extract time and corresponding values
    time <- species_data$year
    value <- species_data$rel_abun_diff
    
    # Calculate the variance
    resid <- residuals(lm(value ~ time))
    var <- sum(resid^2) / (length(value) - 2) / sum((time - mean(time))^2)
    
    # Store the variance in the list
    variances[[species]] <- var
    
  }
  
  return(variances)
}
all_variances <- lapply(final_list, calculate_variances)



### Slope contribution calculations
## Using raw temp niche values
# Initialize a list to store the contributions for each dataframe
all_contributions <- vector("list", length(all_slopes))

# Initialize a list to store the sum of contributions for each dataframe
slope_sums <- numeric(length(all_slopes))

# Iterate over each element in the all_slopes list
for (i in seq_along(all_slopes)) {
  
  # Extract slopes for the current dataframe
  slopes <- all_slopes[[i]]
  
  # Determine the corresponding dataframe
  abun <- final_list[[i]]
  
  # Initialize a vector to store contributions for this dataframe
  contributions <- numeric(length(slopes))
  
  # Set the names of contributions to match the species names
  names(contributions) <- names(slopes)
  
  # Calculate the contribution for each species
  for (species in names(slopes)) {
    
    niche_value <- abun$temp_niche[abun$species == species]
    
    contributions[species] <- slopes[[species]] * niche_value
  }
  
  # Store the named vector of contributions in all_contributions
  all_contributions[[i]] <- contributions
  
  # Sum of contributions for the current dataframe
  slope_sum <- sum(contributions, na.rm = TRUE)
  
  # Store the result in slope_sums
  slope_sums[i] <- slope_sum
}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_contributions) <- names(final_list)
names(slope_sums) <- names(final_list)


## Using mean-centered values
# Initialize a list to store the contributions for each dataframe
all_contributions_center <- vector("list", length(all_slopes))

# Initialize a list to store the sum of contributions for each dataframe
slope_sums_center <- numeric(length(all_slopes))

# Iterate over each element in the all_slopes list
for (i in seq_along(all_slopes)) {
  
  # Extract slopes for the current dataframe
  slopes <- all_slopes[[i]]
  
  # Determine the corresponding dataframe
  abun <- final_list[[i]]
  
  # Initialize a vector to store contributions for this dataframe
  contributions_center <- numeric(length(slopes))
  
  # Set the names of contributions to match the species names
  names(contributions_center) <- names(slopes)
  
  # Calculate the contribution for each species
  for (species in names(slopes)) {
    
    niche_value_center <- abun$temp_niche_center[abun$species == species]
    
    contributions_center[species] <- slopes[[species]] * niche_value_center
  }
  
  # Store the named vector of contributions in all_contributions
  all_contributions_center[[i]] <- contributions_center
  
  # Sum of contributions for the current dataframe
  slope_sum_center <- sum(contributions_center, na.rm = TRUE)
  
  # Store the result in slope_sums
  slope_sums_center[i] <- slope_sum_center
}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_contributions) <- names(final_list)
names(slope_sums) <- names(final_list)



### Variance contribution calculations
## Using raw temp niche values
# Initialize a list to store the contributions for each dataframe
all_var_contributions <- vector("list", length(all_variances))

# Initialize a list to store the sum of contributions for each dataframe
var_sums <- numeric(length(all_variances))

# Iterate over each element in the all_variances list
for (i in seq_along(all_variances)) {
  
  # Extract variances for the current dataframe
  vars <- all_variances[[i]]
  
  # Determine the corresponding dataframe
  abun <- final_list[[i]]
  
  # Initialize a vector to store contributions for this dataframe
  var_contributions <- numeric(length(vars))
  
  # Set the names of contributions to match the species names
  names(var_contributions) <- names(vars)
  
  # Calculate the contribution for each species
  for (species in names(vars)) {
    
    # Pulling out temp niche
    niche_value <- abun$temp_niche[abun$species == species]
    
    # Calculating variance contribution
    var_contributions[species] <- vars[[species]] * niche_value^2
  }
  
  # Store the named vector of contributions in all_contributions
  all_var_contributions[[i]] <- var_contributions
  
  # Sum of contributions for the current dataframe
  var_sum <- sum(var_contributions, na.rm = TRUE)
  
  # Store the result in slope_sums
  var_sums[i] <- var_sum
}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_var_contributions) <- names(final_list)
names(var_sums) <- names(final_list)


## Using mean-centered niche values
# Initialize a list to store the contributions for each dataframe
all_var_contributions_center <- vector("list", length(all_variances))

# Initialize a list to store the sum of contributions for each dataframe
var_sums_center <- numeric(length(all_variances))

# Iterate over each element in the all_variances list
for (i in seq_along(all_variances)) {
  
  # Extract variances for the current dataframe
  vars_center <- all_variances[[i]]
  
  # Determine the corresponding dataframe
  abun_center <- final_list[[i]]
  
  # Initialize a vector to store contributions for this dataframe
  var_contributions_center <- numeric(length(vars_center))
  
  # Set the names of contributions to match the species names
  names(var_contributions_center) <- names(vars_center)
  
  # Calculate the contribution for each species
  for (species in names(vars_center)) {
    
    # Pulling out temp niche
    niche_value_center <- abun_center$temp_niche_center[abun_center$species == species]
    
    # Calculating variance contribution
    var_contributions_center[species] <- vars_center[[species]] * niche_value_center^2
  }
  
  # Store the named vector of contributions in all_contributions
  all_var_contributions_center[[i]] <- var_contributions_center
  
  # Sum of contributions for the current dataframe
  var_sum_center <- sum(var_contributions_center, na.rm = TRUE)
  
  # Store the result in slope_sums
  var_sums_center[i] <- var_sum_center
}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_var_contributions_center) <- names(final_list)
names(var_sums_center) <- names(final_list)




### Print comparisons to ensure sum = calculated CTI
# Initialize a list to store output strings
output_strings <- list()

# Add the slope of the CTI using raw temp niche data
output_strings[[1]] <- str_c("Slope of CTI: ", as.character(CTI_slope_list))

# Add the sum of contributions
output_strings <- c(output_strings, str_c("Sum of contributions: ", as.character(slope_sums)))

# Add the sum of contributions for mean-centered calculations
output_strings <- c(output_strings, str_c("Sum of contributions (centered): ", as.character(slope_sums_center)))

# Print all output strings
writeLines(unlist(output_strings))



### Print variance comparisons to ensure sum = calculated CTI
# Initialize a list to store output strings
output_strings_var <- list()

# Add the variance of slope of CTI
output_strings_var[[1]] <- str_c("Variance of slope of CTI: ", as.character(CTI_variance_list))

# Add the sum of contributions
output_strings_var <- c(output_strings_var, str_c("Sum of variance contributions: ", as.character(var_sums)))

# Add the sum of contributions using mean-centered data
output_strings_var <- c(output_strings_var, str_c("Sum of variance contributions (centered): ", as.character(var_sums_center)))

# Print all output strings
writeLines(unlist(output_strings_var))


