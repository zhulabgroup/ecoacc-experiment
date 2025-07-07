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
library(ggpubr)
library(ggtree)
library(ggrepel)

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
phace <- read.csv(" phace_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
niche_est_tera <- read.csv(" teracon_niche.csv")
tera <- read.csv(" teracon_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
niche_est_cfc <- read.csv(" b4warmed_cfc_niche.csv")
niche_est_hwrc <- read.csv(" b4warmed_hwrc_niche.csv")
b4_cfc <- read.csv(" b4warmed_cfc_clean.csv")
b4_hwrc <- read.csv(" b4warmed_hwrc_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
niche_est_ok <- read.csv(" ok_niche.csv")
ok <- read.csv(" ok_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
jrgce <- read.csv(" jrgce_clean.csv")


### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/phylogenies/"
setwd(path_data)
# Load in data
phylo_phace <- readRDS(" phace_phylo_tree.rds")
phylo_jrgce <- readRDS(" jrgce_phylo_tree.rds")
phylo_tera <- readRDS(" tera_phylo_tree.rds")
phylo_ok <- readRDS(" ok_phylo_tree.rds")
phylo_b4 <- readRDS(" b4_phylo_tree.rds")
all_phylo <- readRDS(" all_species_phylo.rds")



### Selecting each plot's treatment designation for each experiment
phace_treat <- phace %>%
  dplyr::select(plot, temp_treatment) %>%
  distinct()
tera_treat <- tera %>%
  dplyr::select(plot, temp_treatment) %>%
  distinct()
b4_cfc_treat <- b4_cfc %>%
  dplyr::select(plot, temp_treatment) %>%
  distinct()
b4_hwrc_treat <- b4_hwrc %>%
  dplyr::select(plot, temp_treatment) %>%
  distinct()
ok_treat <- ok %>%
  dplyr::select(plot, temp_treatment) %>%
  distinct()
jrgce_treat <- jrgce %>%
  dplyr::select(year, plot, temp_treatment) %>% # some plots changed treatments over time; need to include year here
  distinct()


### Making sure all years are present for all species
complete_phace <- phace %>%
  complete(species, year = 2007:2013,
           plot = c(22, 25, 12, 8, 14, 20, 30, 13, 1, 4, 26, 27, 11, 7, 3, 19, 21, 18, 9, 2),
           fill = list(rel_abun = 0))
complete_tera <- tera %>%
  complete(species, year = 2012:2023,
           plot = c(7, 11, 17, 21, 26, 34, 48, 50, 66, 84, 87, 92, 97, 106, 117, 119, 128, 131,
                    133, 149, 165, 166, 171, 177, 185, 186, 224, 226, 230, 232, 233, 243, 245,
                    264, 274, 278, 280, 282, 293, 299, 308, 320, 324, 341, 356, 357, 360, 361),
           fill = list(rel_abun = 0))
complete_b4_cfc <- b4_cfc %>%
  complete(species, year = 2008:2021,
           plot = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3","F4",
                    "F5", "F6", "F7", "F8"),
           fill = list(rel_abun = 0))
complete_b4_hwrc <- b4_hwrc %>%
  complete(species, year = 2008:2021,
           plot = c("J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "L1", "L2", "L3", "L4",
                    "L5", "L6", "L7", "L8"),
           fill = list(rel_abun = 0))
complete_ok <- ok %>%
  complete(species, year = 2000:2013,
           plot = c("UC1", "UC2", "UC3", "UC4", "UC5", "UC6", "UW1", "UW2", "UW3", "UW4", "UW5", "UW6", "CC1", "CC2", "CC3", "CC4", "CC5",
                    "CC6", "CW1", "CW2", "CW3", "CW4", "CW5", "CW6"),
           fill = list(rel_abun = 0))
complete_jrgce <- jrgce %>%
  complete(species, year = 1999:2014,
           plot = c(1, 10, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 11, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 12, 120,
                    121, 122, 123, 124, 125, 126, 127, 128, 129, 13, 131, 134, 136, 137, 139, 14, 142, 144, 15, 16, 17, 18, 19, 2, 20, 21,
                    22, 23, 24, 25, 26, 27, 28, 29, 3, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 4, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 5,
                    50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 6, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 7, 70, 71, 72, 73, 74, 75, 76, 77, 78,
                    79, 8, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 9, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99),
           fill = list(rel_abun = 0))



### Merging with treatment info
phace <- complete_phace %>%
  left_join(phace_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
tera <- complete_tera %>%
  left_join(tera_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
b4_cfc <- complete_b4_cfc %>%
  left_join(b4_cfc_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
b4_hwrc <- complete_b4_hwrc %>%
  left_join(b4_hwrc_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
ok <- complete_ok %>%
  left_join(ok_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
jrgce <- complete_jrgce %>%
  left_join(jrgce_treat, by = c("year","plot")) %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)



### Merging niche lists into one
niche_est_phace$site <- "PHACE"
niche_est_tera$site <- "TeRaCON"
niche_est_cfc$site <- "B4Warmed CFC"
niche_est_hwrc$site <- "B4Warmed HWRC"
niche_est_ok$site <- "Oklahoma"
niche_est_jrgce$site <- "JRGCE"
dat_niche <- rbind(niche_est_phace, niche_est_tera, niche_est_cfc, niche_est_hwrc, niche_est_ok, niche_est_jrgce)

# Combining abundance data with niche estimate data
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
  filter(!(year == 2021)) %>%
  filter(!(temp_treatment == 1.7)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))
full_abun_data_hwrc <- full_abun_data_hwrc %>%
  filter(!(year == 2021)) %>%
  filter(!(temp_treatment == 1.7)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))

# Put data into a list
dataframes_list <- list(full_abun_data_phace, full_abun_data_tera, full_abun_data_cfc, full_abun_data_hwrc, full_abun_data_ok, full_abun_data_jrgce)
names(dataframes_list) <- c("PHACE","TeRaCON","B4Warmed CFC","B4Warmed HWRC","Oklahoma","JRGCE")



### Calculating relative abundance
# Re-accounting for completed data
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
    summarise(
      cti_baseline = sum(rel_abund * temp_niche) / sum(rel_abund)
    )
  
  return(initial_cti)
}
cti_initial <- lapply(rel_abun_list, calculate_initial_CTI)

## Merging abundance data with initial CTI
merged_list <- Map(function(df1, df2) {
  merge(df1, df2)
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
    group_by(site, year, species, temp_niche_center, temp_niche,temp_treatment) %>%
    summarize(rel_abun_avg = mean(rel_abun)) %>%
    pivot_wider(names_from = temp_treatment, values_from = rel_abun_avg) %>%
    mutate(rel_abun_diff = warmed - ambient)
}
final_list <- lapply(centered_list, abun_calc)



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
# Additional function to calculate the pairwise contributions
calculate_pairwise_contributions <- function(abun_data, species1, species2, beta1, beta2, var_time) {
  # Subset data for both species
  data1 <- abun_data[abun_data$species == species1, ]
  data2 <- abun_data[abun_data$species == species2, ]
  
  # Calculate covariance of their relative abundance differences
  shared_years <- intersect(data1$year, data2$year)
  abun_diff1 <- data1$rel_abun_diff[data1$year %in% shared_years]
  abun_diff2 <- data2$rel_abun_diff[data2$year %in% shared_years]
  
  cov_abun <- cov(abun_diff1, abun_diff2, use = "complete.obs")
  
  # Get the centered niche values
  T1 <- unique(data1$temp_niche)
  T2 <- unique(data2$temp_niche)
  
  # Calculate the pairwise contribution to variance
  pairwise_contribution <- (T1 * T2 * (cov_abun / var_time - beta1 * beta2)) / (length(shared_years) - 2)
  
  return(pairwise_contribution)
}

# Calculating species contributions
calculate_species_contributions <- function(all_variances, final_list) {
  
  all_var_contributions <- vector("list", length(all_variances))
  var_sums <- numeric(length(all_variances))
  
  for (i in seq_along(all_variances)) {
    vars <- all_variances[[i]]
    abun <- final_list[[i]]
    var_time <- var(abun$year)  # Calculate variance of time
    var_contributions<- numeric(length(vars))
    names(var_contributions) <- names(vars)
    
    # Calculate species contribution
    for (species in names(vars)) {
      niche_value <- abun$temp_niche[abun$species == species]
      beta <- coef(lm(rel_abun_diff ~ year, data=abun[abun$species == species, ]))[2]
      var_contributions[species] <- vars[[species]] * niche_value^2
    }
    
    # Loop to add pairwise contributions
    species_names <- names(vars)
    for (j in 1:(length(species_names) - 1)) {
      for (k in (j + 1):length(species_names)) {
        species1 <- species_names[j]
        species2 <- species_names[k]
        beta1 <- coef(lm(rel_abun_diff ~ year, data=abun[abun$species == species1, ]))[2]
        beta2 <- coef(lm(rel_abun_diff ~ year, data=abun[abun$species == species2, ]))[2]
        pairwise_contribution <- calculate_pairwise_contributions(abun, species1, species2, beta1, beta2, var_time)
        var_contributions[species1] <- var_contributions[species1] + pairwise_contribution
        var_contributions[species2] <- var_contributions[species2] + pairwise_contribution
      }
    }
    
    all_var_contributions[[i]] <- var_contributions
    var_sums[i] <- sum(var_contributions, na.rm = TRUE)
  }
  
  names(all_var_contributions) <- names(final_list)
  names(var_sums) <- names(final_list)
  
  return(list(all_var_contributions = all_var_contributions, var_sums = var_sums))
}

# Execute the function
result <- calculate_species_contributions(all_variances, final_list)
all_var_contributions <- result$all_var_contributions
var_sums <- result$var_sums


## Using mean-centered niche values
# Additional function to calculate the pairwise contributions
calculate_pairwise_contributions_center <- function(abun_data, species1, species2, beta1, beta2, var_time) {
  # Subset data for both species
  data1 <- abun_data[abun_data$species == species1, ]
  data2 <- abun_data[abun_data$species == species2, ]
  
  # Calculate covariance of their relative abundance differences
  shared_years <- intersect(data1$year, data2$year)
  abun_diff1 <- data1$rel_abun_diff[data1$year %in% shared_years]
  abun_diff2 <- data2$rel_abun_diff[data2$year %in% shared_years]
  
  cov_abun <- cov(abun_diff1, abun_diff2, use = "complete.obs")
  
  # Get the centered niche values
  T1 <- unique(data1$temp_niche_center)
  T2 <- unique(data2$temp_niche_center)
  
  # Calculate the pairwise contribution to variance
  pairwise_contribution <- (T1 * T2 * (cov_abun / var_time - beta1 * beta2)) / (length(shared_years) - 2)
  
  return(pairwise_contribution)
}

# Calculating species contributions
calculate_species_contributions_center <- function(all_variances, final_list) {
  
  all_var_contributions_center <- vector("list", length(all_variances))
  var_sums_center <- numeric(length(all_variances))
  
  for (i in seq_along(all_variances)) {
    vars_center <- all_variances[[i]]
    abun_center <- final_list[[i]]
    var_time <- var(abun_center$year)  # Calculate variance of time
    var_contributions_center <- numeric(length(vars_center))
    names(var_contributions_center) <- names(vars_center)
    
    # Calculate species contribution
    for (species in names(vars_center)) {
      niche_value_center <- abun_center$temp_niche_center[abun_center$species == species]
      beta <- coef(lm(rel_abun_diff ~ year, data=abun_center[abun_center$species == species, ]))[2]
      var_contributions_center[species] <- vars_center[[species]] * niche_value_center^2
    }
    
    # Loop to add pairwise contributions
    species_names <- names(vars_center)
    for (j in 1:(length(species_names) - 1)) {
      for (k in (j + 1):length(species_names)) {
        species1 <- species_names[j]
        species2 <- species_names[k]
        beta1 <- coef(lm(rel_abun_diff ~ year, data=abun_center[abun_center$species == species1, ]))[2]
        beta2 <- coef(lm(rel_abun_diff ~ year, data=abun_center[abun_center$species == species2, ]))[2]
        pairwise_contribution <- calculate_pairwise_contributions_center(abun_center, species1, species2, beta1, beta2, var_time)
        var_contributions_center[species1] <- var_contributions_center[species1] + pairwise_contribution
        var_contributions_center[species2] <- var_contributions_center[species2] + pairwise_contribution
      }
    }
    
    all_var_contributions_center[[i]] <- var_contributions_center
    var_sums_center[i] <- sum(var_contributions_center, na.rm = TRUE)
  }
  
  names(all_var_contributions_center) <- names(final_list)
  names(var_sums_center) <- names(final_list)
  
  return(list(all_var_contributions_center = all_var_contributions_center, var_sums_center = var_sums_center))
}

# Execute the function
result_center <- calculate_species_contributions_center(all_variances, final_list)
all_var_contributions_center <- result_center$all_var_contributions_center
var_sums_center <- result_center$var_sums_center



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
# Working well for contribution centered, not as well for contribution?
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



### Data for plotting
## Contribution data
# Initialize an empty data frame
contribution_df <- data.frame()

# Iterate through each element in all_contributions
for (site_index in seq_along(all_contributions)) {
  
  # Extract the contributions and site name
  contributions <- all_contributions[[site_index]]
  var_contributions <- all_var_contributions[[site_index]]
  site_name <- names(all_contributions)[site_index]
  contributions_center <- all_contributions_center[[site_index]]
  var_contributions_center <- all_var_contributions_center[[site_index]]
  
  # Create a temporary data frame for the current site
  temp_df <- data.frame(
    species = names(contributions),
    contribution = contributions,
    var = var_contributions,
    contribution_center = contributions_center,
    var_center = var_contributions_center,
    site = site_name
  )
  
  # Bind the temporary data frame to the main data frame
  contribution_df <- rbind(contribution_df, temp_df)
}

## Combining each site with its species' centered niche values
# Function to join only the "temp_niche_center" column from the list with the main dataframe
join_site_data <- function(site_name, site_data, main_df) {
  # Filter the main dataframe for the current site
  site_main_df <- main_df %>% filter(site == site_name)
  
  # Select only the relevant columns from the site data
  site_data <- site_data %>% dplyr::select(species, temp_niche_center, temp_niche)
  
  # Perform the join operation
  joined_df <- left_join(site_main_df, site_data, by = "species")
  
  return(joined_df)
}
# Initialize an empty dataframe to store all results
all_joined_df <- data.frame()

## Iterate over each site and its corresponding data in 'final_list_center'
# Mean temperature niche per species
niche_list <- lapply(final_list, function(df) {
  df %>%
    dplyr::select(species,temp_niche,temp_niche_center) %>%
    group_by(species) %>%
    mutate(temp_niche_center = mean(temp_niche_center)) %>%
    distinct()
})

# Joining that with plot data
for (site_name in names(niche_list)) {
  # Get the tibble for the current site
  site_data <- niche_list[[site_name]]
  
  # Join with the main dataframe, including only the "temp_niche_center" column
  joined_df <- join_site_data(site_name, site_data, contribution_df)
  
  # Bind the results together
  all_joined_df <- bind_rows(all_joined_df, joined_df)
}
all_joined_df <- all_joined_df %>%
  distinct()


## Slope data
# Convert all_slopes to a data frame
all_slopes_df <- do.call(rbind, lapply(names(all_slopes), function(site) {
  data.frame(site = site, 
             species = names(all_slopes[[site]]), 
             slope = unlist(all_slopes[[site]]),
             stringsAsFactors = FALSE)
}))

# Merge data frames
data_for_plot <- merge(all_joined_df, all_slopes_df, by = c("site", "species"), all.x = TRUE)

## CTI slope
# Convert slope_CTI to a dataframe for easier merging
slope_CTI_df <- data.frame(
  site = names(CTI_slope_list),
  slope_CTI = unlist(CTI_slope_list)
)

# Merge the slope_CTI values with data_for_plot by site
data_for_plot <- left_join(data_for_plot, slope_CTI_df, by = "site")

# Relative contribution to CTI across species
data_for_plot <- data_for_plot %>%
  mutate(rel_cont = contribution_center / abs(slope_CTI))
data_for_phylo_plot <- data_for_plot %>%
  mutate(rel_cont = contribution_center / abs(slope_CTI)) %>%
  group_by(species) %>%
  summarize(rel_cont = mean(rel_cont),
            temp_niche = mean(temp_niche))
data_for_phylo_plot_center <- data_for_plot %>%
  group_by(species) %>%
  summarize(contribution_center = mean(contribution_center))
data_for_phylo_plot_abun <- data_for_plot %>%
  group_by(species) %>%
  summarize(slope = mean(slope),
            temp_niche = mean(temp_niche))

# Add in species trait values
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TRY_data/"
setwd(path_data)
species_traits <- read_csv(" exp_species_traits.csv")

data_for_spp_plot <- data_for_plot %>%
  left_join(species_traits, by = c("species" = "AccSpeciesName"))

# Remove data whose absolute value for slope is < 0.0002
#data_for_plot <- data_for_plot[abs(data_for_plot$contribution) > 0.002, ]




### Point plot (mean-centered)
# Function to plot data for a given site
plot_site_contribution_center <- function(data, slope_data, site_name) {
  # Filter data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Extract variance in CTI slope for the site
  site_var_CTI <- CTI_variance_list[[site_name]]
  
  # Point plot for the specified site
  p <- ggplot(site_data, aes(x = contribution_center, y = reorder(species, contribution_center), color = temp_niche_center)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_rect(aes(xmin = site_slope_CTI - site_var_CTI,
                  xmax = site_slope_CTI + site_var_CTI,
                  ymin = -Inf, ymax = Inf), 
              alpha = 0.2, color = "grey", fill = "grey") +
    geom_vline(xintercept = site_slope_CTI, linetype = "dashed") +
    geom_segment(aes(x = 0, xend = slope, 
                     y = reorder(species, abs(contribution_center)), 
                     yend = reorder(species, abs(contribution_center))),
                 arrow = arrow(type = "closed", length = unit(0.075, "inches")),
                 color = "red") +
    geom_errorbar(aes(xmin = contribution_center-var_center,
                      xmax = contribution_center+var_center), 
                  width =0, color = "black") +
    geom_point(shape = 20, size = 5, position = position_dodge(width = 0.9)) +
    scale_color_gradientn(colors = c("blue", "orangered"), 
                          name = "Species temperature (°C)") +
    ggtitle(site_name) +
    xlab("Contribution to CTI") +
    ylab("Species") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold"),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 12,face="bold"))
  
  # Print the plot
  print(p)
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_list_center <- list()

for (site_name in unique(data_for_plot$site)) {
  # Generate the plot for the current site
  plot <- plot_site_contribution_center(data_for_plot, slope_CTI_df, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_list_center[[site_name]] <- plot
}
point_center_cfc <- plots_list_center[[1]]
point_center_hwrc <- plots_list_center[[2]]
point_center_jrgce <- plots_list_center[[3]]
point_center_ok <- plots_list_center[[4]]
point_center_phace <- plots_list_center[[5]]
point_center_tera <- plots_list_center[[6]]



### Scatter plot for mean-centered CTI
# Define the plotting function
scatter_func <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>%
    group_by(year,temp_treatment) %>%
    reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
    distinct() %>%
    group_by(year,temp_treatment) %>%
    summarize(mean_cti = mean(CTI)) %>%
    pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
    mutate(CTI_diff = warmed - ambient)
  
  # Scatter plot
  p <- ggscatter(site_data, x = "year", y = "CTI_diff", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = FALSE, cor.method = "pearson",
                 xlab = "Year", ylab = "CTI (warmed - ambient)",title=site_name) +
    stat_regline_equation(
      aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~")))  
  
  # Return the plot
  return(p)
}
# Initialize an overall list for all plots for convenience
all_plots <- list()

# Define a vector of dataset names corresponding to elements in `final_list`
dataframe_names <- c("PHACE", "TeRaCOn", "B4Warmed CFC", "B4Warmed HWRC", "Oklahoma", "JRGCE")

# Loop through each dataframe in final_list
for (i in seq_along(centered_list)) {
  data <- centered_list[[i]]
  dataframe_name <- dataframe_names[i]
  
  # Loop through each site in the dataframe
  for (site_name in unique(data$site)) {
    # Generate the plot for the current site
    site_plot <- scatter_func(data, site_name)
    
    # Create a descriptive plot name
    plot_name <- paste0(site_name, "_plot")
    
    # Assign the plot to a variable in the global environment
    assign(plot_name, site_plot)
    
    # If needed, store the plot in a general list for further use
    all_plots[[plot_name]] <- site_plot
  }
}
TeRaCON_plot
PHACE_plot
`B4Warmed CFC_plot`
`B4Warmed HWRC_plot`
Oklahoma_plot
JRGCE_plot



### Point plot w/ ellipse
# Plotting thermal niche vs slope
point_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Calculate the row indices for the top 5 absolute contribution_center values
  top_contributors <- site_data %>%
    mutate(abs_contribution_center = abs(contribution_center)) %>%
    arrange(desc(abs_contribution_center)) %>%
    slice(1:5)

  # Plot
  p <- ggplot(site_data, aes(x = temp_niche_center, y = slope, fill = contribution_center)) +
    geom_vline(xintercept = 0, size = 0.2, linetype = "solid", color = "black") + # Bold line at x = 0
    geom_hline(yintercept = 0, size = 0.2, linetype = "solid", color = "black") + # Bold median line
    geom_point(shape=21,color="black",size=3) +
    geom_label_repel(data = top_contributors,
                    aes(label = species), 
                    size = 3,
                    box.padding = 0.5,          # Increase the padding around the box
                    point.padding = 0.3,   
                    fill = "white",             # Background color of the label
                    color = "black",
                    nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
                    nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
                    segment.color = 'grey50') +
    #stat_ellipse(level = 0.95, aes(group = 1), color = "black") + # Add an ellipse
    ggtitle(site_name)+
    xlab("Species temperature anomaly (°C)") +
    ylab(expression(bold(Delta~Abundance))) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold")) +
    coord_cartesian(xlim = c(min(site_data$temp_niche_center) - 0.2, max(site_data$temp_niche_center) + 0.2),
                    ylim = c(min(site_data$slope) - 0.001, max(site_data$slope) +0.001))
  
  
  # Print the plot
  print(p)
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_list_ellipse <- list()

for (site_name in unique(data_for_plot$site)) {
  # Generate the plot for the current site
  plot <- point_plot(data_for_plot, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_list_ellipse[[site_name]] <- plot
}
point_ellipse_treat_cfc <- plots_list_ellipse[[1]]
point_ellipse_treat_hwrc <- plots_list_ellipse[[2]]
point_ellipse_treat_jrgce <- plots_list_ellipse[[3]]
point_ellipse_treat_ok <- plots_list_ellipse[[4]]
point_ellipse_treat_phace <- plots_list_ellipse[[5]]
point_ellipse_treat_tera <- plots_list_ellipse[[6]]



### Contour plot
# Determine top 3 contributors per site
data_for_plot <- data_for_plot %>%
  group_by(site) %>%
  mutate(abs_contribution_center = abs(contribution_center)) %>%
  arrange(desc(abs_contribution_center)) %>%
  mutate(rank = row_number()) %>%
  mutate(top_contributors = case_when(
    rank == 1 ~ "top_1",
    rank == 2 ~ "top_2",
    rank == 3 ~ "top_3",
    TRUE ~ "none"
  )) %>%
  dplyr::select(-abs_contribution_center, -rank)
# Saving top contributors to a dataframe
#output <- data_for_plot %>%
# dplyr::select(site,species,top_contributors)
#path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
#write.csv(output, paste(path_out,"top_contributors.csv"), row.names = FALSE)

contour_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min (site_data$temp_niche_center-0.1), max (site_data$temp_niche_center+0.1), length.out = 50)
  slope <- seq(min (site_data$slope-0.001), max (site_data$slope+0.001), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund_diff = slope)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly*abund_diff)
  
  top_contributors <- site_data %>% 
    mutate(abs_contribution_center = abs(contribution_center)) %>%
    arrange(desc(abs_contribution_center)) %>%
    slice(1:3)
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund_diff)) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    #geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    #metR::geom_text_contour(aes(z = spp_contrib), color = "black") + # Add contour labels
    geom_point(data=site_data,aes(x=temp_niche_center,y=slope,shape=top_contributors,alpha=top_contributors),size=3) +
    scale_alpha_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 1, "top_2" = 1, "top_3" = 1, "none" = 0.4),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_shape_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 17, "top_2" = 18, "top_3" = 8, "none" = 20),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto β CTI"
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggtitle(site_name) +
    labs(
      x = "Species temperature anomaly (°C)                                    ",
      #y = expression(bold(atop(Delta~Abundance,(Delta~Warmed - Delta~Ambient)))),
      y = "β Abundance (Warmed - Ambient)"
    ) +
    geom_label_repel(data = top_contributors,
                     aes(x = temp_niche_center, y = slope, label = species),
                     size = 3.5,
                     box.padding = 0.5,          # Increase the padding around the box
                     point.padding = 0.3,   
                     fill = "white",             # Background color of the label
                     color = "black",
                     nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
                     nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
                     min.segment.length = unit(0, 'lines'),
                     segment.color = 'grey50') +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold"),
          plot.title = element_text(size=14, face="bold"),
          legend.text=element_text(size=14),
          legend.title=element_text(size=14)) +
    #coord_cartesian(clip = 'off') +
    coord_cartesian(expand = FALSE) # Ensure no expansion on axes
  
  
  # Print the plot
  print(p)
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_contours <- list()

for (site_name in unique(data_for_plot$site)) {
  # Generate the plot for the current site
  plot <- contour_plot(data_for_plot, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_contours[[site_name]] <- plot
}
point_contours_treat_ok <- plots_contours[[1]]
point_contours_treat_phace <- plots_contours[[2]]
point_contours_treat_tera <- plots_contours[[3]]
point_contours_treat_jrgce <- plots_contours[[4]]
point_contours_treat_hwrc<- plots_contours[[5]]
point_contours_treat_cfc <- plots_contours[[6]]

# Arrange plots into multi-panel figure
# Function to remove color, shape, and alpha guides
legend_rem <- function(plt){
  plt <- plt +
    guides(shape = "none", alpha = "none")
  return(plt)   
}
# Get shape legend
shp_lgnd <- point_contours_treat_jrgce +
  guides(fill = "none") +
  theme(axis.text.y = element_blank(),
        plot.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, 0.5),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "white", fill='white', size=1)) 
# Specify plot layout
design <- "
  aaaaaabbbbbb##
  aaaaaabbbbbb##
  aaaaaabbbbbb##
  ccccccddddddgg
  ccccccddddddgg
  ccccccddddddgg
  eeeeeeffffff##
  eeeeeeffffff##
  eeeeeeffffff##
"
# Plot
png("contours_5.png", units="in", width=13, height=12, res=300)
legend_rem(point_contours_treat_jrgce) + 
  legend_rem(point_contours_treat_phace) + 
  legend_rem(point_contours_treat_tera) + 
  legend_rem(point_contours_treat_ok) + 
  legend_rem(point_contours_treat_cfc) + 
  legend_rem(point_contours_treat_hwrc) + 
  shp_lgnd +
  plot_layout(design = design, 
              axis_titles = "collect")
dev.off()



### Contour plot - all species
# If a species is present in >1 experiment, take the average
data_for_plot2 <- data_for_plot %>%
  dplyr::select(species,temp_niche_center,slope) %>%
  group_by(species) %>%
  mutate(temp_niche_center = mean(temp_niche_center),
         slope = mean(slope),
         contribution_center = temp_niche_center*slope) %>%
  distinct()
# Plotting all species onto the contour background
contour_plot_all <- function(data) {
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min (data$temp_niche_center), max (data$temp_niche_center), length.out = 50)
  slope <- seq(min (data$slope), max (data$slope), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund_diff = slope)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly*abund_diff)
  
  top_contributors <- data %>%
    mutate(abs_contribution_center = abs(contribution_center)) %>%
    arrange(desc(abs_contribution_center))
  top_contributors <- top_contributors[1:10,]
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund_diff)) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
   # metR::geom_text_contour(aes(z = spp_contrib), color = "black") + # Add contour labels
    geom_point(data=data,aes(x=temp_niche_center,y=slope),
               size=2,alpha=0.6) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)"
    ) +
    geom_label_repel(data = top_contributors,
                     aes(x = temp_niche_center, y = slope, label = species),
                     size = 3,
                     box.padding = 0.5,          # Increase the padding around the box
                     point.padding = 0.3,   
                     fill = "white",             # Background color of the label
                     color = "black",
                     nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
                     nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
                     segment.color = 'grey50') +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    labs(x = "Species temperature anomaly (°C)",
         y = expression(bold(Delta~Abundance)),
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold")) +
    coord_cartesian(expand = FALSE) # Ensure no expansion on axes
  
  
  # Print the plot
  print(p)
}
all_spp_contour <- contour_plot_all(data_for_plot2)


### Merging all spp contour with individual site contours
a <-ggarrange(
  all_spp_contour,
  ggarrange(point_contours_treat_cfc, point_contours_treat_hwrc, point_contours_treat_ok,nrow = 3), 
  ncol=2, widths=c(1.5, 0.8)
)
b <- ggarrange(point_contours_treat_jrgce, point_contours_treat_phace, point_contours_treat_tera,ncol = 3,
               widths=c(1.05,1.05,1.1)) +
  theme(plot.margin = margin(0.1,0,0.1,0.2,"cm")) 
all_sites_contour <- ggarrange(a,b,
          nrow=2,heights=c(1.7,0.6))
# Save to computer
png("spp_cont2.png", units="in", width=17, height=14, res=300)
ggarrange(a,b,
          nrow=2,heights=c(1.7,0.6))
dev.off()



### Spp traits and contour plot
# If a species is present in >1 experiment, take the average
data_for_spp_plot2 <- data_for_spp_plot %>%
  dplyr::select(species,temp_niche_center,slope,TraitName,mean_trait_val) %>%
  group_by(species) %>%
  mutate(temp_niche_center = mean(temp_niche_center),
         slope = mean(slope),
         contribution_center = temp_niche_center*slope) %>%
  distinct()
# Fixing long trait names
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)"] <- "LDMC"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Fine root length per fine root dry mass (specific fine root length, SRL)"] <- "Specific fine root length"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Root length per root dry mass (specific root length, SRL)"] <- "Specific root length"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Plant biomass and allometry: Fine root length per plant"] <- "Fine root length"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Plant biomass and allometry: Seed number per plant"] <- "Seed number per plant"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"] <- "SLA"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Coarse root length per coarse root dry mass (specific coarse root length, SRL)"] <- "Specific coarse root length"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Plant biomass and allometry: Coarse root length per plant"] <- "Coarse root length"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Plant morphological adaptations: seed or dispersal unit metamorphoses"] <- "Seed or dispersal unit metamorphoses"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Seed germination rate (germination efficiency)"] <- "Seed germination rate"
data_for_spp_plot$TraitName[data_for_spp_plot$TraitName == "Seed (seedbank) longevity"] <- "Seed longevity"

# Plotting all species onto the contour background
traits_contour_plot_all <- function(data, trait) {
  
  # Select trait
  data <- data %>%
    filter(TraitName == trait)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min (data$temp_niche_center), max (data$temp_niche_center), length.out = 50)
  slope <- seq(min (data$slope), max (data$slope), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund_diff = slope)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly*abund_diff)
  
  #top_contributors <- data %>%
  #  mutate(abs_contribution_center = abs(contribution_center)) %>%
  #  arrange(desc(abs_contribution_center))
  #top_contributors <- top_contributors[1:10,]
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund_diff)) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    # metR::geom_text_contour(aes(z = spp_contrib), color = "black") + # Add contour labels
    geom_point(data=data,aes(x=temp_niche_center,y=slope,size=mean_trait_val),
               alpha=0.6) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)"
    ) +
    ggtitle(trait) +
    #geom_label_repel(data = top_contributors,
    #                 aes(x = temp_niche_center, y = slope, label = species),
    #                 size = 3,
    #                 box.padding = 0.5,          # Increase the padding around the box
    #                 point.padding = 0.3,   
    #                 fill = "white",             # Background color of the label
    #                 color = "black",
    #                 nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
    #                 nudge_y = 0.0001,          # Slightly nudge labels in the y-direction if needed
    #                 segment.color = 'grey50') +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    labs(x = "Species temperature anomaly (°C)",
         y = expression(bold(Delta~Abundance)),
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold")) +
    coord_cartesian(expand = FALSE) # Ensure no expansion on axes
  
  
  # Print the plot
  print(p)
}
traits_contour_plot_all(data_for_spp_plot,"Seed dry mass")
traits_contour_plot_all(data_for_spp_plot,"Plant height vegetative")
traits_contour_plot_all(data_for_spp_plot,"Seed germination rate (germination efficiency)")
traits_contour_plot_all(data_for_spp_plot,"Plant biomass and allometry: Seed number per plant")
traits_contour_plot_all(data_for_spp_plot,"Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded")
traits_contour_plot_all(data_for_spp_plot,"Seed (seedbank) longevity")
traits_contour_plot_all(data_for_spp_plot,"Root length per root dry mass (specific root length, SRL)")



### Regress trait values and contribution
# Remove non-numeric traits or traits with low sample sizes
data_for_spp_plot2 <- data_for_spp_plot %>%
  filter(!(TraitName == "Coarse root length" |
             TraitName == "Specific coarse root length" |
             TraitName == "Dispersal syndrome" |
             TraitName == "Seed or dispersal unit metamorphoses" |
             TraitName == "Seedbank duration")) %>%
  filter(!is.na(TraitName)) %>%
  filter(!(TraitName == "Fine root length" & mean_trait_val > 3000)) %>%
  filter(!(TraitName == "Seed length" & mean_trait_val > 1000)) %>%
  filter(!(TraitName == "Plant height vegetative" & mean_trait_val > 500)) %>%
  filter(!(TraitName == "Specific fine root length" & mean_trait_val > 400))
# Make a function to select a given trait name and plot it against contribution
trait_con_plot <- function(data,trait){
  plot_data <- data %>%
    filter(TraitID == trait)
  
  trait_name <- plot_data$TraitName[1]
  
  ggplot(plot_data, aes(x = log(mean_trait_val), y = contribution_center)) +
    geom_point() +
    geom_smooth(method="lm") +
    labs(title = trait_name, x = "Trait value", y = "Species contribution") +
    theme_minimal()
}
#trait_niche_plot(traits_niche_rem,3106)

# Define the desired order of TraitName
trait_order <- c(26,27,131,66,95,33,3117,47,46,21,3106,2026,614,1080)  # Modify as needed

# Convert TraitName to a factor with specified levels
data_for_spp_plot2$TraitID <- factor(data_for_spp_plot2$TraitID, levels = trait_order)

# Iterate over the ordered TraitID values
plots_list <- list()  # Initialize an empty list to store plots
for (trait in levels(data_for_spp_plot2$TraitID)) {
  trait_id <- as.numeric(trait)  # Convert factor level back to numeric
  
  # Filter and ensure valid data
  plot_data <- data_for_spp_plot2 %>% filter(TraitID == trait_id)
  
  if (nrow(plot_data) > 0) {  # Ensure data exists for this TraitID
    p <- trait_con_plot(data_for_spp_plot2, trait_id)
    plots_list[[trait]] <- p  # Store with TraitID as key
  } else {
    message("Warning: No data for TraitID ", trait_id)
  }
}

# Arrange all plots in a grid
wrap_plots(grobs = plots_list) +
  plot_layout(guides = "collect",axis_titles = "collect")



### How many species per quadrant for contour plot?
quad1 <- data_for_plot %>%
  filter(slope > 0, temp_niche_center > 0)
nrow(quad1)
quad2 <- data_for_plot %>%
  filter(slope > 0, temp_niche_center < 0)
nrow(quad2)
quad3 <- data_for_plot %>%
  filter(slope < 0, temp_niche_center < 0)
nrow(quad3)
quad4 <- data_for_plot %>%
  filter(slope < 0, temp_niche_center > 0)
nrow(quad4)
# Total sum of contributions
sum_cont <- data_for_plot %>%
  summarize(sum_value = sum(slope * temp_niche_center)) %>%
  pull(sum_value)

# Plotting all species onto the contour background, and including numbers from above
contour_plot_all_vals <- function(data) {
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min (data$temp_niche_center), max (data$temp_niche_center), length.out = 50)
  slope <- seq(min (data$slope), max (data$slope), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund_diff = slope)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly*abund_diff)
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund_diff)) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    # metR::geom_text_contour(aes(z = spp_contrib), color = "black") + # Add contour labels
    geom_point(data=data,aes(x=temp_niche_center,y=slope),
               size=2,alpha=0.6) +
    annotate("text", x = 4, y = 0.02, label = "67") +
    annotate("text", x = -4, y = 0.02, label = "70") +
    annotate("text", x = -4, y = -0.01, label = "83") +
    annotate("text", x = 4, y = -0.01, label = "50") +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Relative\nspecies\ncontribution\nto CTI (°C)"
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    labs(x = "Species temperature anomaly (°C)",
         y = expression(bold(Delta~Abundance)),
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold")) +
    coord_cartesian(expand = FALSE) # Ensure no expansion on axes
  
  
  # Print the plot
  print(p)
}
all_spp_contour_vals <- contour_plot_all_vals(data_for_plot)



### Phylo tree
# Fixing species names
phylo_plot <- function(data, slope_data, phylo, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Fixing species names
  phylo$tip.label <- phylo$tip.label %>% gsub("_", " ", .)

  # Adding padding to species name to right-align names
  phylo2 <- data.frame(label = phylo$tip.label, 
                     newlabel = label_pad(phylo$tip.label))

  tree <- ggtree(phylo) %<+% phylo2 + xlim(NA, 5)

  tree2 <- tree + geom_tiplab(aes(label=newlabel), 
                            align=TRUE, family='mono',
                            linetype = "dotted", linesize = .7) +
    ggplot2::xlim(0, 220)

  # Extracting the species name order from the tree
  tip_order <- tree2$data %>% 
  filter(isTip) %>% 
  arrange(y) %>% 
  pull(label)

  # Point plot w/ tree
  site_data$species_tree <- site_data$species

  site_data$species_tree <- factor(site_data$species_tree, levels = tip_order)
  
  point_plot <- ggplot(site_data, aes(x=contribution_center, y = species_tree, color=temp_niche_center)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_vline(xintercept = site_slope_CTI, linetype = "dashed") +
    geom_errorbar(aes(xmin = contribution_center-var_center,
                      xmax = contribution_center+var_center), 
                      width =0, color = "black") +
    geom_point(shape = 20, size = 5, position = position_dodge(width = 0.9)) +
    scale_color_gradientn(colors = c("blue", "orangered"), 
                          name = "Species temperature (°C)") +
    ggtitle(site_name) +
    xlab("Contribution to CTI") +
    ylab(NULL) +
    theme_classic() +
    theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12,face="bold"),
        panel.grid.major.y = element_line())
  
  joined <- ggarrange(tree2, point_plot, ncol = 2, align = "h")
  
  print(joined)
}

# Combined point and tree plot
phylo_plot(data_for_plot, slope_CTI_df, phylo_jrgce, "JRGCE")



# below not working?
### Phylo tree (per site circular)
# Fixing species names
phylo_circ_plot <- function(data, phylo, site_name) {
  # Filter the data for the specified site
  site_data <- data %>%
    filter(site == site_name) %>%
    select(species, contribution_center)
  
  phylo$tip.label <- phylo$tip.label %>% gsub("_", " ", .)
  
  tree_circ <- ggtree(phylo, layout = "circular") + theme_tree()  # Plot the tree
  
  # Extracting the species name order from the tree
  order_circ <- tree_circ$data %>% 
    filter(isTip) %>% 
    arrange(y) %>% 
    pull(label)
  
  site_data$species <- factor(site_data$species, levels = order_circ)
  
  # Plot the tree
  circ_tree <- ggtree(phylo, layout = "circular") %<+% site_data +  # The '%<+%' operator merges data into ggtree
    geom_tiplab(aes(color = contribution_center)) +  # Color by contribution_value
    scale_color_gradient2(
      low = "blue", mid = "grey40", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)",
      breaks=seq(min(site_data$contribution_center),max(site_data$contribution_center),
                 (max(site_data$contribution_center)-min(site_data$contribution_center))/3)
    ) +
    theme_tree()
  
  print(circ_tree)
}

# Plot
phylo_plot_phace <- phylo_circ_plot(data_for_plot, phylo_phace, "PHACE")
phylo_plot_cfc <- phylo_circ_plot(data_for_plot, phylo_b4, "B4Warmed CFC")
phylo_plot_hwrc <- phylo_circ_plot(data_for_plot, phylo_b4, "B4Warmed HWRC")
phylo_plot_tera <- phylo_circ_plot(data_for_plot, phylo_tera, "TeRaCON")
phylo_plot_ok <- phylo_circ_plot(data_for_plot, phylo_ok, "Oklahoma")
phylo_plot_jrgce <- phylo_circ_plot(data_for_plot, phylo_jrgce, "JRGCE")



### All species phylo tree
# Fixing species names
all_phylo$tip.label <- all_phylo$tip.label %>% gsub("_", " ", .)

# Adding padding to species name to right-align names
all_phylo2 <- data.frame(label = all_phylo$tip.label, 
                     newlabel = label_pad(all_phylo$tip.label))

all_tree <- ggtree(all_phylo) %<+% all_phylo2 + xlim(NA, 5)

all_tree2 <- all_tree + geom_tiplab(aes(label=newlabel), 
                            align=TRUE, family='mono',
                            linetype = "dotted", linesize = .7) +
  ggplot2::xlim(0, 550)

# Extracting the species name order from the tree
tip_order <- all_tree2$data %>% 
  filter(isTip) %>% 
  arrange(y) %>% 
  pull(label)

# Point plot w/ tree
data_for_phylo_plot$species_tree <- data_for_phylo_plot$species

data_for_phylo_plot$species_tree <- factor(data_for_phylo_plot$species_tree, levels = tip_order)

point_plot <- ggplot(data_for_phylo_plot, aes(x=rel_cont, y = species_tree,color=temp_niche)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_point(shape = 20, size = 5, position = position_dodge(width = 0.9)) +
  scale_color_gradientn(colors = c("blue", "red"), 
                        name = "Species temperature (°C)") +
  xlab("Relative contribution to CTI") +
  ylab(NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12,face="bold"),
        panel.grid.major.y = element_line())

#png("phylo.png", units="in", width=12, height=25, res=300)
all_spp_phylo <- ggarrange(all_tree2, point_plot, ncol = 2, align = "h")
#dev.off()



### All spp phylo tree circular
# Fixing species names
# Replace underscores in both to be safe
all_phylo$tip.label <- gsub("_", " ", all_phylo$tip.label)
data_for_plot$species <- gsub("_", " ", data_for_plot$species)

all_tree_circ <- ggtree(all_phylo, layout = "circular") + theme_tree()  # Plot the tree

# Extracting the species name order from the tree
tip_order_circ <- all_tree_circ$data %>% 
  filter(isTip) %>% 
  arrange(y) %>% 
  pull(label)

# Using relative contribution
data_for_phylo_plot2 <- data_for_phylo_plot %>%
  dplyr::select(species, contribution_center)
data_for_phylo_plot2$species <- factor(data_for_phylo_plot2$species, levels = tip_order_circ)
data_for_phylo_plot2 <- data_for_phylo_plot2 %>%
  filter(!is.na(species)) %>%
  group_by(species) %>%
  summarize(
    contribution_center = mean(contribution_center, na.rm = TRUE))
# Using plain contribution center values
data_for_phylo_plot_center2 <- data_for_phylo_plot_center %>%
  dplyr::select(species, contribution_center)
data_for_phylo_plot_center2$species <- factor(data_for_phylo_plot_center2$species, levels = tip_order_circ)
data_for_phylo_plot_center2 <- data_for_phylo_plot_center2 %>%
  filter(!is.na(species)) %>%
  group_by(species) %>%
  summarize(
    contribution_center = mean(contribution_center, na.rm = TRUE))

# Merging with family informaiton
### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/phylogenies/"
setwd(path_data)
# Load in data
families <- read.csv(" species_families.csv")
families <- left_join(data_for_phylo_plot_center2, families, by = "species")

# Abundance data
data_for_phylo_plot_abun <- data_for_phylo_plot_abun %>%
  dplyr::select(species, slope)
data_for_phylo_plot_abun$species <- factor(data_for_phylo_plot_abun$species, levels = tip_order_circ)

# Plot the tree
png("phylo.png", units="in", width=18, height=18, res=300)
#circ_tree <- 
  ggtree(all_phylo, layout = "circular") %<+% data_for_phylo_plot_center2 +  # The '%<+%' operator merges data into ggtree
  geom_tiplab(aes(color = contribution_center)) +  # Color by contribution_value
  scale_color_gradient2(
    low = "blue", mid = "grey60", high = "red", midpoint = 0,
    name = "Species\ncontribution\nto β CTI (°C)"
  ) +
  #geom_hilight(node=452, fill="gold") + 
  geom_hilight(node=449, fill="gold",alpha=0.4) + 
  geom_hilight(node=287, fill="lightskyblue",alpha=0.4) + 
 #geom_text(aes(label=node), hjust=-.3) +
  theme_tree()
dev.off()
png("phylo_abun.png", units="in", width=18, height=18, res=300)
ggtree(all_phylo, layout = "circular") %<+% data_for_phylo_plot_abun +  # The '%<+%' operator merges data into ggtree
  geom_tiplab(aes(color = slope)) +  # Color by contribution_value
  scale_color_gradient2(
    low = "blue", mid = "grey60", high = "red", midpoint = 0,
    name = "Species\nβ Abundance"
  ) +
  geom_hilight(node=449, fill="gold",alpha=0.4) + 
  geom_hilight(node=287, fill="lightskyblue",alpha=0.4) + 
  #geom_text(aes(label=node), hjust=-.3) +
  theme_tree()
dev.off()



### Merging all spp tree with individual site trees
a <- ggarrange(
  circ_tree,
  ggarrange(phylo_plot_cfc, phylo_plot_hwrc, phylo_plot_ok,nrow = 3), 
  ncol=2, widths=c(1.5, 0.8)
)
b <- ggarrange(phylo_plot_jrgce, phylo_plot_phace, phylo_plot_tera,ncol = 3,
               widths=c(1.05,1.05,1.1)) +
  theme(plot.margin = margin(0.1,0,0.1,0.2,"cm")) 
all_sites_phylos <- ggarrange(a,b,
                               nrow=2,heights=c(1.7,0.6))
# Save to computer
png("spp_cont2.png", units="in", width=17, height=14, res=300)
ggarrange(a,b,
          nrow=2,heights=c(1.7,0.6))
dev.off()



### Top contributors %
# Function to calculate top species contributing to 95% of absolute contributions per site
calculate_top_species_per_site <- function(df, contributions_col = "contribution_center", target_percent = 95) {
  
  total_species <- df %>%
    group_by(site) %>%
    summarise(total_spp = n(), .groups = "drop")
  
  top_species_details <- df %>%
    group_by(site) %>%
    arrange(site, desc(abs(!!sym(contributions_col)))) %>%
    mutate(
      abs_contribution = abs(!!sym(contributions_col)),
      species_percent = abs_contribution / sum(abs_contribution) * 100,
      cumulative_contribution = cumsum(abs_contribution),
      cumulative_percentage = cumulative_contribution / sum(abs_contribution) * 100
    ) %>%
    filter(row_number() <= which.max(cumulative_percentage >= target_percent)) %>%
    dplyr::select(site, species, species_percent, cumulative_percentage)
  
  top_species_summary <- top_species_details %>%
    group_by(site) %>%
    summarise(
      top_species_count = n(),
      captured_percentage = max(cumulative_percentage),
      species_details = list(
        tibble(species = species, percent = round(species_percent, 2))
      ),
      .groups = "drop"
    )
  
  top_species_summary %>%
    left_join(total_species, by = "site") %>%
    mutate(percent_species_contributing = round((top_species_count / total_spp) * 100, 1))
}
# Calculate the top-contributing species per site
top_species_per_site <- calculate_top_species_per_site(data_for_plot, contributions_col = "contribution_center", target_percent = 94)



### Contribution data for table
table_data <- data_for_plot %>%
  dplyr::select(site, species, contribution_center,temp_niche_center,slope, slope_CTI)


### Boxplots
boxplot_data <- data_for_plot
boxplot_data$temp_designation <- NA
boxplot_data$temp_designation[boxplot_data$temp_niche_center > 0 &
                                boxplot_data$slope > 0] <- "Warm increasing"
boxplot_data$temp_designation[boxplot_data$temp_niche_center > 0 &
                                boxplot_data$slope < 0] <- "Warm decreasing"
boxplot_data$temp_designation[boxplot_data$temp_niche_center < 0 &
                                boxplot_data$slope > 0] <- "Cold increasing"
boxplot_data$temp_designation[boxplot_data$temp_niche_center < 0 &
                                boxplot_data$slope < 0] <- "Cold decreasing"
boxplot_data$temp_designation[boxplot_data$slope == 0] <- NA
# Boxplot for contributions based on temp designation
ggplot(boxplot_data, aes(x = temp_designation, y = rel_cont)) +
  geom_boxplot() +
  xlab("Temperature designation") +
  ylab("Contribution to CTI") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12,face="bold"))
  


### Export Rdata for plots
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(point_center_cfc, paste(path_out,'point_center_treat_cfc.rds'))
saveRDS(point_center_hwrc, paste(path_out,'point_center_treat_hwrc.rds'))
saveRDS(point_center_jrgce, paste(path_out,'point_center_treat_jrgce.rds'))
saveRDS(point_center_ok, paste(path_out,'point_center_treat_ok.rds'))
saveRDS(point_center_phace, paste(path_out,'point_center_treat_phace.rds'))
saveRDS(point_center_tera, paste(path_out,'point_center_treat_tera.rds'))

saveRDS(TeRaCON_plot, paste(path_out,'scatter_treat_tera.rds'))
saveRDS(PHACE_plot, paste(path_out,'scatter_treat_phace.rds'))
saveRDS(`B4Warmed CFC_plot`, paste(path_out,'scatter_treat_cfc.rds'))
saveRDS(`B4Warmed HWRC_plot`, paste(path_out,'scatter_treat_hwrc.rds'))
saveRDS(Oklahoma_plot, paste(path_out,'scatter_treat_ok.rds'))
saveRDS(JRGCE_plot, paste(path_out,'scatter_treat_jrgce.rds'))

saveRDS(point_ellipse_treat_cfc, paste(path_out,'point_ellipse_treat_cfc.rds'))
saveRDS(point_ellipse_treat_hwrc, paste(path_out,'point_ellipse_treat_hwrc.rds'))
saveRDS(point_ellipse_treat_jrgce, paste(path_out,'point_ellipse_treat_jrgce.rds'))
saveRDS(point_ellipse_treat_ok, paste(path_out,'point_ellipse_treat_ok.rds'))
saveRDS(point_ellipse_treat_phace, paste(path_out,'point_ellipse_treat_phace.rds'))
saveRDS(point_ellipse_treat_tera, paste(path_out,'point_ellipse_treat_tera.rds'))

saveRDS(point_contours_treat_cfc, paste(path_out,'point_contours_treat_cfc.rds'))
saveRDS(point_contours_treat_hwrc, paste(path_out,'point_contours_treat_hwrc.rds'))
saveRDS(point_contours_treat_jrgce, paste(path_out,'point_contours_treat_jrgce.rds'))
saveRDS(point_contours_treat_ok, paste(path_out,'point_contours_treat_ok.rds'))
saveRDS(point_contours_treat_phace, paste(path_out,'point_contours_treat_phace.rds'))
saveRDS(point_contours_treat_tera, paste(path_out,'point_contours_treat_tera.rds'))

saveRDS(contours_treat_final, paste(path_out,'point_contours_treat_final.rds'))

saveRDS(all_spp_contour, paste(path_out,'point_contours_treat_all.rds'))
saveRDS(all_spp_contour_vals, paste(path_out,'point_contours_treat_vals_all.rds'))
saveRDS(all_sites_contour, paste(path_out,'all_sites_contour.rds'))

saveRDS(all_spp_phylo, paste(path_out,'phylo_plot_all.rds'))
saveRDS(circ_tree, paste(path_out,'phylo_plot_all_circ.rds'))
saveRDS(all_sites_phylos, paste(path_out,'phylo_plot_all_comb_circ.rds'))
saveRDS(phylo_plot_phace, paste(path_out,'phylo_plot_phace.rds'))
saveRDS(phylo_plot_cfc, paste(path_out,'phylo_plot_cfc.rds'))
saveRDS(phylo_plot_hwrc, paste(path_out,'phylo_plot_hwrc.rds'))
saveRDS(phylo_plot_tera, paste(path_out,'phylo_plot_tera.rds'))
saveRDS(phylo_plot_ok, paste(path_out,'phylo_plot_ok.rds'))
saveRDS(phylo_plot_jrgce, paste(path_out,'phylo_plot_jrgce.rds'))

saveRDS(data_for_spp_plot2, paste(path_out,'trait_contribution_data.rds'))

write.csv(table_data, "my_dataframe.csv", row.names = FALSE)





