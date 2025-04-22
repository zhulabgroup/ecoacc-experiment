# TITLE:          Calculating contribution to CTI
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    Species individual contributions to CTI
# PROJECT:        EcoAcc
# DATE:           Jan 2025



### Load packages
library(tidyverse)
library(stringr)

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


### Merging with treatment info
phace <- phace %>%
  left_join(phace_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
tera <- tera %>%
  left_join(tera_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
b4_cfc <- b4_cfc %>%
  left_join(b4_cfc_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
b4_hwrc <- b4_hwrc %>%
  left_join(b4_hwrc_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
ok <- ok %>%
  left_join(ok_treat, by = "plot") %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)
jrgce <- jrgce %>%
  left_join(jrgce_treat, by = c("year","plot")) %>%
  dplyr::select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)

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
final_list <- lapply(merged_list, function(df) {
  df %>%
    mutate(temp_niche_center = temp_niche - cti_baseline)
})



### Calculating CTI
# Using raw temp niche values
calculate_CTI <- function(data) {
  data %>%
    group_by(year, plot, temp_treatment) %>%
    reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
    distinct()
}
cti_results <- lapply(final_list, calculate_CTI)

# Using mean-centered niche values
calculate_CTI_center <- function(data) {
  data %>%
    group_by(year, plot, temp_treatment) %>%
    reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
    distinct()
}
cti_results_center <- lapply(final_list, calculate_CTI_center)



### Compute linear slopes using covariance and variance
## CTI
# Slope values are the same whether using raw or centered temp niche values
calculate_CTI_slope <- function(data) {
  data %>%
    group_by(temp_treatment) %>%
    summarise(slope = cov(year, CTI) / var(year))
  #cov(data$year, data$CTI) / var(data$year)
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
    
    slope_data <- species_data %>%
      group_by(temp_treatment) %>%
      summarise(slope = cov(year, rel_abund) / var(year))
    
    # Store the slope in the list
    slopes[[species]] <- slope_data
  }
  
  return(slopes)
}
all_slopes <- lapply(final_list, calculate_slopes)



#########  Skip for now ###########
### Calculate the variance of the slopes using residuals
## CTI
calculate_CTI_variance <- function(data) {
  cti_variances <- list()
  
  # Calculate variance
  resid_CTI <- residuals(lm(data$CTI ~ data$year))
  var <- sum(resid_CTI^2) / (length(data$CTI) - 2) / sum((data$year - mean(data$year))^2)
  
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
    value <- species_data$rel_abun
    
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
all_contributions <- list()

# Iterate over each element in the all_slopes list
for (i in seq_along(all_slopes)) {
  
  result_list <- list()
  
  # Extract slopes for the current dataframe
  slopes <- all_slopes[[i]]
  
  # Determine the corresponding dataframe
  abun <- final_list[[i]]
  
  # Calculate the contribution for each species
  for (species in names(slopes)) {
    
    niche_value <- abun$temp_niche[abun$species == species]
    
    contribution <- slopes[[species]] %>%
      group_by(temp_treatment) %>%
      summarize(contribution = slope * niche_value) %>%
      distinct()
    
    result_list[[species]] <- contribution
  }
  
  # Store the named vector of contributions in all_contributions
  all_contributions[[i]] <- result_list

}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_contributions) <- names(all_slopes)


## Using mean-centered values
# Initialize a list to store the contributions for each dataframe
all_contributions_center <- list()

# Iterate over each element in the all_slopes list
for (i in seq_along(all_slopes)) {
  
  result_list <- list()
  
  # Extract slopes for the current dataframe
  slopes <- all_slopes[[i]]
  
  # Determine the corresponding dataframe
  abun <- final_list[[i]]
  
  # Calculate the contribution for each species
  for (species in names(slopes)) {
    
    niche_value <- abun$temp_niche_center[abun$species == species]
    
    contribution <- slopes[[species]] %>%
      group_by(temp_treatment) %>%
      summarize(contribution = slope * niche_value) %>%
      distinct()
    
    result_list[[species]] <- contribution
  }
  
  # Store the named vector of contributions in all_contributions
  all_contributions_center[[i]] <- result_list
  
}

# Optionally, set names for the elements in all_contributions and slope_sums
names(all_contributions_center) <- names(all_slopes)



########## skip for now ##############
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



### Data for plotting
## Contribution data
all_contributions_df <- imap_dfr(all_contributions, ~ {
  site_name <- .y  # Get the site name
  # Iterate over each species tibble within the site
  imap_dfr(.x, ~ {
    species_name <- .y  # Get the species name
    tibble_data <- .x
    # Add species and site columns
    tibble_data %>%
      mutate(species = species_name, site = site_name)
  })
})

## Combining each site with its species' centered niche values
# Function to join only the "temp_niche_center" column from the list with the main dataframe
join_site_data <- function(site_name, site_data, main_df) {
  # Filter the main dataframe for the current site
  site_main_df <- main_df %>% filter(site == site_name)
  
  # Select only the relevant columns from the site data
  site_data <- site_data %>% select(species, temp_niche_center, temp_niche)
  
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
    select(species,temp_niche,temp_niche_center) %>%
    group_by(species) %>%
    mutate(temp_niche_center = mean(temp_niche_center)) %>%
    distinct()
})

# Joining that with plot data
for (site_name in names(niche_list)) {
  # Get the tibble for the current site
  site_data <- niche_list[[site_name]]
  
  # Join with the main dataframe, including only the "temp_niche_center" column
  joined_df <- join_site_data(site_name, site_data, all_contributions_df)
  
  # Bind the results together
  all_joined_df <- bind_rows(all_joined_df, joined_df)
}


## Slope data
# Convert all_slopes to a data frame
all_slopes_df <- imap_dfr(all_slopes, ~ {
  site_name <- .y  # Get the site name
  # Iterate over each species tibble within the site
  imap_dfr(.x, ~ {
    species_name <- .y  # Get the species name
    tibble_data <- .x
    # Add species and site columns
    tibble_data %>%
      mutate(species = species_name, site = site_name)
  })
})

# Merge data frames
data_for_plot <- merge(all_joined_df, all_slopes_df, by = c("site", "temp_treatment","species"), all.x = TRUE)

### Adding in top contributor designations (determined in contributions_treatment.R)
### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
# Load in data
top_contributors <- read.csv(" top_contributors.csv")
# Merge with data
data_for_plot <- left_join(data_for_plot, top_contributors, by = c("site","species"))

## CTI slope
# Convert slope_CTI to a dataframe for easier merging
#slope_CTI_df <- data.frame(
#  site = names(CTI_slope_list),
#  slope_CTI = unlist(CTI_slope_list)
#)

# Merge the slope_CTI values with data_for_plot by site
#data_for_plot <- left_join(data_for_plot, slope_CTI_df, by = "site")

# Remove data whose absolute value for slope is < 0.0002
#data_for_plot <- data_for_plot[abs(data_for_plot$contribution) > 0.002, ]



### Contour plot
contour_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min(site_data$temp_niche), max(site_data$temp_niche), length.out = 50)
  abun <- seq(min(site_data$slope), max(site_data$slope), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund = abun)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly * abund)
  
  # Get unique temp_treatment categories
  unique_treatments <- unique(site_data$temp_treatment)
  
  # Define the color mapping based on available treatments
  treatment_colors <- c("ambient" = "blue", "1.7" = "orange", "warmed" = "red", "3.4" = "red", "amb" = "blue")
  labels = c("ambient" = "Ambient","warmed" = "Warmed","1.7" = "Intermediate", "3.4" = "Warmed", "amb" = "Ambient")
  used_colors <- treatment_colors[names(treatment_colors) %in% unique_treatments]
  used_labels <- labels[names(labels) %in% unique_treatments]
  
  # Top species for name labels per year
  #top_contributors <- site_data %>% 
  #  filter(temp_treatment == "warmed" | temp_treatment == "3.4") %>%
  #  mutate(abs_contribution_center = abs(contribution)) %>%
  #  arrange(desc(abs_contribution_center)) %>%
  #  slice(1:3)
  top_contributors <- site_data %>% 
    filter(top_contributors == "top_1" | top_contributors == "top_2" | top_contributors == "top_3") %>%
    filter(temp_treatment == "warmed" | temp_treatment == "3.4")
  
  # Determine arrows from ambient to warm
  arrow_data <- site_data %>%
    filter(temp_treatment %in% c("ambient", "warmed", "amb", "3.4")) %>% 
    filter(top_contributors %in% c("top_1", "top_2", "top_3")) %>%
    group_by(species) %>%
    filter(n() > 1) %>% # Make sure both treatments exist for each species/year
    arrange(temp_treatment) %>%
    summarize(
      x_start = temp_niche[temp_treatment %in% c("ambient", "amb")],
      y_start = slope[temp_treatment %in% c("ambient", "amb")],
      x_end = temp_niche[temp_treatment %in% c("warmed", "3.4")],
      y_end = slope[temp_treatment %in% c("warmed", "3.4")]
    )
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund)) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    geom_segment(data = arrow_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 color = "black", size = 0.5) +
    geom_point(data = site_data, aes(x = temp_niche, y = slope, color = temp_treatment,alpha = top_contributors, shape = top_contributors),size=3) +
    scale_alpha_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 1, "top_2" = 1, "top_3" = 1, "none" = 0.4),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_shape_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 8, "top_2" = 18, "top_3" = 17, "none" = 20),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)"
    ) +
    scale_color_manual(name = "Treatment",
                       values = used_colors,labels = used_labels) + # Use the dynamic color mapping
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggtitle(site_name) +
    labs(
      x = "Species temperature (°C)",
      y = expression(bold(Delta~Abundance)),
    ) +
    #geom_label_repel(data = top_contributors,
    #                 aes(x = temp_niche, y = slope, label = species),
    #                 size = 3,
    #                 box.padding = 0.5,          # Increase the padding around the box
    #                 point.padding = 0.3,   
    #                 fill = "white",             # Background color of the label
    #                 color = "black",
    #                 nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
    #                 nudge_y = 0.0001,  
    #                 min.segment.length = unit(0, 'lines'),
    #                 segment.color = 'grey50') +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14, face = "bold")) +
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
# Get shared legend information
point_contours_cfc <- plots_contours[[1]]
point_contours_hwrc <- plots_contours[[2]]
point_contours_jrgce <- plots_contours[[3]]
point_contours_ok <- plots_contours[[4]]
point_contours_phace <- plots_contours[[5]]
point_contours_tera <- plots_contours[[6]]

# Function to remove color, shape, and alpha guides
legend_rem <- function(plt){
  plt <- plt +
    guides(shape = "none", color = "none", alpha = "none")
  return(plt)   
}

# Getting the color, shape-alpha guide separately
clr_shp_lgnd <- point_contours_cfc +
  guides(fill = "none") +
  theme(axis.text.y = element_blank(),
        plot.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, 0.5),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "white", fill='white', size=1)) 

# Patchwork design
design <- "
  aaaaaabbbbbbcccccc##
  aaaaaabbbbbbccccccgg
  aaaaaabbbbbbccccccgg
  ddddddeeeeeeffffffgg
  ddddddeeeeeeffffffgg
  ddddddeeeeeeffffff##
"
# Combine plots
#png("cti_panel.png", units="in", width=15, height=7, res=300)
legend_rem(point_contours_cfc) + 
  legend_rem(point_contours_hwrc) +
  legend_rem(point_contours_jrgce) +
  legend_rem(point_contours_ok) +
  legend_rem(point_contours_phace) +
  legend_rem(point_contours_tera) +
  clr_shp_lgnd +
  plot_layout(design = design, axis_titles = "collect")
#dev.off()




### Point plot (raw niche data)
# Function to plot data for a given site
plot_site_contribution <- function(data, slope_data, site_name) {
  # Filter data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Check if the data is available for the specified site
  if (nrow(site_data) == 0) {
    stop("No data available for the specified site.")
  }
  
  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Point plot for the specified site
  p <- ggplot(site_data, aes(x = contribution, y = reorder(species, abs(contribution)), color = temp_niche)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_vline(xintercept = site_slope_CTI, linetype = "dashed") +
    geom_errorbar(aes(xmin = contribution-var, xmax = contribution+var), 
                  width =0.9, color = "black") +
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
plots_list <- list()

for (site_name in unique(data_for_plot$site)) {
  # Generate the plot for the current site
  plot <- plot_site_contribution(data_for_plot, slope_CTI_df, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_list[[site_name]] <- plot
}
plots_list[[5]]



### Point plot (mean-centered)
# Function to plot data for a given site
plot_site_contribution_center <- function(data, slope_data, site_name) {
  # Filter data for the specified site
  site_data <- data %>% filter(site == site_name)

  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Point plot for the specified site
  p <- ggplot(site_data, aes(x = contribution_center, y = reorder(species, abs(contribution_center)), color = temp_niche_center)) +
    geom_vline(xintercept = 0, linetype = "solid") +
    geom_vline(xintercept = site_slope_CTI, linetype = "dashed") +
    geom_segment(aes(x = 0, xend = slope, 
                     y = reorder(species, abs(contribution_center)), 
                     yend = reorder(species, abs(contribution_center))),
                 arrow = arrow(type = "closed", length = unit(0.075, "inches")),
                 color = "red") +
    geom_errorbar(aes(xmin = contribution_center-var_center,
                      xmax = contribution_center+var_center), 
                  width =0.9, color = "black") +
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



### Point plot w/ ellipse
# Plotting thermal niche vs slope
point_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Check if data is available for the specified site
  if (nrow(site_data) == 0) {
    stop("No data available for the specified site.")
  }
  
  # Plot
  p <- ggplot(site_data, aes(x = temp_niche_center, y = slope, color = temp_niche_center)) +
    geom_vline(xintercept = 0, size = 0.2, linetype = "solid", color = "black") + # Bold line at x = 0
    geom_hline(yintercept = 0, size = 0.2, linetype = "solid", color = "black") + # Bold median line
    geom_point() +
    stat_ellipse(level = 0.95, aes(group = 1), color = "black") + # Add an ellipse
    ggtitle(site_name) +
    xlab("Species temperature (°C)") +
    ylab(expression(bold(Delta~Abundance))) +
    scale_color_gradientn(colors = c("blue", "orangered"), name = "Species temperature (°C)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size=14,face="bold"),
          legend.position = "none")
  
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
point_ellipse_cfc <- plots_list_ellipse[[1]]
point_ellipse_hwrc <- plots_list_ellipse[[2]]
point_ellipse_jrgce <- plots_list_ellipse[[3]]
point_ellipse_ok <- plots_list_ellipse[[4]]
point_ellipse_phace <- plots_list_ellipse[[5]]
point_ellipse_tera <- plots_list_ellipse[[6]]



### Point plot w/ ellipse (plotly version)
# Plotting thermal niche vs slope with plotly
point_plot_plotly <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Using plotly to create the plot
  p <- plot_ly(data = site_data, 
               x = ~temp_niche_center, 
               y = ~slope, 
               type = 'scatter', 
               mode = 'markers', 
               color = ~temp_niche_center, 
               colors = c("blue", "orangered"),
               text = ~species) %>%
    layout(xaxis = list(title = 'Species temperature (°C)'),
           yaxis = list(title = 'Species change in abundance'))
  
  # Print or return the plot
  p
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_list_plotly <- list()

for (site_name in unique(data_for_plot$site)) {
  # Generate the plot for the current site
  plot <- point_plot_plotly(data_for_plot, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_list_plotly[[site_name]] <- plot
}
point_plotly_cfc <- plots_list_plotly[[1]]
point_plotly_hwrc <- plots_list_plotly[[2]]
point_plotly_jrgce <- plots_list_plotly[[3]]
point_plotly_ok <- plots_list_plotly[[4]]
point_plotly_phace <- plots_list_plotly[[5]]
point_plotly_tera <- plots_list_plotly[[6]]



### Scatter plot for mean-centered CTI
# Define the plotting function
scatter_func <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>%
    filter(site == site_name) %>%
    group_by(year, plot) %>%
    reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
    distinct()
  
  # Scatter plot
  p <- ggscatter(site_data, x = "year", y = "CTI", 
                 add = "reg.line", conf.int = TRUE, 
                 cor.coef = FALSE, cor.method = "pearson",
                 xlab = "Year", ylab = "CTI",title=site_name) +
    stat_regline_equation(
      aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~")))  
  
  # Return the plot
  return(p)
}
# Initialize an overall list for all plots for convenience
all_plots <- list()

# Define a vector of dataset names corresponding to elements in `final_list`
dataframe_names <- c("phace", "tera", "cfc", "hwrc", "ok", "jrgce")

# Loop through each dataframe in final_list
for (i in seq_along(final_list)) {
  data <- final_list[[i]]
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



### Bar plot
# Function to plot data for a given site
plot_site_contribution2 <- function(data, slope_data, site_name) {
  # Filter data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Check if the data is available for the specified site
  if (nrow(site_data) == 0) {
    stop("No data available for the specified site.")
  }
  
  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Bar plot for the specified site
  p <- ggplot(site_data, aes(x = reorder(species, -abs(contribution)), y = contribution, fill = temp_niche)) +
    geom_bar(stat = "identity") +
    xlab("Species") +
    ylab("Contribution to CTI") +
    scale_fill_gradientn(colors = c("blue", "orangered"), 
                         name = "Species temperature (°C)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(xlim = c(0.5, length(unique(site_data$species)) + 0.5)) +
    # Annotate the plot with the slope_CTI value
    geom_segment(aes(x = 0.5, xend = 0.5, y = 0, yend = site_slope_CTI),
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 color = "red") +
    theme(legend.text = element_text(size = 11)) +
    # Add dummy arrow for legend
    geom_segment(aes(x = Inf, y = -Inf, xend = Inf, yend = -Inf, color = "Delta CTI"),
                 arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                 linetype = "solid", size = 0.5, show.legend = TRUE) +
    guides(color = guide_legend(override.aes = list(linetype = "solid", size = 1, color = "red", alpha = 1))) +
    scale_color_manual(name = NULL, values = "red", labels = "Slope of CTI")
  
  # Print the plot
  print(p)
}
# Plot
plot_site_contribution2(data_for_plot, slope_CTI_df, "teracon")



### Vertical bar plot
# Function to plot data for a given site
plot_site_contribution3 <- function(data, slope_data, site_name) {
  # Filter data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Check if the data is available for the specified site
  if (nrow(site_data) == 0) {
    stop("No data available for the specified site.")
  }
  
  # Extract the slope_CTI for the site
  site_slope_CTI <- slope_data %>% filter(site == site_name) %>% pull(slope_CTI)
  
  # Create a data frame for the total CTI
  total_CTI_data <- data.frame(
    species = "Slope of CTI",
    contribution = site_slope_CTI,
    temp_niche = NA
  )
  
  # Combine the data
  combined_data <- bind_rows(site_data, total_CTI_data)
  
  # Order the species levels such that "Slope of CTI" is always last
  combined_data$species <- factor(
    combined_data$species,
    levels = c(
      # Use `unique()` to automatically handle species order and append "Slope of CTI" at the last
      "Slope of CTI",
      unique(combined_data$species[combined_data$species != "Slope of CTI"])[order(-abs(combined_data$contribution[combined_data$species != "Slope of CTI"]))]
    )
  )
  
  # Bar plot for the specified site
  p <- ggplot(combined_data, aes(x = contribution, y = species, fill = temp_niche)) +
    geom_hline(yintercept = 1.5, size = 0.5, linetype = "solid", color = "black") +
    geom_bar(stat = "identity") +
    xlab("Contribution to CTI") +
    ylab("Species") +
    scale_fill_gradientn(colors = c("blue", "orangered"), 
                         name = "Species temperature (°C)",
                         na.value = NA) +
    theme_minimal() +
    # Annotate the plot with the slope_CTI value
    geom_segment(aes(x = 0, xend = test_site_slope_CTI, y = 1, yend = 1),
                 arrow = arrow(type = "closed", length = unit(0.1, "inches")),
                 color = "red") +
    theme(legend.text = element_text(size = 11))
  # Add dummy arrow for legend
  #geom_segment(aes(x = Inf, y = -Inf, xend = Inf, yend = -Inf, color = "Slope of CTI"),
  #             arrow = arrow(type = "closed", length = unit(0.2, "inches")),
  #             linetype = "solid", size = 0.5, show.legend = TRUE) +
  #guides(color = guide_legend(override.aes = list(linetype = "solid", size = 1, color = "red", alpha = 1))) +
  #scale_color_manual(name = NULL, values = "red", labels = "Slope of CTI")
  
  # Print the plot
  print(p)
}
# Plot
plot_site_contribution3(data_for_plot, slope_CTI_df, "angelo")



### Export Rdata for plots
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(point_center_cfc, paste(path_out,'point_center_cfc.rds'))
saveRDS(point_center_hwrc, paste(path_out,'point_center_hwrc.rds'))
saveRDS(point_center_jrgce, paste(path_out,'point_center_jrgce.rds'))
saveRDS(point_center_ok, paste(path_out,'point_center_ok.rds'))
saveRDS(point_center_phace, paste(path_out,'point_center_phace.rds'))
saveRDS(point_center_tera, paste(path_out,'point_center_tera.rds'))

saveRDS(point_contours_cfc, paste(path_out,'point_contours_cfc.rds'))
saveRDS(point_contours_hwrc, paste(path_out,'point_contours_hwrc.rds'))
saveRDS(point_contours_jrgce, paste(path_out,'point_contours_jrgce.rds'))
saveRDS(point_contours_ok, paste(path_out,'point_contours_ok.rds'))
saveRDS(point_contours_phace, paste(path_out,'point_contours_phace.rds'))
saveRDS(point_contours_tera, paste(path_out,'point_contours_tera.rds'))

saveRDS(point_ellipse_cfc, paste(path_out,'point_ellipse_cfc.rds'))
saveRDS(point_ellipse_hwrc, paste(path_out,'point_ellipse_hwrc.rds'))
saveRDS(point_ellipse_jrgce, paste(path_out,'point_ellipse_jrgce.rds'))
saveRDS(point_ellipse_ok, paste(path_out,'point_ellipse_ok.rds'))
saveRDS(point_ellipse_phace, paste(path_out,'point_ellipse_phace.rds'))
saveRDS(point_ellipse_tera, paste(path_out,'point_ellipse_tera.rds'))

saveRDS(point_plotly_cfc, paste(path_out,'point_plotly_cfc.rds'))
saveRDS(point_plotly_hwrc, paste(path_out,'point_plotly_hwrc.rds'))
saveRDS(point_plotly_jrgce, paste(path_out,'point_plotly_jrgce.rds'))
saveRDS(point_plotly_ok, paste(path_out,'point_plotly_ok.rds'))
saveRDS(point_plotly_phace, paste(path_out,'point_plotly_phace.rds'))
saveRDS(point_plotly_tera, paste(path_out,'point_plotly_tera.rds'))

saveRDS(`B4Warmed CFC_plot`, paste(path_out,'scatter_cfc.rds'))
saveRDS(`B4Warmed HWRC_plot`, paste(path_out,'scatter_hwrc.rds'))
saveRDS(JRGCE_plot, paste(path_out,'scatter_jrgce.rds'))
saveRDS(Oklahoma_plot, paste(path_out,'scatter_ok.rds'))
saveRDS(PHACE_plot, paste(path_out,'scatter_phace.rds'))
saveRDS(TeRaCON_plot, paste(path_out,'scatter_tera.rds'))




