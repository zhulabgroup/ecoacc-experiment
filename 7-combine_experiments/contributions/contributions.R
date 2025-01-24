# TITLE:          Calculating contribution to CTI
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    Species individual contributions to CTI
# PROJECT:        EcoAcc
# DATE:           Jan 2025



# Load packages
library(tidyverse)
library(stringr)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
niche_est_phace <- niche_est_phace %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
phace <- read.csv(" phace_clean.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)
# Load in data
niche_est_b4 <- read.csv(" b4warmed_niche.csv")
niche_est_b4 <- niche_est_b4 %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
b4 <- read.csv(" b4warmed_clean.csv")
# Separate dataframes for CFC and HWRC
b4_CFC <- b4 %>%
  filter(site == "CFC")
niche_est_CFC <- niche_est_b4 %>%
  filter(site == "CFC")
b4_HWRC <- b4 %>%
  filter(site == "HWRC")
niche_est_HWRC <- niche_est_b4 %>%
  filter(site == "HWRC")
# Making sure all years are present for all species
b4_CFC <- b4_CFC %>%
  complete(species, year = 2008:2021,
           plot = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "F1", "F2", "F3","F4",
                    "F5", "F6", "F7", "F8"),
           fill = list(rel_abun = 0)) %>%
  filter(!is.na(species))
b4_HWRC <- b4_HWRC %>%
  complete(species, year = 2008:2021,
           plot = c("J1", "J2", "J3", "J4", "J5", "J6", "J7", "J8", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8", "L1", "L2", "L3", "L4",
                    "L5", "L6", "L7", "L8"),
           fill = list(rel_abun = 0)) %>%
  filter(!is.na(species))

# Combining phace abundance data with niche estimate data
full_abun_data_phace <- left_join(phace, niche_est_phace, by = "species")
full_abun_data_cfc <- left_join(b4_CFC, niche_est_CFC, by = c("species"))
full_abun_data_hwrc <- left_join(b4_HWRC, niche_est_HWRC, by = c("species"))

# Put data into a list
dataframes_list <- list(full_abun_data_phace, full_abun_data_cfc, full_abun_data_hwrc)



# Calculating CTI
calculate_CTI <- function(data) {
  data %>%
    group_by(year, plot) %>%
    reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
    filter(!is.na(CTI)) %>%
    distinct()
}
cti_results <- lapply(dataframes_list, calculate_CTI)
#CTI <- full_abun_data_cfc %>%
#  group_by(year, plot) %>%
#  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
#  distinct()

# Matching abundance data w/ CTI data
prepare_abundance_data <- function(data) {
  data %>%
    select(year, plot, species, rel_abun, temp_niche) %>%
    filter(!is.na(rel_abun)) %>%
    filter(!is.na(species))
}
abun_list <- lapply(dataframes_list, prepare_abundance_data)
abun <- full_abun_data_cfc %>%
 select(year,plot,species,rel_abun,temp_niche) %>%
 filter(!is.na(rel_abun)) %>%
 filter(!is.na(species))
test <- abun %>%
  group_by(year,plot) %>%
  summarize(sum = sum(rel_abun))



# Compute linear slopes using covariance and variance
# CTI
calculate_CTI_slope <- function(data) {
  cov(data$year, data$CTI) / var(data$year)
}
CTI_slope_list <- lapply(cti_results, calculate_CTI_slope)
#slope_CTI <- cov(CTI$year, CTI$CTI) / var(CTI$year)

# Species
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
    value <- species_data$rel_abun
    
    # Calculate the slope
    slope <- cov(time, value) / var(time)
    
    # Store the slope in the list
    slopes[[species]] <- slope
  }
  
  return(slopes)
}
all_slopes <- lapply(abun_list, calculate_slopes)
# Initialize a list to store slopes
slopes <- list()
# Unique species names
unique_species <- unique(abun$species)
# Calculate the slope for each species
for (species in unique_species) {
  # Subset the data for the current species
  species_data <- abun[abun$species == species, ]
  
  # Extract time and corresponding values
  time <- species_data$year
  value <- species_data$rel_abun
  
  # Calculate the slope
  slope <- cov(time, value) / var(time)
  
  # Store the slope in the list
  slopes[[species]] <- slope
}



# Compute the contribution of each species to the slope of CTI
# Initialize a list to store the sum of contributions for each dataframe
slope_sums <- numeric(length(all_slopes))

# Iterate over each element in the all_slopes list
for (i in seq_along(all_slopes)) {
  
  # Extract slopes for current dataframe
  slopes <- all_slopes[[i]]
  
  # Determine the corresponding dataframe
  abun <- dataframes_list[[i]]
  
  # Initialize a vector to store contributions for this dataframe
  contributions <- numeric(length(slopes))
  
  # Set the names of contributions to match the species names
  names(contributions) <- names(slopes)
  
  # Calculate the contribution for each species
  for (species in names(slopes)) {
    
    # Find the average thermal niche value for the current species
    niche_values <- abun$temp_niche[abun$species == species]
    
    # Ensure the species has associated niche values
    if (length(niche_values) > 0) {
      mean_niche_value <- mean(niche_values, na.rm = TRUE)
      
      # Compute the contribution: slope * mean thermal niche
      contributions[species] <- slopes[[species]] * mean_niche_value
    } else {
      contributions[species] <- NA  # Assign NA if no thermal niche is found
    }
  }
  
  # Sum of contributions for the current dataframe
  slope_sum <- sum(contributions, na.rm = TRUE)
  
  # Store the result in slope_sums
  slope_sums[i] <- slope_sum
}
# Initialize a vector or list to store contributions
#contributions <- numeric(length(slopes))
#names(contributions) <- names(slopes)
#
## Calculate the contribution for each species
#for (species in names(slopes)) {
#  # Find the thermal niche for the current species
#  niche_value <- abun$temp_niche[abun$species == species]
#  
#  # Compute the contribution: slope * thermal niche
#  contributions[species] <- slopes[[species]] * niche_value
#}
#
## Sum of contributions
#slope_sum <- sum(contributions)


# Print values
# Initialize a list to store output strings
output_strings <- list()

# Add the slope of the CTI
output_strings[[1]] <- str_c("Slope of CTI: ", as.character(slope_CTI))

# Add each species' contribution
for (species in names(contributions)) {
  contribution_str <- str_c("Contribution of ", species, ": ", as.character(contributions[species]))
  output_strings <- c(output_strings, contribution_str)
}

# Add the sum of contributions
output_strings <- c(output_strings, str_c("Sum of contributions: ", as.character(slope_sum)))

# Print all output strings
writeLines(unlist(output_strings))



# Pull out species names and species slopes into their own lists
species_slopes <- unlist(slopes)
species_names <- names(slopes)
# Create a data frame for plotting
data_for_plot <- data.frame(
  Category = c(species_names),
  Slope = c(species_slopes),
  Type = c(rep("Species", length(species_names)))
)

# Merge with niche data
data_for_plot <- left_join(data_for_plot, niche_est, by = c("Category" = "species"))

# Remove data whose absolute value for slope is < 0.0002
data_for_plot <- data_for_plot[abs(data_for_plot$Slope) > 0.0002, ]



# Bar plot
p <- ggplot(data_for_plot, aes(x = reorder(Category, -abs(Slope)), y = Slope, fill = temp_niche)) +
  geom_bar(stat = "identity") +
  xlab("Species") +
  ylab(expression(Delta~Abundance)) +
  scale_fill_gradientn(colors = c("blue", "orangered"), 
                       name = "Species temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(xlim = c(0.5, length(unique(data_for_plot$Category)) + 0.5)) +
  annotate("segment",
           x = 0.5, xend = 0.5,
           y = 0, yend = slope_CTI,
           arrow = arrow(type = "closed", length = unit(0.1, "inches")),
           color = "red") +
  theme(legend.text = element_text(size = 11)) 
# Add dummy arrow for legend
p + geom_segment(aes(x = Inf, y = -Inf, xend = Inf, yend = -Inf, color = "Delta CTI"),
                 arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                 linetype = "solid", size = 0.5, show.legend = TRUE) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", size = 1, color = "red", alpha = 1))) +
  scale_color_manual(name = NULL, values = "red", labels = expression(Delta~CTI))



# Pie chart
# Reorder data based on temp_niche
data_for_plot_pie <- data_for_plot[order(data_for_plot$temp_niche), ]
# Create pie chart
ggplot(data_for_plot_pie, aes(x = factor(1, levels = 1), y = abs(Slope), fill = temp_niche)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  ylab("") +
  xlab("") +
  scale_fill_gradientn(colors = c("blue", "orangered"), 
                       name = "Species temperature (°C) with increasing order") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())



# Donut chart
ggplot(data_for_plot, aes(x = 2, y = abs(Slope), fill = Category)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y", start = 0) +
  ylab("") +
  xlab("") +
 # scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  xlim(0.5, 2.5) +  # Ensure space for the donut hole
  theme_void()
ggplot(data_for_plot, aes(x = 2, y = abs(Slope), fill = temp_niche)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  ylab("") +
  xlab("") +
  scale_fill_gradientn(colors = c("blue", "orangered"), name = "Thermal Niche (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  xlim(0.5, 2.5) +
  theme_void() +
  geom_text_repel(aes(label = Category), 
                  nudge_x = 1.5,
                  direction = "y",
                  hjust = 0.5,
                  segment.size = 0.2,
                  box.padding = 0.1,
                  size = 3)



# Plotting species slopes vs their thermal niches
ggplot(data_for_plot, aes(x = temp_niche, y = Slope, color = temp_niche)) +
  geom_point() +
  xlab("Species temperature (°C)") +
  ylab(expression(Delta~Abundance)) +
  scale_color_gradientn(colors = c("blue", "orangered"), name = "Species temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none")
