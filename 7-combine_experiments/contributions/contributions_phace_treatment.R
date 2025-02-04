# TITLE:          Calculating contribution to CTI for one site
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

# Combining phace abundance data with niche estimate data
full_abun_data_phace <- left_join(phace, niche_est_phace, by = "species")


# Calculating baseline CTI to use for measure of center 
calculate_initial_CTI <- function(data) {
  # Calculate the baseline CTI for the initial year
  initial_year <- min(data$year)
  
  # Calculate CTI baseline
  initial_cti <- data %>%
    filter(year == initial_year) %>%
    group_by(plot) %>%
    summarise(
      cti_baseline = sum(rel_abun * temp_niche) / sum(rel_abun)
    )
  
  return(initial_cti)
}
cti_initial <- calculate_initial_CTI(full_abun_data_phace)

# Merging with abundance data and centering the niche values
full_abun_data_phace <- left_join(full_abun_data_phace, cti_initial, by = "plot")
full_abun_data_phace <- full_abun_data_phace %>%
  mutate(temp_niche_center = temp_niche - cti_baseline)



# Calculating CTI
CTI <- full_abun_data_phace %>%
  group_by(year,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
  distinct() %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(CTI_diff = warmed - ambient)

# Matching abundance data w/ CTI data
abun <- full_abun_data_phace %>%
  group_by(year, species, temp_treatment) %>%
  summarize(rel_abun_avg = mean(rel_abun)) %>%
  pivot_wider(names_from = temp_treatment, values_from = rel_abun_avg) %>%
  mutate(rel_abun_diff = warmed - ambient)
niche_center <- full_abun_data_phace %>%
  group_by(species) %>%
  summarize(mean_niche = mean(temp_niche_center))
abun <- left_join(abun, niche_center, by = "species")


# Compute linear slopes using covariance and variance
# CTI
slope_CTI <- cov(CTI$year, CTI$CTI_diff) / var(CTI$year)

# Species
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
  value <- species_data$rel_abun_diff
  
  # Calculate the slope
  slope <- cov(time, value) / var(time)
  
  # Store the slope in the list
  slopes[[species]] <- slope
}



# Initialize a vector or list to store contributions
contributions <- numeric(length(slopes))
names(contributions) <- names(slopes)

# Calculate the contribution for each species
for (species in names(slopes)) {
  # Find the thermal niche for the current species
  niche_value <- abun$mean_niche[abun$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions[species] <- slopes[[species]] * niche_value
}

# Sum of contributions
slope_sum <- sum(contributions)



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



# Data for plotting
# Pull out species names and species slopes into their own lists
species_contribution <- unlist(contributions)
species_names <- names(contributions)
species_slopes <- unlist(slopes)
# Create a data frame for plotting
data_for_plot <- data.frame(
  species = c(species_names),
  contribution = c(species_contribution),
  slope = c(species_slopes)
)
# Merge with niche data
data_for_plot <- left_join(data_for_plot,niche_center, by = c("species"))
# Remove data whose absolute value for slope is < 0.0002
data_for_plot <- data_for_plot[abs(data_for_plot$contribution) > 0.002, ]



# Warming treatment effect over time
# This slope matches the calculated slope & sum of contributions
ggscatter(CTI, x = "year", y = "CTI_diff", 
          add = "reg.line", conf.int = T, 
          cor.coef = F, cor.method = "pearson",
          xlab = "Year", ylab = "CTI (warmed - ambient)",title="PHACE") +
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~"))
  )



# Bar plot
# Which species contribute to the treatment effect?
p <- ggplot(data_for_plot, aes(x = reorder(species, -abs(contribution)), y = contribution, fill = temp_niche)) +
  geom_bar(stat = "identity") +
  xlab("Species") +
  ylab("Contribution to CTI") +
  scale_fill_gradientn(colors = c("blue", "orangered"), 
                       name = "Species temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(xlim = c(0.5, length(unique(data_for_plot$species)) + 0.5)) +
  annotate("segment",
           x = 0.5, xend = 0.5,
           y = 0, yend = slope_CTI,
           arrow = arrow(type = "closed", length = unit(0.2, "inches")),
           color = "red") +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size=14),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14)) 
# Add dummy arrow for legend
p + geom_segment(aes(x = Inf, y = -Inf, xend = Inf, yend = -Inf, color = "Slope of CTI"),
                 arrow = arrow(type = "closed", length = unit(0.2, "inches")),
                 linetype = "solid", size = 0.5, show.legend = TRUE) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", size = 1, color = "red", alpha = 1))) +
  scale_color_manual(name = NULL, values = "red", labels = "Slope of CTI")



# Point plot
point <- ggplot(data_for_plot, aes(x=contribution, y = reorder(species, abs(contribution)), color=mean_niche)) +
  geom_segment(aes(x = 0, xend = slope, 
                   y = reorder(species, abs(contribution)), 
                   yend = reorder(species, abs(contribution))),
               arrow = arrow(type = "closed", length = unit(0.075, "inches")),
               color = "red") +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = slope_CTI, linetype = "dashed") +
  #geom_errorbar(aes(xmin = contribution-var_contribution_center, xmax = contribution_center+var_contribution_center), 
  #             width =0.9, color = "black") +
  geom_point(shape = 20, size = 5, position = position_dodge(width = 0.9)) +
  scale_color_gradientn(colors = c("blue", "orangered"), 
                        name = "Species temperature (°C)") +
  xlab("Contribution to CTI") +
  ylab("Species") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12,face="bold"))



# Abundance plot
ggplot(data_for_plot, aes(x = contribution, y = slope, color = temp_niche)) +
  geom_point(size=3) +
  scale_color_gradientn(colors = c("blue", "orangered"), name = "Species temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14))







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