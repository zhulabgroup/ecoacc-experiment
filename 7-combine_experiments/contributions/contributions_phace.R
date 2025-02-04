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
library(ggtree)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
niche_est_phace <- niche_est_phace %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
phace <- read.csv(" phace_clean.csv")
phylo <- readRDS(" phace_phylo_tree.rds")

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
  group_by(year, plot) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun)) %>%
  distinct()
CTI_center <- full_abun_data_phace %>%
  group_by(year, plot) %>%
  reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
  distinct()

# Matching abundance data w/ CTI data
abun <- full_abun_data_phace %>%
  dplyr::select(year,plot,species,rel_abun,temp_niche) %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(species))
abun_center <- full_abun_data_phace %>%
  dplyr::select(year,plot,species,rel_abun,temp_niche_center) %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(species))


### Compute linear slopes using covariance and variance
# Note: the slope values stay the same whether using mean-centered data or not
# CTI
slope_CTI <- cov(CTI$year, CTI$CTI) / var(CTI$year)

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
  value <- species_data$rel_abun
  
  # Calculate the slope
  slope <- cov(time, value) / var(time)
  
  # Store the slope in the list
  slopes[[species]] <- slope
}



### Calculate the variance using residuals
# CTI
resid_CTI <- residuals(lm(CTI$CTI ~ CTI$year))
var_CTI <- sum(resid_CTI^2) / (length(CTI$CTI) - 2) / sum((CTI$year - mean(CTI$year))^2)

# Species
# Initialize a list to store slopes
variances <- list()
# Unique species names
unique_species <- unique(abun$species)
# Calculate the slope for each species
for (species in unique_species) {
  # Subset the data for the current species
  species_data <- abun[abun$species == species, ]
  
  # Extract time and corresponding values
  time <- species_data$year
  value <- species_data$rel_abun
  
  # Calculate the variance
  resid <- residuals(lm(value ~ time))
  var <- sum(resid^2) / (length(value) - 2) / sum((time - mean(time))^2)
  
  # Store the slope in the list
  variances[[species]] <- var
}

N <- length(CTI$year)



### Slope contributions
## Raw niche values
# Initialize a vector or list to store contributions
contributions <- numeric(length(slopes))
names(contributions) <- names(slopes)

# Calculate the contribution for each species
for (species in names(slopes)) {
  # Find the thermal niche for the current species
  niche_value <- abun$temp_niche[abun$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions[species] <- slopes[[species]] * niche_value
}

# Sum of contributions
slope_sum <- sum(contributions)

## Mean-centered
# Initialize a vector or list to store contributions
contributions_center <- numeric(length(slopes))
names(contributions_center) <- names(slopes)

# Calculate the contribution for each species
for (species in names(slopes)) {
  # Find the thermal niche for the current species
  niche_value <- abun_center$temp_niche_center[abun_center$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions_center[species] <- slopes[[species]] * niche_value
}

# Sum of contributions
slope_sum_center <- sum(contributions_center)



### Variance contributions
## Raw temp niche values
# Initialize a vector or list to store contributions
contributions_var <- numeric(length(variances))
names(contributions_var) <- names(variances)

# Calculate the contribution for each species
for (species in names(variances)) {
  # Find the thermal niche for the current species
  niche_value <- abun$temp_niche[abun$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions_var[species] <- variances[[species]] * niche_value^2
}

# Sum of contributions
variances_sum <- sum(contributions_var)


## Mean-centered values
# Initialize a vector or list to store contributions
contributions_var_center <- numeric(length(variances))
names(contributions_var_center) <- names(variances)

# Calculate the contribution for each species
for (species in names(variances)) {
  # Find the thermal niche for the current species
  niche_value <- abun_center$temp_niche_center[abun_center$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions_var_center[species] <- variances[[species]] * niche_value^2
}

# Sum of contributions
variances_sum_center <- sum(contributions_var_center)



### Print values for slopes
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



### Print values for variances
# Initialize a list to store output strings
output_strings_var <- list()

# Add the slope of the CTI
output_strings_var[[1]] <- str_c("Variance of CTI: ", as.character(var_CTI))

# Add each species' contribution
for (species in names(contributions_var)) {
  contribution_var_str <- str_c("Contribution of ", species, ": ", as.character(contributions_var[species]))
  output_strings_var <- c(output_strings_var, contribution_var_str)
}

# Add the sum of contributions
output_strings_var <- c(output_strings_var, str_c("Sum of variance contributions: ", as.character(variances_sum)))

# Print all output strings
writeLines(unlist(output_strings_var))



### Data for plotting
# Pull out species names and species slopes into their own lists
species_contribution <- unlist(contributions)
species_contribution_var <- unlist(contributions_var)
species_contribution_center <- unlist(contributions_center)
species_contribution_var_center <- unlist(contributions_var_center)
species_names <- names(contributions)
species_slopes <- unlist(slopes)
species_var <- unlist(variances)

# Create a data frame for plotting
data_for_plot <- data.frame(
  species = c(species_names),
  contribution = c(species_contribution),
  contribution_center = c(species_contribution_center),
  var_contribution = c(species_contribution_var),
  var_contribution_center = c(species_contribution_var_center),
  slope = c(species_slopes),
  var = c(species_var)
)

# Merge with niche data
niches <- abun_center %>%
  dplyr::select(species, temp_niche_center) %>%
  group_by(species) %>%
  summarize(mean_niche = mean(temp_niche_center))
niches2 <- abun %>%
  dplyr::select(species, temp_niche) %>%
  distinct()
data_for_plot <- left_join(data_for_plot, niches, by = c("species"))
data_for_plot <- left_join(data_for_plot, niches2, by = c("species"))

# Remove data whose absolute value for slope is < 0.0002
#data_for_plot <- data_for_plot[abs(data_for_plot$contribution) > 0.002, ]

# Save data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
saveRDS(data_for_plot, paste(path_out,file = "phace_contribution.rds"))


# Phylo tree
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
data_for_plot$species <- factor(data_for_plot$species, levels = tip_order)
point_plot <- ggplot(data_for_plot, aes(x=contribution_center, y = species, color=mean_niche)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = slope_CTI, linetype = "dashed") +
  geom_errorbar(aes(xmin = contribution_center-var_contribution_center,
                    xmax = contribution_center+var_contribution_center), 
                width =0.9, color = "black") +
  geom_point(shape = 20, size = 5, position = position_dodge(width = 0.9)) +
  scale_color_gradientn(colors = c("blue", "orangered"), 
                        name = "Species temperature (°C)") +
  xlab("Contribution to CTI") +
  ylab(NULL) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_blank(),
        axis.title = element_text(size=14,face="bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12,face="bold"),
        panel.grid.major.y = element_line())

# Combined point and tree plot
ggarrange(tree2, point_plot, nrow = 1,align = "h")



# Scatter plot for mean-centered CTI
CTI_center2 <- full_abun_data_phace%>%
  group_by(year, plot,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
  distinct()
phace_scatter2 <- ggscatter(CTI_center2, x = "year", y = "CTI", 
                           color = "temp_treatment",add = "reg.line", conf.int = T, 
                           cor.coef = F, cor.method = "pearson",
                           xlab = "Year", ylab = "CTI",title="PHACE") +
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~"))) +
  scale_color_manual(labels = c("ambient","warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(labels = c("ambient","warmed"),
                    values = c("blue","red"))


# Regular point plot
ggplot(data_for_plot, aes(x=contribution, y = reorder(species, abs(contribution)), color=temp_niche)) +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = slope_CTI, linetype = "dashed") +
  geom_errorbar(aes(xmin = contribution-var_contribution, xmax = contribution+var_contribution), 
                width =0.9, color = "black") +
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



# Bar plot
p <- ggplot(data_for_plot, aes(x = reorder(species, -abs(contribution)), y = contribution, fill = temp_niche)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=contribution-var_contribution, ymax=contribution+var_contribution), width=.2,
                position=position_dodge(.9)) + 
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
ggplot(data_for_plot, aes(x = contribution, y = temp_niche, color = temp_niche)) +
  geom_vline(xintercept = 0, size = 0.2, linetype = "solid", color = "black") + # Bold line at x = 0
  geom_hline(yintercept = median(data_for_plot$temp_niche, na.rm = TRUE), size = 0.2, linetype = "solid", color = "black") + # Bold median line
  geom_point(size=3) +
  stat_ellipse(level = 0.95, aes(group = 1), color = "black") + # Add an ellipse
  xlab("Contribution to CTI") +
  ylab("Species temperature (°C)") +
  scale_color_gradientn(colors = c("blue", "orangered"), name = "Species temperature (°C)") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))



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



# Plotting species slopes vs their thermal niches
ggplot(data_for_plot, aes(x = temp_niche, y = Slope, color = temp_niche)) +
  geom_point() +
  xlab("Species temperature (°C)") +
  ylab(expression(Delta~Abundance)) +
  scale_color_gradientn(colors = c("blue", "orangered"), name = "Species temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "none")