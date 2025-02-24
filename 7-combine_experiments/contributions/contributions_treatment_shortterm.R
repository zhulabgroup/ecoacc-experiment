# TITLE:          Calculating contribution to CTI (warming effect) short-term
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
library(data.table)

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
niche_est_phace <- niche_est_phace %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
phace <- read.csv(" phace_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
niche_est_tera <- read.csv(" teracon_niche.csv")
niche_est_tera <- niche_est_tera %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
tera <- read.csv(" teracon_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
niche_est_ok <- read.csv(" ok_niche.csv")
niche_est_ok <- niche_est_ok %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
ok <- read.csv(" ok_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
niche_est_jrgce <- niche_est_jrgce %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
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
#full_abun_data_cfc <- full_abun_data_cfc %>%
#  filter(!(temp_treatment == 1.7)) %>%
#  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))
#full_abun_data_hwrc <- full_abun_data_hwrc %>%
#  filter(!(temp_treatment == 1.7)) %>%
#  mutate(temp_treatment = if_else(str_detect(temp_treatment, "3.4"), "warmed", "ambient"))

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
# Using mean-centered niche values
calculate_CTI_center <- function(data) {
  # Calculate the mean CTI for each treatment and year
  cti_summary <- data %>%
    group_by(year, temp_treatment) %>%
    summarize(mean_cti = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
    pivot_wider(names_from = temp_treatment, values_from = mean_cti)
  
  # Initialize an empty data frame to store results
  cti_diffs <- data.frame(year = cti_summary$year)
  
  # Calculate CTI_diff for warmed - ambient if those treatments exist
  if (all(c("warmed", "ambient") %in% names(cti_summary))) {
    cti_diffs$CTI_diff <- cti_summary$warmed - cti_summary$ambient
  }
  
  # Calculate CTI_diff for 3.4 - amb if those treatments exist
  if (all(c("3.4", "amb") %in% names(cti_summary))) {
    cti_diffs$CTI_diff_3.4 <- cti_summary$`3.4` - cti_summary$amb
  }
  
  # Calculate CTI_diff for 1.7 - amb if those treatments exist
  if (all(c("1.7", "amb") %in% names(cti_summary))) {
    cti_diffs$CTI_diff_1.7 <- cti_summary$`1.7` - cti_summary$amb
  }
  
  return(cti_diffs)
}

# Apply the function to your list of data frames
cti_results_center <- lapply(centered_list, calculate_CTI_center)

### Matching abundance data w/ CTI data
abun_calc <- function(data) {
  # Calculate average relative abundance for each group
  abun_summary <- data %>%
    group_by(site, year, species, temp_niche_center, temp_treatment) %>%
    summarize(rel_abun_avg = mean(rel_abun, na.rm = TRUE)) %>%
    pivot_wider(names_from = temp_treatment, values_from = rel_abun_avg)
  
  # Initialize a result data frame
  abun_diffs <- abun_summary %>%
    select(site, year, species, temp_niche_center)
  
  # Calculate abundance differences based on available treatments
  if (all(c("warmed", "ambient") %in% names(abun_summary))) {
    abun_diffs$rel_abun_diff <- abun_summary$warmed - abun_summary$ambient
  }
  
  if (all(c("3.4", "amb") %in% names(abun_summary))) {
    abun_diffs$rel_abun_diff_3.4 <- abun_summary$`3.4` - abun_summary$amb
  }
  
  if (all(c("1.7", "amb") %in% names(abun_summary))) {
    abun_diffs$rel_abun_diff_1.7 <- abun_summary$`1.7` - abun_summary$amb
  }
  
  return(abun_diffs)
}

# Apply the function to your list of dataframes
final_list <- lapply(centered_list, abun_calc)



### Calculate contributions
# Initialize an empty list to store the results
result_list <- list()

# Iterate over each element in final_list
for (i in seq_along(final_list)) {
  
  # Access the current dataframe
  abun <- final_list[[i]]
  
  # Check for the presence of specific columns and perform corresponding operations
  res <- abun %>%
    group_by(species, year)
  
  # Conditional operations based on the presence of certain columns
  if ("rel_abun_diff" %in% names(abun)) {
    res <- res %>%
      mutate(contribution_center = rel_abun_diff * temp_niche_center)
  }
  
  if (all(c("rel_abun_diff_3.4", "rel_abun_diff_1.7") %in% names(abun))) {
    res <- res %>%
      mutate(contribution_center_3.4 = rel_abun_diff_3.4 * temp_niche_center,
             contribution_center_1.7 = rel_abun_diff_1.7 * temp_niche_center)
  }
  
  # Store the resulting dataframe in result_list
  result_list[[i]] <- res
}

# Name each element in result_list after the corresponding element in final_list
names(result_list) <- names(final_list)

# Combine all data frames & b4warmed dataframes into separate dataframes
res_df <- rbindlist(result_list[c(1:2,5:6)], idcol = "site1")
res_df_b4 <- rbindlist(result_list[3:4], idcol = "site1")


## Fix spp name
res_df$species <- recode(res_df$species, "solidago drummondii" = "Solidago drummondii")
res_df$species <- recode(res_df$species, "Andropogon gerardi" = "Andropogon gerardii")

### Adding in top contributor designations (determined in contributions_treatment.R)
### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
# Load in data
top_contributors <- read.csv(" top_contributors.csv")
# Merge with data
res_df <- left_join(res_df, top_contributors, by = c("site","species"))
res_df_b4 <- left_join(res_df_b4, top_contributors, by = c("site","species"))




### Contour plot
contour_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min(site_data$temp_niche_center), max(site_data$temp_niche_center), length.out = 50)
  abun <- seq(min(site_data$rel_abun_diff), max(site_data$rel_abun_diff), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund = abun)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly * abund)
  
  # Top species for name labels per year
  top_contributors <- site_data %>% 
    group_by(year) %>%
    mutate(abs_contribution_center = abs(contribution_center)) %>%
    arrange(desc(abs_contribution_center)) %>%
    slice(1)
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund)) +
    facet_wrap(.~year) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    geom_point(data = site_data, aes(x = temp_niche_center, y = rel_abun_diff, shape=top_contributors, alpha=top_contributors)) +
    scale_alpha_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 1, "top_2" = 1, "top_3" = 1, "none" = 0.4),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_shape_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 8, "top_2" = 18, "top_3" = 17, "none" = 20),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)\n(Warmed - Ambient)"
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggtitle(site_name) +
    labs(
      x = "Species temperature anomaly (°C)",
      y = "Abundance (Warmed - Ambient)",
    ) +
    #geom_label_repel(data = top_contributors,
    #                 aes(x = temp_niche_center, y = rel_abun_diff, label = species),
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

for (site_name in unique(res_df$site)) {
  # Generate the plot for the current site
  plot <- contour_plot(res_df, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_contours[[site_name]] <- plot
}
contour_treat_shortterm_phace <- plots_contours[[1]]
contour_treat_shortterm_tera <- plots_contours[[2]]
contour_treat_shortterm_ok <- plots_contours[[3]]
contour_treat_shortterm_jrgce <- plots_contours[[4]]



### Contour plot for b4warmed
contour_plot_b4 <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min(site_data$temp_niche_center), max(site_data$temp_niche_center), length.out = 50)
  abun_3.4 <- seq(min(site_data$rel_abun_diff_3.4), max(site_data$rel_abun_diff_3.4), length.out = 50)
  abun_1.7 <- seq(min(site_data$rel_abun_diff_1.7), max(site_data$rel_abun_diff_1.7), length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund_3.4 = abun_3.4)
  grid_df <- grid_df %>%
    mutate(spp_contrib_3.4 = temp_anomaly * abund_3.4)
  
  # Top species for name labels per year
  top_contributors <- site_data %>% 
    group_by(year) %>%
    mutate(abs_contribution_center_3.4 = abs(contribution_center_3.4)) %>%
    arrange(desc(abs_contribution_center_3.4)) %>%
    slice(1)
  top_contributors2 <- site_data %>% 
    group_by(year) %>%
    mutate(abs_contribution_center_1.7 = abs(contribution_center_1.7)) %>%
    arrange(desc(abs_contribution_center_1.7)) %>%
    slice(1)
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund_3.4)) +
    facet_wrap(.~year) +
    geom_tile(aes(fill = spp_contrib_3.4)) + # Add tiles to represent the surface
    geom_contour(aes(z = spp_contrib_3.4), color = "gray50") + # Specified contour lines
    geom_point(data = site_data, aes(x = temp_niche_center, y = rel_abun_diff_3.4,alpha=top_contributors,shape=top_contributors),color="red") +
    geom_point(data = site_data, aes(x = temp_niche_center, y = rel_abun_diff_1.7,alpha=top_contributors,shape=top_contributors),color="orange") +
    scale_alpha_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 1, "top_2" = 1, "top_3" = 1, "none" = 0.4),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_shape_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 8, "top_2" = 18, "top_3" = 17, "none" = 20),
                       labels = c("top_1" = "Top 1", "top_2" = "Top 2", "top_3" = "Top 3", "none" = "N/A")) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red", midpoint = 0,
      name = "Species\ncontribution\nto CTI (°C)\n(Warmed - Ambient)"
    ) +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggtitle(site_name) +
    labs(
      x = "Species temperature anomaly (°C)",
      y = "Abundance (Warmed - Ambient)",
    ) +
    #geom_label_repel(data = top_contributors,
    #                 aes(x = temp_niche_center, y = rel_abun_diff_3.4, label = species),
    #                 size = 3,
    #                 box.padding = 0.5,          # Increase the padding around the box
    #                 point.padding = 0.3,   
    #                 fill = "white",             # Background color of the label
    #                 color = "black",
    #                 nudge_x = 0.1,          # Slightly nudge labels in the x-direction if needed
    #                 nudge_y = 0.0001,  
    #                 min.segment.length = unit(0, 'lines'),
    #                 segment.color = 'grey50') +
    #geom_label_repel(data = top_contributors2,
    #                 aes(x = temp_niche_center, y = rel_abun_diff_1.7, label = species),
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
plots_contours_b4 <- list()

for (site_name in unique(res_df_b4$site)) {
  # Generate the plot for the current site
  plot <- contour_plot_b4(res_df_b4, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_contours_b4[[site_name]] <- plot
}
contour_treat_shortterm_cfc <- plots_contours_b4[[1]]
contour_treat_shortterm_hwrc <- plots_contours_b4[[2]]




### Contributions over time
temporal_cont <- function(data, cti, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  cti_data <- cti[[site_name]]
  
  p <- ggplot() +
    geom_point(data=cti_data,aes(x = year, y = CTI_diff),color="red",shape=18,size=3) +
    geom_line(data=cti_data,aes(x = year, y = CTI_diff),color="red") +
    geom_point(data = site_data, aes(x = year, y = contribution_center, color = species)) +
    geom_line(data = site_data, aes(x = year, y = contribution_center, color = species)) +
    ggtitle(site_name) +
    scale_color_viridis(discrete = TRUE, option = "D")+
    labs(
      x = "Year",
      y = "Species contribution to CTI\n(Warmed - Ambient)",
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_temp <- list()

for (site_name in unique(res_df$site)) {
  # Generate the plot for the current site
  plot <- temporal_cont(res_df, cti_results_center,site_name)
  
  # Store the plot in the list with the site name as the key
  plots_temp[[site_name]] <- plot
}
contour_treat_shortterm_phace <- plots_temp[[1]]
contour_treat_shortterm_tera <- plots_temp[[2]]
contour_treat_shortterm_ok <- plots_temp[[3]]
contour_treat_shortterm_jrgce <- plots_temp[[4]]



### Export Rdata for plots
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(contour_treat_shortterm_phace, paste(path_out,'contours_treat_shortterm_phace.rds'))
saveRDS(contour_treat_shortterm_tera, paste(path_out,'contours_treat_shortterm_tera.rds'))
saveRDS(contour_treat_shortterm_cfc, paste(path_out,'contours_treat_shortterm_cfc.rds'))
saveRDS(contour_treat_shortterm_hwrc, paste(path_out,'contours_treat_shortterm_hwrc.rds'))
saveRDS(contour_treat_shortterm_ok, paste(path_out,'contours_treat_shortterm_ok.rds'))
saveRDS(contour_treat_shortterm_jrgce, paste(path_out,'contours_treat_shortterm_jrgce.rds'))


