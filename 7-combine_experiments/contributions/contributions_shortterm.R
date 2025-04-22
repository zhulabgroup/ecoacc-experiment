# TITLE:          Calculating contribution to CTI short-term
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for phace
# DATA OUTPUT:    Species individual contributions to CTI
# PROJECT:        EcoAcc
# DATE:           Jan 2025



### Load packages
library(tidyverse)
library(stringr)
library(data.table)

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
tera <- tera %>%
  dplyr::select(-mean_C_temp_summer)

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
jrgce <- jrgce %>%
  dplyr::select(-mean_C_temp_summer)



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

##Merging with treatment info
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
# Using mean-centered niche values
calculate_CTI_center <- function(data) {
  data %>%
    group_by(year, plot, temp_treatment) %>%
    reframe(CTI = sum(rel_abund * temp_niche) / sum(rel_abund)) %>%
    distinct()
}
cti_results_center <- lapply(final_list, calculate_CTI_center)
# Combine all data frames into a single data frame
cti_df <- rbindlist(cti_results_center, idcol = "site")



### Calculate contributions
# Initialize an empty list to store the results
result_list <- list()

# Iterate over each element in final_list
for (i in seq_along(final_list)) {
  
  # Access the current dataframe
  abun <- final_list[[i]]
  
  # Perform operations on the dataframe
  res <- abun %>%
    group_by(species, year, plot, temp_treatment) %>%
    mutate(contribution_center = rel_abund * temp_niche)
  
  # Store the resulting dataframe in result_list
  result_list[[i]] <- res
}
names(result_list) <- names(final_list)
# Combine all data frames into a single data frame
res_df <- rbindlist(result_list, idcol = "site1")



### Data for plotting
# Taking average CTI and average spp contribution per year and temp treatment (averaging over plot)
# Note: avg sum of contributions still equals avg cti after averaging
# Also note: the 'test' calculation below tests to see if the above calculated contributions match 'test' after we took the average
cti_avg <- cti_df %>%
  group_by(site,year,temp_treatment) %>%
  summarize(avg_cti = mean(CTI))
cont_avg <- res_df %>%
  group_by(site,species,year,temp_treatment) %>%
  summarize(avg_cont = mean(contribution_center),
            avg_abun = mean(rel_abund),
            avg_temp_niche = mean(temp_niche)) %>%
  mutate(test = avg_abun * avg_temp_niche)



### Adding in top contributor designations (determined in contributions_treatment.R)
### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
# Load in data
top_contributors <- read.csv(" top_contributors.csv")
# Merge with data
cont_avg <- left_join(cont_avg, top_contributors, by = c("site","species"))




### Contour plot
contour_plot <- function(data, site_name) {
  # Filter the data for the specified site
  site_data <- data %>% filter(site == site_name)
  
  # Pulling out ranges of temp niches and slopes
  temp <- seq(min(site_data$avg_temp_niche)-0.1, max(site_data$avg_temp_niche)+0.1, length.out = 50)
  abun <- seq(min(site_data$avg_abun), max(site_data$avg_abun)+0.02, length.out = 50)
  
  # Merge data into dataframe
  grid_df <- expand.grid(temp_anomaly = temp, abund = abun)
  grid_df <- grid_df %>%
    mutate(spp_contrib = temp_anomaly * abund)
  
  # Get unique temp_treatment categories
  unique_treatments <- unique(site_data$temp_treatment)
  
  # Define the color mapping based on available treatments
  treatment_colors <- c("ambient" = "blue4", "1.7" = "orange3", "warmed" = "red4", "3.4" = "red4", "amb" = "blue4")
  labels = c("ambient" = "Ambient","warmed" = "Warmed","1.7" = "Intermediate", "3.4" = "Warmed", "amb" = "Ambient")
  used_colors <- treatment_colors[names(treatment_colors) %in% unique_treatments]
  used_labels <- labels[names(labels) %in% unique_treatments]
  
  # Top species for name labels per year
  top_contributors <- site_data %>% 
    group_by(year, temp_treatment) %>%
    mutate(abs_contribution_center = abs(avg_cont)) %>%
    arrange(desc(abs_contribution_center)) %>%
    slice(1)
  
  # Create a mapping from top_contributors to species names with labels "Top X: Species"
  initial_year <- min(site_data$year)
  treatment_level <- min(site_data$temp_treatment)
  
  species_mapping <- site_data %>%
    filter(!(top_contributors == "none")) %>%
    filter(year == initial_year) %>%
    filter(temp_treatment == treatment_level) %>%
    dplyr::select(species, top_contributors) %>%
    arrange(top_contributors) %>%
    ungroup() %>%  # Ensure no grouping before mutating
    mutate(rank = row_number()) %>%
    mutate(label = paste0(rank, ": ", species)) %>%
    dplyr::select(top_contributors, label) %>%
    distinct() %>%
    deframe()
  
  # Determine arrows from ambient to warm
  arrow_data <- site_data %>%
    filter(temp_treatment %in% c("ambient", "warmed", "amb", "3.4")) %>% 
    filter(top_contributors %in% c("top_1", "top_2", "top_3")) %>%
    group_by(species, year) %>%
    filter(n() > 1) %>% # Make sure both treatments exist for each species/year
    summarize(
      x_start = avg_temp_niche[temp_treatment %in% c("ambient", "amb")],
      y_start = avg_abun[temp_treatment %in% c("ambient", "amb")],
      x_end = avg_temp_niche[temp_treatment %in% c("warmed", "3.4")],
      y_end = avg_abun[temp_treatment %in% c("warmed", "3.4")]
    )
  
  # Plot
  p <- ggplot(grid_df, aes(x = temp_anomaly, y = abund)) +
    facet_wrap(.~year) +
    geom_tile(aes(fill = spp_contrib)) + # Add tiles to represent the surface
    #geom_contour(aes(z = spp_contrib), color = "gray50") + # Specified contour lines
    geom_segment(data = arrow_data, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
                 color = "black", size = 0.5) +
    geom_point(data = site_data, aes(x = avg_temp_niche, y = avg_abun, color = temp_treatment, alpha = top_contributors, shape = top_contributors, size=top_contributors)) + #########
    scale_alpha_manual(name = "Top species\ncontributors",
                     values = c("top_1" = 1, "top_2" = 1, "top_3" = 1, "none" = 0.4),
                     labels = c(species_mapping, "none" = "N/A")) +
    scale_shape_manual(name = "Top species\ncontributors",
                       values = c("top_1" = 17, "top_2" = 18, "top_3" = 8, "none" = 20),
                       labels = c(species_mapping, "none" = "N/A")) +
    scale_size_manual(name = "Top species\ncontributors",
                      values = c("top_1" = 3, "top_2" = 3, "top_3" = 3, "none" = 1),
                      labels = c(species_mapping, "none" = "N/A")) +
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
      #y = paste(site_name,"Abundance", sep = "\n"),
      y = "Abundance",
    ) +
    #geom_label_repel(data = top_contributors,
    #                 aes(x = avg_temp_niche, y = avg_abun, label = species),
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
          axis.title = element_text(size = 15, face = "bold"),
          strip.text.x = element_text(size = 12),
          plot.title = element_text(size=15, face="bold"),
          #axis.title.x = element_blank(),
          legend.text=element_text(size=13),
          legend.title=element_text(size=13)) +
    coord_cartesian(expand = FALSE) + # Ensure no expansion on axes
    guides(shape = guide_legend(order = 1),
           size = guide_legend(order = 1),
           alpha = guide_legend(order = 1))
  
  # Print the plot
  print(p)
}
# Loop through each site and store the output plot
# Initialize an empty list to store the plots
plots_contours <- list()

for (site_name in unique(cont_avg$site)) {
  # Generate the plot for the current site
  plot <- contour_plot(cont_avg, site_name)
  
  # Store the plot in the list with the site name as the key
  plots_contours[[site_name]] <- plot
}
contour_shortterm_cfc <- plots_contours[[1]] + theme(axis.title.x = element_blank())
contour_shortterm_hwrc <- plots_contours[[2]]
contour_shortterm_jrgce <- plots_contours[[3]] + theme(axis.title.x = element_blank()) + scale_x_continuous(breaks=seq(11, 17, 2))
contour_shortterm_ok <- plots_contours[[4]]+ theme(axis.title.x = element_blank())
contour_shortterm_phace <- plots_contours[[5]]+ theme(axis.title.x = element_blank())
contour_shortterm_tera <- plots_contours[[6]]+ theme(axis.title.x = element_blank())

png("contours_4.png", units="in", width=18, height=18, res=300)
wrap_plots(contour_shortterm_jrgce, contour_shortterm_phace, contour_shortterm_tera,
           contour_shortterm_ok, contour_shortterm_hwrc, contour_shortterm_cfc,
           ncol=2,axis_titles = "collect")
dev.off()


### Export Rdata for plots
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(contour_shortterm_cfc, paste(path_out,'contours_shortterm_cfc.rds'))
saveRDS(contour_shortterm_hwrc, paste(path_out,'contours_shortterm_hwrc.rds'))
saveRDS(contour_shortterm_jrgce, paste(path_out,'contours_shortterm_jrgce.rds'))
saveRDS(contour_shortterm_ok, paste(path_out,'contours_shortterm_ok.rds'))
saveRDS(contour_shortterm_phace, paste(path_out,'contours_shortterm_phace.rds'))
saveRDS(contour_shortterm_tera, paste(path_out,'contours_shortterm_tera.rds'))





