# TITLE:          Calculating contribution to CTI for biocon
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for biocon
# DATA OUTPUT:    Species individual contributions to CTI
# PROJECT:        EcoAcc
# DATE:           Feb 2025



### Load packages
library(tidyverse)
library(stringr)

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
niche_est <- read.csv(" teracon_niche.csv") # using ecoregion 8 niche estimates
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
biocon <- read.csv(" biocon_clean.csv")
teracon <- read.csv(" teracon_clean.csv")

### Combining biocon abundance data with niche estimate data
full_abun_data <- left_join(biocon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "elv"), "warmed", "ambient"))

### Subsetting to plots used in TeRaCON
teracon_plots <- unique(teracon$plot)
biocon_plots <- full_abun_data %>%
  filter(plot %in% teracon_plots) %>%
  filter(Season == "August")

### Selecting each plot's treatment designation for each experiment
biocon_treat <- biocon_plots %>%
  select(plot, temp_treatment) %>%
  distinct()

### Calculating relative abundance
biocon_plots <- biocon_plots %>%
  filter(!is.na(temp_niche)) %>%
  group_by(year, plot) %>%
  mutate(total_cover = sum(percent_cover,na.rm=T)) %>%
  mutate(rel_abun = percent_cover / total_cover) %>%
  filter(!is.na(rel_abun) & rel_abun != "NaN") %>%
  ungroup()

### Making sure all years are present for all species
complete_biocon <- biocon_plots %>%
  complete(species, year = 1998:2023,
           plot = c(7, 11, 17, 21, 26, 34, 48, 50, 66, 84, 87, 92, 97, 106, 117, 119, 128, 131,
                    133, 149, 165, 166, 171, 177, 185, 186, 224, 226, 230, 232, 233, 243, 245,
                    264, 274, 278, 280, 282, 293, 299, 308, 320, 324, 341, 356, 357, 360, 361),
           fill = list(rel_abun = 0))

### Merging with treatment info
biocon <- complete_biocon %>%
  left_join(biocon_treat, by = "plot") %>%
  select(-temp_treatment.x) %>%
  rename(temp_treatment = temp_treatment.y)

### Combining abundance data with niche estimate data
full_biocon <- biocon %>%
  left_join(niche_est, by = "species") %>%
  select(-temp_niche.x) %>%
  rename(temp_niche = temp_niche.y)



### Calculating baseline CTI to use for measure of center 
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
cti_initial <- calculate_initial_CTI(full_biocon)

# Merging with abundance data and centering the niche values
full_biocon <- left_join(full_biocon, cti_initial, by = "plot")
full_biocon <- full_biocon %>%
  mutate(temp_niche_center = temp_niche - cti_baseline)


### Pre- vs.post-warming treatment dataframes (warming started in 2012)
pre_warming <- full_biocon %>%
  filter(year < 2012)
post_warming <- full_biocon %>%
  filter(year >= 2012)



### CTI diff between warmed and ambient
CTI_pre <- pre_warming %>%
  group_by(year,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
  distinct() %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(CTI_diff = warmed - ambient)
CTI_post <- post_warming %>%
  group_by(year,temp_treatment) %>%
  reframe(CTI = sum(rel_abun * temp_niche_center) / sum(rel_abun)) %>%
  distinct() %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(CTI_diff = warmed - ambient)


### Matching abundance data w/ CTI data
abun <- full_biocon %>%
    group_by(year, species, temp_treatment) %>%
    summarize(rel_abun_avg = mean(rel_abun)) %>%
    pivot_wider(names_from = temp_treatment, values_from = rel_abun_avg) %>%
    mutate(rel_abun_diff = warmed - ambient)
# Pull out niche data per species
niches <- full_biocon %>%
    group_by(species) %>%
    summarize(temp_niche_center = mean(temp_niche_center)) %>%
    distinct()
# Joining that with abun data
final_biocon <- left_join(abun, niches, by = "species")
pre_warming_bio <- final_biocon %>%
  filter(year < 2012)
post_warming_bio <- final_biocon %>%
  filter(year >= 2012)



### Compute linear slopes using covariance and variance
## CTI
# Slope values are the same whether using raw or centered temp niche values
CTI_pre_slope <- cov(CTI_pre$year, CTI_pre$CTI_diff) / var(CTI_pre$year)
CTI_post_slope <- cov(CTI_post$year, CTI_post$CTI_diff) / var(CTI_post$year)

## Species
slopes_pre <- list()
# Unique species names
unique_species <- unique(pre_warming_bio$species)
# Calculate the slope for each species
for (species in unique_species) {
  # Subset the data for the current species
  species_data <- pre_warming_bio[pre_warming_bio$species == species, ]
  
  # Extract time and corresponding values
  time <- species_data$year
  value <- species_data$rel_abun_diff
  
  # Calculate the slope
  slope <- cov(time, value) / var(time)
  
  # Store the slope in the list
  slopes_pre[[species]] <- slope
}

slopes_post <- list()
# Unique species names
unique_species <- unique(post_warming_bio$species)
# Calculate the slope for each species
for (species in unique_species) {
  # Subset the data for the current species
  species_data <- post_warming_bio[post_warming_bio$species == species, ]
  
  # Extract time and corresponding values
  time <- species_data$year
  value <- species_data$rel_abun_diff
  
  # Calculate the slope
  slope <- cov(time, value) / var(time)
  
  # Store the slope in the list
  slopes_post[[species]] <- slope
}



### Initialize a vector or list to store contributions
## Pre-warming
contributions_pre <- numeric(length(slopes_pre))
names(contributions_pre) <- names(slopes_pre)

# Calculate the contribution for each species
for (species in names(slopes_pre)) {
  # Find the thermal niche for the current species
  niche_value <- pre_warming_bio$temp_niche_center[pre_warming_bio$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions_pre[species] <- slopes_pre[[species]] * niche_value
}

# Sum of contributions
slope_sum_pre <- sum(contributions_pre)

## Post-warming
contributions_post <- numeric(length(slopes_post))
names(contributions_post) <- names(slopes_post)

# Calculate the contribution for each species
for (species in names(slopes_post)) {
  # Find the thermal niche for the current species
  niche_value <- post_warming_bio$temp_niche_center[post_warming_bio$species == species]
  
  # Compute the contribution: slope * thermal niche
  contributions_post[species] <- slopes_post[[species]] * niche_value
}

# Sum of contributions
slope_sum_post <- sum(contributions_post)



### Data for plotting
# Pull out species names and species slopes into their own lists
species_contribution_pre <- unlist(contributions_pre)
species_contribution_post <- unlist(contributions_post)
species_names <- names(contributions_pre)
species_slopes_pre <- unlist(slopes_pre)
species_slopes_post <- unlist(slopes_post)
# Create a data frame for plotting
data_for_plot <- data.frame(
  species = c(species_names),
  contribution_pre = c(species_contribution_pre),
  contribution_post = c(species_contribution_post),
  slope_pre = c(species_slopes_pre),
  slope_post = c(species_slopes_post)
)
# Merge with niche data
data_for_plot <- left_join(data_for_plot,niches, by = c("species"))
# Remove data whose absolute value for slope is < 0.0002
data_for_plot <- data_for_plot[abs(data_for_plot$contribution) > 0.002, ]



# Warming treatment effect over time
# This slope matches the calculated slope & sum of contributions
ggscatter(CTI_pre, x = "year", y = "CTI_diff", 
          add = "reg.line", conf.int = T, 
          cor.coef = F, cor.method = "pearson",
          xlab = "Year", ylab = "CTI (warmed - ambient)",title="PHACE") +
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~"))
  )
ggscatter(CTI_post, x = "year", y = "CTI_diff", 
          add = "reg.line", conf.int = T, 
          cor.coef = F, cor.method = "pearson",
          xlab = "Year", ylab = "CTI (warmed - ambient)",title="PHACE") +
  stat_regline_equation(
    aes(label =  paste(after_stat(eq.label), ..adj.rr.label.., sep = "~~~~"))
  )



### Point plot
pre_cont <- ggplot(data_for_plot, aes(x=contribution_pre, y = reorder(species, abs(contribution_pre)), color=temp_niche_center)) +
  geom_segment(aes(x = 0, xend = slope_pre, 
                   y = reorder(species, abs(contribution_pre)), 
                   yend = reorder(species, abs(contribution_pre))),
               arrow = arrow(type = "closed", length = unit(0.075, "inches")),
               color = "red") +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = CTI_slope, linetype = "dashed") +
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

post_cont <- ggplot(data_for_plot, aes(x=contribution_post, y = reorder(species, abs(contribution_post)), color=temp_niche_center)) +
  geom_segment(aes(x = 0, xend = slope_post, 
                   y = reorder(species, abs(contribution_post)), 
                   yend = reorder(species, abs(contribution_post))),
               arrow = arrow(type = "closed", length = unit(0.075, "inches")),
               color = "red") +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = CTI_slope, linetype = "dashed") +
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




### Export Rdata for plots
path_out = "/Volumes/seas-zhukai/proj-ecoacc/data_for_plots/"
saveRDS(pre_cont, paste(path_out,'biocon_treat_pre.rds'))
saveRDS(post_cont, paste(path_out,'biocon_treat_post.rds'))


