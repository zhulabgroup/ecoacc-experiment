# TITLE:          B4Warmed data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned B4Warmed experiment data
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/B4Warmed/"
setwd(path_data)

# Read in data
b4_data <- read.csv("2008-2021_B4W_Census_All_wBiomass_02122024.csv")
b4_spp <- read.csv("b4warmed_species_codes.csv")

# Selecting columns to keep
colnames(b4_data)
b4_data_sub <- b4_data[,c(2,5:8,12:14,46)]

# Fixing species names
b4_data_sub <- left_join(b4_data_sub, b4_spp, by="species")
b4_data_sub <- b4_data_sub[,-c(8)]

# Fixing column names and back transforming log
b4_named <- b4_data_sub %>%
  rename(temp_treatment = heat_trt) %>%
  rename(water_treatment = water_trt) %>%
  rename(plot = plot_id) %>%
  rename(year = census_year) %>%
  rename(ab_biomass_log = PRed.Formula.C.log.measured.stemM_2023_est) %>%
  rename(species = species_name) %>%
  mutate(ab_biomass = exp(ab_biomass_log))
b4_named <- b4_named[,-c(8)]
b4_named$temp_treatment[b4_named$temp_treatment == "oldAmbient"] <- "amb"

# Calculating relative abundance and fixing column names
b4_abun <- b4_named %>%
  filter(!is.na(year)) %>%
  filter(planting_cohort_corrected == 2012) %>%
  group_by(year,plot,canopy_condition,site,water_treatment,temp_treatment, species) %>%
  summarize(species_biomass = sum(ab_biomass, na.rm=T)) %>%
  group_by(year,plot,canopy_condition,site,water_treatment,temp_treatment) %>%
  mutate(total_biomass = sum(species_biomass, na.rm=T)) %>%
  ungroup %>%
  mutate(rel_abun = species_biomass/total_biomass) 

# Selecting only plots with open canopy and subsetting to wanted columns for each site
b4_cfc <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "CFC") %>%
  dplyr::select(year,plot,temp_treatment,species,rel_abun)
b4_hwrc <- b4_abun %>%
  filter(canopy_condition == "Open") %>%
  filter(site == "HWRC") %>%
  dplyr::select(year,plot,temp_treatment,species,rel_abun)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)

# Load in data
niche_est_cfc <- read.csv(" b4warmed_cfc_niche.csv")
niche_est_hwrc <- read.csv(" b4warmed_hwrc_niche.csv")

# Combining phace abundance data with niche estimate data
full_abun_data_cfc <- left_join(b4_cfc, niche_est_cfc, by = c("species"))
full_abun_data_cfc <- full_abun_data_cfc %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_data_cfc$site <- "B4Warmed CFC"
full_abun_data_hwrc <- left_join(b4_hwrc, niche_est_hwrc, by = c("species"))
full_abun_data_hwrc <- full_abun_data_hwrc %>%
  filter(!is.na(rel_abun)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_data_hwrc$site <- "B4Warmed HWRC"



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# Load in data
mat <- read.csv(" MAT.csv")
# Merging with data
full_abun_data_cfc <- left_join(full_abun_data_cfc, mat, by = c("site","year"))
full_abun_data_hwrc <- left_join(full_abun_data_hwrc, mat, by = c("site","year"))



# Coding MAT from warmed plots to be hotter
# 1.7 and 3.4 warming levels, warmed for 8 months of the year
1.6/1.5 # Add 1.07 to MAT (12/8 = 1.5)
3.1/1.5 # Add 2.07 to MAT (12/8 = 1.5)
full_abun_data_cfc$MAT <- ifelse(
  full_abun_data_cfc$temp_treatment == "1.7",
  full_abun_data_cfc$MAT + 1.07,
  full_abun_data_cfc$MAT
)
full_abun_data_cfc$MAT <- ifelse(
  full_abun_data_cfc$temp_treatment == "3.4",
  full_abun_data_cfc$MAT + 2.07,
  full_abun_data_cfc$MAT
)
full_abun_data_hwrc$MAT <- ifelse(
  full_abun_data_hwrc$temp_treatment == "1.7",
  full_abun_data_hwrc$MAT + 1.07,
  full_abun_data_hwrc$MAT
)
full_abun_data_hwrc$MAT <- ifelse(
  full_abun_data_hwrc$temp_treatment == "3.4",
  full_abun_data_hwrc$MAT + 2.07,
  full_abun_data_hwrc$MAT
)


# Removing 2021; trees young in this year
full_abun_data_cfc <- full_abun_data_cfc %>%
  filter(year != 2021)
full_abun_data_hwrc <- full_abun_data_hwrc %>%
  filter(year != 2021)

# Calculating CTI
CTI_cfc <- full_abun_data_cfc %>%
  group_by(year,plot,temp_treatment,MAT) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3,
          disequilib = CTI - MAT) %>%
  distinct()
CTI_hwrc <- full_abun_data_hwrc %>%
  group_by(year,plot,temp_treatment,MAT) %>%
  reframe(CTI = sum(rel_abun * temp_niche) / sum(rel_abun),
          CTI_var = sum(rel_abun * (temp_niche - CTI)^2) / sum(rel_abun),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(rel_abun * (temp_niche - CTI)^3) / (sum(rel_abun) * CTI_sd^3),
          CTI_kurt = sum(rel_abun * (temp_niche - CTI)^4) / (sum(rel_abun) * CTI_sd^4) - 3,
          disequilib = CTI - MAT) %>%
  distinct()


# Calculating CTI sensitivity (warmed - ambient)
CTI_sens_cfc <- CTI_cfc %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI), sd_cti = sd(CTI), n = n()) %>%
  pivot_wider(names_from = temp_treatment, values_from = c(mean_cti, sd_cti, n)) %>%
  mutate(
    sensitivity_high_temp = `mean_cti_3.4` - `mean_cti_amb`,
    SE_diff_high_temp = sqrt((`sd_cti_3.4`^2 / `n_3.4`) + (`sd_cti_amb`^2 / `n_amb`)),
    sensitivity_med_temp = `mean_cti_1.7` - `mean_cti_amb`,
    SE_diff_med_temp = sqrt((`sd_cti_1.7`^2 / `n_1.7`) + (`sd_cti_amb`^2 / `n_amb`)),
    sens_high_temp_scaled = sensitivity_high_temp / 2.07,
    sens_med_temp_scaled = sensitivity_med_temp / 1.07,
    SE_diff_high_temp_scaled = SE_diff_high_temp / 2.07,
    SE_diff_med_temp_scaled = SE_diff_med_temp / 1.07
  )
CTI_sens_hwrc <- CTI_hwrc %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_cti = mean(CTI), sd_cti = sd(CTI), n = n()) %>%
  pivot_wider(names_from = temp_treatment, values_from = c(mean_cti, sd_cti, n)) %>%
  mutate(
    sensitivity_high_temp = `mean_cti_3.4` - `mean_cti_amb`,
    SE_diff_high_temp = sqrt((`sd_cti_3.4`^2 / `n_3.4`) + (`sd_cti_amb`^2 / `n_amb`)),
    sensitivity_med_temp = `mean_cti_1.7` - `mean_cti_amb`,
    SE_diff_med_temp = sqrt((`sd_cti_1.7`^2 / `n_1.7`) + (`sd_cti_amb`^2 / `n_amb`)),
    sens_high_temp_scaled = sensitivity_high_temp / 2.07,
    sens_med_temp_scaled = sensitivity_med_temp / 1.07,
    SE_diff_high_temp_scaled = SE_diff_high_temp / 2.07,
    SE_diff_med_temp_scaled = SE_diff_med_temp / 1.07
  )

CTI_mean_b4_cfc <- CTI_cfc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_b4_hwrc <- CTI_hwrc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))


# Plot
CTI_mean_b4_cfc$temp_treatment <- factor(
  CTI_mean_b4_cfc$temp_treatment,
  levels = c("amb", "1.7", "3.4")
)
ggplot(CTI_mean_b4_cfc, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI),
                position = position_dodge(width = 0.8),
                width = 0.2, alpha = 0.8) +
  labs(x = "Year", y = "CTI", title = "(e) B4WarmED CFC") +
  scale_fill_manual(name = "Treatment",
                    breaks = c("amb", "1.7", "3.4"),
                    labels = c("Ambient", "Intermediate", "Warmed"),
                    values = c("blue", "orange", "red")) +
  #coord_cartesian(ylim = c(6.5, 7.6)) +
  scale_x_discrete(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
CTI_mean_b4_hwrc$temp_treatment <- factor(
  CTI_mean_b4_hwrc$temp_treatment,
  levels = c("amb", "1.7", "3.4")
)
ggplot(CTI_mean_b4_hwrc, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI),
                position = position_dodge(width = 0.8),
                width = 0.2, alpha = 0.8) +
  labs(x = "Year", y = "CTI", title = "(f) B4WarmED HWRC") +
  scale_fill_manual(name = "Treatment",
                    breaks = c("amb", "1.7", "3.4"),
                    labels = c("Ambient", "Intermediate", "Warmed"),
                    values = c("blue", "orange", "red")) +
  #coord_cartesian(ylim = c(6.5, 7.6)) +
  scale_x_discrete(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
ggplot(CTI_sens_cfc) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity_high_temp,
                    ymin = sensitivity_high_temp-SE_diff_high_temp,
                    ymax = sensitivity_high_temp+SE_diff_high_temp,
                    linetype="Warmed - Ambient"), 
                width = 0.2, alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity_high_temp), 
             shape = 21, size = 1.5, alpha=0.4,color="black",fill="black") +
  geom_errorbar(aes(x = year, y = sensitivity_med_temp,
                    ymin = sensitivity_med_temp-SE_diff_med_temp,
                    ymax = sensitivity_med_temp+SE_diff_med_temp,
                    linetype="Intermediate - Ambient"), 
                width = 0.2, position = position_nudge(x = 0.2), , alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity_med_temp), 
             shape = 21, size = 1.5, position = position_nudge(x = 0.2), ,alpha=0.4,color="black",fill="black") +
  geom_smooth(aes(x = year, y = sensitivity_high_temp,linetype="Warmed - Ambient"),method='lm',color="black") +
  geom_smooth(aes(x = year, y = sensitivity_med_temp,linetype="Intermediate - Ambient"),method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI",title = "(k) B4WarmED CFC",linetype="Treatment difference") +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
ggplot(CTI_sens_hwrc) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity_high_temp,
                    ymin = sensitivity_high_temp-SE_diff_high_temp,
                    ymax = sensitivity_high_temp+SE_diff_high_temp,
                    linetype="Warmed - Ambient"), 
                width = 0.2, alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity_high_temp), 
             shape = 21, size = 1.5, alpha=0.4,color="black",fill="black") +
  geom_errorbar(aes(x = year, y = sensitivity_med_temp,
                    ymin = sensitivity_med_temp-SE_diff_med_temp,
                    ymax = sensitivity_med_temp+SE_diff_med_temp,
                    linetype="Intermediate - Ambient"), 
                width = 0.2, position = position_nudge(x = 0.2), , alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity_med_temp), 
             shape = 21, size = 1.5, position = position_nudge(x = 0.2), ,alpha=0.4,color="black",fill="black") +
  geom_smooth(aes(x = year, y = sensitivity_high_temp,linetype="Warmed - Ambient"),method='lm',color="black") +
  geom_smooth(aes(x = year, y = sensitivity_med_temp,linetype="Intermediate - Ambient"),method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI",title = "(l) B4WarmED HWRC", linetype="Treatment difference") +
  scale_linetype_manual(values = linetypes) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
