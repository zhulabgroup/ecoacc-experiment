# TITLE:          TeRaCON CTI/CPI & ecosystem response with no Andropogon
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI/CPI calculations and biomass without Andropogon
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
niche_est <- read.csv(" teracon_niche.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/TeRaCON"
setwd(path_data)
# Load in data
teracon_eco <- read.csv("teracon harvest file for Kara Dobson_241021.csv")


### Niche data for CTI
# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Remove species entirely
full_abun_no_andro <- full_abun_data %>%
  filter(!(species == "Andropogon gerardi"))

# Remove entire plots that contained the species
# Identify plot numbers that contain the target species
plots_with_target_species <- full_abun_data %>%
  filter(species == "Andropogon gerardi") %>%
  pull(plot) %>%
  unique()
# Filter out rows where the plot number is in the list of plots with target species
full_abun_no_andro_plots <- full_abun_data %>%
  filter(!plot %in% plots_with_target_species)

# CTI
CTI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  filter(!(CTI == "NaN")) %>%
  distinct()
CTI_filt_plot <- full_abun_no_andro_plots %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  filter(!(CTI == "NaN")) %>%
  distinct()

# CTI Sensitivity
CTI_sens_filt <- CTI_filt %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)
CTI_sens_filt_plot <- CTI_filt_plot %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

# CPI
CPI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3) %>%
  filter(!(CPI == "NaN"))
CPI_filt_plot <- full_abun_no_andro_plots %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3) %>%
  filter(!(CPI == "NaN"))

# CTI and CPI combined
CTI_CPI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  filter(!(CTI == "NaN")) %>%
  filter(!(CPI == "NaN")) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")
CTI_CPI_filt_plot <- full_abun_no_andro_plots %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  filter(!(CTI == "NaN")) %>%
  filter(!(CPI == "NaN")) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")



### Ecosystem response data
# Select columns to keep
teracon_data_sub_eco_blue <- teracon_eco[,c(1:9,11,27,42,55,68:69,74,76,78,80,82,84)]

# Removing first test row
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue[2:1153,]

# Scaling data from F to C
teracon_data_sub_eco_blue <- teracon_data_sub_eco_blue %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)

# Remove species entirely
teracon_spp_rem <- teracon_data_sub_eco_blue %>%
  mutate(ab_biomass = AbovegroundTotal.Biomass..g.m.2. - Andropogon.gerardi..g.m.2.)
teracon_spp_rem <- teracon_spp_rem[,-c(12,13)]

# Remove entire plots that contained the species
eco_plot_rem <- teracon_data_sub_eco_blue %>%
  filter(!Plot %in% plots_with_target_species)
eco_plot_rem <- eco_plot_rem[,-c(12)]

# Renaming columns
teracon_spp_rem <- teracon_spp_rem %>%
  rename(plot = Plot) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment) %>%
  rename(bl_biomass = Total.root.biomass.0.20..g.m.2.) %>%
  rename(total_biomass = Total.Biomass) %>%
  rename(root_ingrowth = Annual.Total.Root.Ingrowth..g.m.2.) %>%
  rename(total_n = Whole.Plot.Total.N..g.m.2.) %>%
  rename(bl_n = Belowground.N..total....g.m.2.) %>%
  rename(bl_c = Belowground.Carbon...) %>%
  rename(ab_n = Aboveground.N..total....g.m.2.) %>%
  rename(ab_c = Aboveground.Carbon...) %>%
  mutate(biomass_plus_root = ab_biomass+root_ingrowth)
eco_plot_rem <- eco_plot_rem %>%
  rename(plot = Plot) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(water_treatment = Water.Treatment) %>%
  rename(n_treatment = Nitrogen.Treatment) %>%
  rename(cn_treatment = C.and.N.treatment) %>%
  rename(co2_treatment = CO2.Treatment) %>%
  rename(ab_biomass = AbovegroundTotal.Biomass..g.m.2.) %>%
  rename(bl_biomass = Total.root.biomass.0.20..g.m.2.) %>%
  rename(total_biomass = Total.Biomass) %>%
  rename(root_ingrowth = Annual.Total.Root.Ingrowth..g.m.2.) %>%
  rename(total_n = Whole.Plot.Total.N..g.m.2.) %>%
  rename(bl_n = Belowground.N..total....g.m.2.) %>%
  rename(bl_c = Belowground.Carbon...) %>%
  rename(ab_n = Aboveground.N..total....g.m.2.) %>%
  rename(ab_c = Aboveground.Carbon...) %>%
  mutate(biomass_plus_root = ab_biomass+root_ingrowth)



### Ecosystem response calculation
# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)
# Load in data
eco_teracon_noblue <- read.csv(" teracon_ecosystem_dat_clean_noblue.csv")
eco_teracon_noblue_plot <- read.csv(" teracon_ecosystem_dat_clean_noblueplots.csv")

# Filter data to August (harvest) and calculate the mean ecosystem response vars for each year + treatment
# Then, calculating 'sensitivity' as warmed-ambient each year for that var
eco_grouped_noblue <- eco_teracon_noblue %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n)) %>%
  pivot_longer(cols = c(mean_ab_bio, mean_bl_bio, mean_total_bio,mean_ab_and_root,
                        mean_total_n, mean_bl_c, mean_bl_n,
                        mean_ab_c,mean_ab_n), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = HTelv - HTamb) %>%
  dplyr::select(year, variable, sensitivity)
eco_grouped_noblue_plot <- eco_teracon_noblue_plot %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n)) %>%
  pivot_longer(cols = c(mean_ab_bio, mean_bl_bio, mean_total_bio,mean_ab_and_root,
                        mean_total_n, mean_bl_c, mean_bl_n,
                        mean_ab_c,mean_ab_n), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = temp_treatment, values_from = value) %>%
  mutate(sensitivity = HTelv - HTamb) %>%
  dplyr::select(year, variable, sensitivity)

eco_grouped_overall_noblue <- eco_teracon_noblue %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass, na.rm=T),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root, na.rm=T),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n))
eco_grouped_overall_noblue_plot <- eco_teracon_noblue_plot %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(mean_ab_bio = mean(ab_biomass, na.rm=T),
          mean_bl_bio = mean(bl_biomass),
          mean_total_bio = mean(total_biomass),
          mean_ab_and_root = mean(biomass_plus_root, na.rm=T),
          mean_total_n = mean(total_n),
          mean_bl_c = mean(bl_c),
          mean_bl_n = mean(bl_n),
          mean_ab_c = mean(ab_c),
          mean_ab_n = mean(ab_n))


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
write.csv(CTI_filt,paste(path_out,'CTI_teracon_nobluestem.csv'))
write.csv(CTI_sens_filt,paste(path_out,'CTI_sens_teracon_nobluestem.csv'))
write.csv(CTI_CPI_filt,paste(path_out,'CTI_CPI_teracon_nobluestem.csv'))
write.csv(CTI_filt_plot,paste(path_out,'CTI_teracon_nobluestemplots.csv'))
write.csv(CTI_sens_filt_plot,paste(path_out,'CTI_sens_teracon_nobluestemplots.csv'))
write.csv(CTI_CPI_filt_plot,paste(path_out,'CTI_CPI_teracon_nobluestemplots.csv'))

write.csv(teracon_spp_rem,paste(path_out,'teracon_ecosystem_dat_clean_noblue.csv'))
write.csv(eco_plot_rem,paste(path_out,'teracon_ecosystem_dat_clean_noblueplots.csv'))

write.csv(eco_grouped_noblue,paste(path_out,'eco_response_teracon_noblue.csv'))
write.csv(eco_grouped_overall_noblue,paste(path_out,'eco_response_overall_teracon_noblue.csv'))
write.csv(eco_grouped_noblue_plot,paste(path_out,'eco_response_teracon_noblueplots.csv'))
write.csv(eco_grouped_overall_noblue_plot,paste(path_out,'eco_response_overall_teracon_noblueplots.csv'))


