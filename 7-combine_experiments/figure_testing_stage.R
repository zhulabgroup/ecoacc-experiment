# TITLE:          Testing different figures; final figs are in EcoAcc-Experiments
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(ggpubr)
library(plotly)
library(maps)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TRY_data/"
setwd(path_data)
# Load in data
growth_life <- read.csv(" exp_species_growth_lifecycle.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
tera <- read.csv(" teracon_clean.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
niche_est_tera <- read.csv(" teracon_niche.csv")
gbif_tera <- read.csv(" GBIF_teracon.csv")
# Set path to turbo to get data; data for no bluestem
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)
CTI_sens_teracon_noblue <- read.csv(" CTI_sens_teracon_nobluestem.csv")
CTI_teracon_noblue <- read.csv(" CTI_teracon_nobluestem.csv")
CTI_CPI_teracon_noblue <- read.csv(" CTI_CPI_teracon_nobluestem.csv")
NPP_teracon_noblue <- read.csv(" eco_response_teracon_noblue.csv")
NPP_overall_teracon_noblue <- read.csv(" eco_response_overall_teracon_noblue.csv")
CTI_sens_teracon_noblueplots <- read.csv(" CTI_sens_teracon_nobluestemplots.csv")
CTI_teracon_noblueplots <- read.csv(" CTI_teracon_nobluestemplots.csv")
CTI_CPI_teracon_noblueplots <- read.csv(" CTI_CPI_teracon_nobluestemplots.csv")
NPP_teracon_noblueplots <- read.csv(" eco_response_teracon_noblueplots.csv")
NPP_overall_teracon_noblueplots <- read.csv(" eco_response_overall_teracon_noblueplots.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)
# Load in data
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
CTI_jrgce <- read.csv(" CTI_jrgce.csv")
jrgce <- read.csv(" jrgce_clean.csv")
jrgce <- jrgce %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "T"), "warmed", "ambient"))
NPP_jrgce <- read.csv(" eco_response_jrgce.csv")
NPP_overall_jrgce <- read.csv(" eco_response_overall_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
gbif_jrgce <- read.csv(" GBIF_jrgce.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace.csv")
CTI_phace <- read.csv(" CTI_phace.csv")
phace <- read.csv(" phace_clean.csv")
NPP_phace <- read.csv(" eco_response_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace.csv")
niche_est_phace <- read.csv(" phace_niche.csv")
gbif_phace <- read.csv(" GBIF_phace.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)
# Load in data
CTI_sens_b4 <- read.csv(" CTI_sens_b4warmed.csv")
CTI_b4 <- read.csv(" CTI_b4warmed.csv")
b4 <- read.csv(" b4warmed_clean.csv")
NPP_b4 <- read.csv(" eco_response_b4warmed.csv")
NPP_overall_b4 <- read.csv(" eco_response_overall_b4warmed.csv")
CTI_CPI_b4 <- read.csv(" CTI_CPI_b4warmed.csv")
niche_est_b4 <- read.csv(" b4warmed_niche.csv")





##### Fig: Checking distribution of occurrences for each species
world <- map_data("world")
distb_occ <- function(data,spp){
  
  spp_data <- data %>%
    filter(species == spp)
  
  ggplot() +
    geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region),
      color = "lightgrey", fill = "darkgrey", size = 0.1
    ) +
    geom_point(
      data = spp_data,
      aes(decimalLongitude, decimalLatitude),
      alpha = 0.7,
      color = "red",
      size=2
    ) +
    theme_classic() +
    labs(x = "Longitude",y = "Latitude") + 
    theme(axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))
}
# Plot occurrence maps for abundant species in each experiment
distb_occ(gbif_tera,"Andropogon gerardi")
distb_occ(gbif_jrgce,"Festuca bromoides")
distb_occ(gbif_phace,"Elymus smithii")






##### Fig: CTI as a function of temperature over time
# Using if...else to determine the value based on temp_treatment
CTI_teracon$ambient_temp <- NA
CTI_teracon <- CTI_teracon %>%
  mutate(ambient_temp = case_when(temp_treatment == "HTelv" ~ mean_C_temp_warmed,
                                  temp_treatment == "HTamb" ~ mean_C_temp_summer))

CTI_teracon_plot <- ggplot(CTI_teracon, aes(x = mean_C_temp_summer, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
              #position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="TeRaCON") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

CTI_jrgce_plot <- ggplot(CTI_jrgce, aes(x = mean_C_temp_summer, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
              #position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="TeRaCON") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))






##### Fig:  interactive plot for species niche estimates
plot_ly(data=niche_est_phace_avg, x = ~temp_niche, y = ~precip_niche,
        type="scatter",color=~species)






##### Fig: Bubble plots for species abundance each year in each treatment
# Merging niche estimate data with abundance data
niche_est_phace_avg <- niche_est_phace[,c(1,6,7)]
niche_est_phace_avg <- niche_est_phace_avg %>%
  distinct()
phace_merge <- merge(phace_test,niche_est_phace_avg, by="species")

niche_est_jrgce_avg <- niche_est_jrgce %>%
  dplyr::select(-c(tmp,ppt)) %>%
  distinct()
jrgce_merge <- merge(jrgce_test,niche_est_jrgce_avg, by="species")

niche_est_tera_avg <- niche_est_tera %>%
  dplyr::select(-c(mean_annual_temp, mean_annual_precip, latitude, longitude)) %>%
  distinct()
tera_merge <- merge(tera_test,niche_est_tera_avg, by="species")

phace_warm <- phace_merge %>%
  filter(temp_treatment == "warmed")
phace_amb <- phace_merge %>%
  filter(temp_treatment == "ambient")
abun_warm <- ggplot(phace_warm, aes(x = temp_niche, y = precip_niche)) + 
  geom_point(aes(size = avg_abun), alpha = 0.75, shape = 21) + 
  facet_wrap(.~year) +
  theme_bw()
abun_amb <- ggplot(phace_amb, aes(x = temp_niche, y = precip_niche)) + 
  geom_point(aes(size = avg_abun), alpha = 0.75, shape = 21) + 
  facet_wrap(.~year) +
  theme_bw()
# Combine figures into one multi-panel plot
ggarrange(abun_warm,abun_amb,
          ncol = 2, nrow=1, common.legend = T, legend = "right")





##### Fig: teracon --> w/ and w/o big bluestem
CTI_blue_tera <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/ big bluestem") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_noblue_tera <- ggplot(CTI_teracon_noblue, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/o big bluestem") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_noblueplots_tera <- ggplot(CTI_teracon_noblueplots, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/o big bluestem plots") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
arrow_blue <- ggplot(CTI_CPI_teracon) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/ big bluestem") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_noblue <- ggplot(CTI_CPI_teracon_noblue) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/o big bluestem") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_noblueplots <- ggplot(CTI_CPI_teracon_noblueplots) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/o big bluestem plots") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
CTI_blue_smooth <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/ big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
CTI_noblue_smooth <- ggplot(CTI_sens_teracon_noblue, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/o big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
CTI_noblueplots_smooth <- ggplot(CTI_sens_teracon_noblueplots, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/o big bluestem plots") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
# Combine figures into one multi-panel plot
ggarrange(CTI_blue_tera, arrow_blue, CTI_blue_smooth,
          CTI_noblue_tera, arrow_noblue, CTI_noblue_smooth,
          CTI_noblueplots_tera, arrow_noblueplots, CTI_noblueplots_smooth,
          ncol = 3, nrow=3)





#### Fig: teracon --> CTI vs. biomass w/ and w/o big bluestem
CTI_yearly_avg_tera <- CTI_teracon %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon <- left_join(CTI_yearly_avg_tera, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter <- ggscatter(NPP_CTI_teracon, x = "mean_ab_bio", y = "mean_CTI", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomass", ylab = "CTI",title="All data")

CTI_yearly_avg_tera_noblue <- CTI_teracon_noblue %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon_noblue <- left_join(CTI_yearly_avg_tera_noblue, NPP_overall_teracon_noblue, by=c("year","temp_treatment"))
tera_scatter_noblue <- ggscatter(NPP_CTI_teracon_noblue, x = "mean_ab_bio", y = "mean_CTI", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomass", ylab = "CTI", title="Bluestem removed")

CTI_yearly_avg_tera_noblueplots <- CTI_teracon_noblueplots %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon_noblueplots <- left_join(CTI_yearly_avg_tera_noblueplots, NPP_overall_teracon_noblueplots, by=c("year","temp_treatment"))
tera_scatter_noblueplots <- ggscatter(NPP_CTI_teracon_noblueplots, x = "mean_ab_bio", y = "mean_CTI", 
                                 add = "reg.line", conf.int = TRUE, 
                                 cor.coef = TRUE, cor.method = "pearson",
                                 xlab = "Biomass", ylab = "CTI", title="Bluestem plots removed")

# Combine figures into one multi-panel plot
ggarrange(tera_scatter,tera_scatter_noblue,tera_scatter_noblueplots,
          ncol = 3)





##### Fig: species sorted by life cycle
jrgce_sig_spp <- left_join(jrgce_sig_spp, growth_life, by="species")
phace_sig_spp <- left_join(phace_sig_spp, growth_life, by="species")
tera_sig_spp <- left_join(tera_sig_spp, growth_life, by="species")
b4_sig_spp_cfc <- left_join(b4_sig_spp_cfc, growth_life, by="species")
b4_sig_spp_hwrc <- left_join(b4_sig_spp_hwrc, growth_life, by="species")

ggplot(jrgce_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "JRGCE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(phace_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(tera_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(b4_sig_spp_cfc, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(b4_sig_spp_hwrc, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")



##### Fig: % perennial species in each experiment
jrgce_lifecycles <- left_join(jrgce, growth_life, by="species")
phace_lifecycles <- left_join(phace, growth_life, by="species")
tera_lifecycles <- left_join(tera, growth_life, by="species")
b4_cfc_lifecycles <- left_join(b4_cfc, growth_life, by="species")
b4_hwrc_lifecycles <- left_join(b4_hwrc, growth_life, by="species")

# Selecting species, life cycle, and growth form column form each data frame
jrgce_lifecycles <- jrgce_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
phace_lifecycles <- phace_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
tera_lifecycles <- tera_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
b4_cfc_lifecycles <- b4_cfc_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
b4_hwrc_lifecycles <- b4_hwrc_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()

# For each data frame, calculate the % of species for life cycle
jrgce_lifecycles_perc <- jrgce_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
phace_lifecycles_perc <- phace_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
tera_lifecycles_perc <- tera_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_cfc_lifecycles_perc <- b4_cfc_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_hwrc_lifecycles_perc <- b4_hwrc_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))

# For each data frame, calculate the % of species for growth form
jrgce_growth_perc <- jrgce_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
phace_growth_perc <- phace_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
tera_growth_perc <- tera_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_cfc_growth_perc <- b4_cfc_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_hwrc_growth_perc <- b4_hwrc_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))



