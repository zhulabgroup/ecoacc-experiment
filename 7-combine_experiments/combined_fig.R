# TITLE:          Combined plots for all experiments 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(ggpubr)
library(plotly)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
# Data using global spp occurrences from GBIF + year-round MAT:
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
tera <- read.csv(" teracon_clean.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
# Data using ecoregion 8 spp occurrences from GBIF + year-round MAT:
CTI_sens_teracon_lim <- read.csv(" CTI_sens_teracon_limited.csv")
CTI_teracon_lim <- read.csv(" CTI_teracon_limited.csv")
CTI_CPI_teracon_lim <- read.csv(" CTI_CPI_teracon_limited.csv")
niche_est_tera <- read.csv(" niche_estimate_teracon_limited.csv")
# Data using ecoregion 8 spp occurrences from GBIF + 6-month temps (March-Aug):
CTI_sens_teracon_6month <- read.csv(" CTI_sens_6month_teracon_limited.csv")
CTI_teracon_6month <- read.csv(" CTI_6month_teracon_limited.csv")
CTI_CPI_teracon_6month <- read.csv(" CTI_CPI_6month_teracon_limited.csv")
# Data w/o big bluestem:
CTI_sens_teracon_noblue <- read.csv(" CTI_sens_teracon_nobluestem.csv")
CTI_teracon_noblue <- read.csv(" CTI_teracon_nobluestem.csv")
CTI_CPI_teracon_noblue <- read.csv(" CTI_CPI_teracon_nobluestem.csv")

# Set path to data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
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
niche_est_jrgce <- read.csv(" niche_estimate_jrgce.csv")

# Set path to data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace_limited.csv")
CTI_phace <- read.csv(" CTI_phace_limited.csv")
phace <- read.csv(" phace_clean.csv")
NPP_phace <- read.csv(" eco_response_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace_limited.csv")
niche_est_phace <- read.csv(" niche_estimate_phace_limited.csv")


##### Fig: CTI, ANPP, and ambient temps over time #####
# CTI figures
CTI_tera_plot <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "TeRaCON\nCTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
CTI_jrgce_plot <- ggplot(CTI_sens_jrgce, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "JRGCE\nCTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw()
CTI_phace_plot <- ggplot(CTI_sens_phace, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "PHACE\nCTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()

# Biomass figures
NPP_teracon <- NPP_teracon %>%
  filter(variable == "mean_ab_bio")
npp_tera_plot <- ggplot(NPP_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
npp_jrgce_plot <- ggplot(NPP_jrgce, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw()
npp_phace_plot <- ggplot(NPP_phace, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "Biomass (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()

# Ambient temp change figures
temp_tera_plot <- ggplot(CTI_sens_teracon, aes(x = year, y = mean_C_temp_summer)) +
  geom_smooth() +
  labs(x = "Year", y = "Ambient temperature (°C)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
temp_jrgce_plot <- ggplot(CTI_sens_jrgce, aes(x = year, y = mean_C_temp_summer)) +
  geom_smooth() +
  labs(x = "Year", y = "Ambient temperature (°C)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_tera_plot,npp_tera_plot,temp_tera_plot,
          CTI_jrgce_plot,npp_jrgce_plot,temp_jrgce_plot,
          CTI_phace_plot,npp_phace_plot,
          ncol = 3, nrow=3, common.legend = T, legend = "right")



##### Fig: correlating biomass w/ CTI
# TeRaCON
CTI_yearly_avg_tera <- CTI_teracon_6month %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon <- left_join(CTI_yearly_avg_tera, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter <- ggscatter(NPP_CTI_teracon, x = "mean_ab_bio", y = "mean_CTI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Biomass", ylab = "TeRaCON\nCTI")
# JRGCE
CTI_yearly_avg_jrgce <- CTI_jrgce %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_jrgce <- left_join(CTI_yearly_avg_jrgce, NPP_overall_jrgce, by=c("year","temp_treatment"))
jrgce_scatter <- ggscatter(NPP_CTI_jrgce, x = "mean_ab_bio", y = "mean_CTI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Biomass", ylab = "JRGCE\nCTI")
# PHACE
CTI_yearly_avg_phace <- CTI_phace %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_phace <- left_join(CTI_yearly_avg_phace, NPP_overall_phace, by=c("year","temp_treatment"))
phace_scatter <- ggscatter(NPP_CTI_phace, x = "mean_ab_bio", y = "mean_CTI", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "Biomass", ylab = "PHACE\nCTI")

# Combine into one figure
ggarrange(tera_scatter,jrgce_scatter,phace_scatter,
          nrow=3)



##### Fig: Arrows pointing from ambient to warmed for each year #####
arrow_teracon <- ggplot(CTI_CPI_teracon) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "TeRaCON") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

arrow_jrgce <- ggplot(CTI_CPI_jrgce) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "JRGCE") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

arrow_phace <- ggplot(CTI_CPI_phace) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "PHACE") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

# Combine figures into one multi-panel plot
ggarrange(arrow_teracon,arrow_jrgce,arrow_phace,
          ncol = 3, nrow=1)



##### Fig: Mean CTI over time in warmed and ambient #####
# note: could also change y = CTI to a different metric (CTI_sd, CTI_skew, etc.)
CTI_teracon_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
              # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="TeRaCON") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

CTI_jrgce <- CTI_jrgce %>%
  filter(!(year == 1998))
CTI_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title = "JRGCE") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

CTI_phace_plot <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title = "PHACE") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

# Combine figures into one multi-panel plot
ggarrange(CTI_teracon_plot,CTI_jrgce_plot,CTI_phace_plot,
          ncol = 3, nrow=1)




##### Fig: Individual species abundances changes as a function of CTI
# Calculate mean spp abundance per year and treatment
phace_test <- phace %>%
  group_by(year, temp_treatment,species) %>%
  summarize(avg_abun = mean(rel_abun))
jrgce_test <- jrgce %>%
  group_by(year, temp_treatment,species) %>%
  summarize(avg_abun = mean(percent_cover))
tera_test <- tera %>%
  group_by(year, temp_treatment,species) %>%
  summarize(avg_abun = mean(percent_cover, na.rm=T))

# Calculating mean CTI per year and treatment
CTI_yearly_avg_phace <- CTI_phace %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
CTI_yearly_avg_jrgce <- CTI_jrgce %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
CTI_yearly_avg_tera <- CTI_teracon_lim %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))

# Merging CTI and species abundance data
phace_spp_abun <- merge(CTI_yearly_avg_phace,phace_test, by=c("year","temp_treatment"))
jrgce_spp_abun <- merge(CTI_yearly_avg_jrgce,jrgce_test, by=c("year","temp_treatment"))
tera_spp_abun <- merge(CTI_yearly_avg_tera,tera_test, by=c("year","temp_treatment"))

### Stats: Which species show significant increases or decreases in abundance as a function of CTI?
# Perform regression analysis for each species
results_phace <- phace_spp_abun %>%
  group_by(species) %>%
  do(model = lm(avg_abun ~ mean_CTI, data = .))
results_jrgce <- jrgce_spp_abun %>%
  filter(!(species == "Madia gracilis" | species == "Quercus lobata" | species == "Festuca DUMMY")) %>% # removing spp w/ NAs
  group_by(species) %>%
  do(model = lm(avg_abun ~ mean_CTI, data = .))
results_tera <- tera_spp_abun %>%
  filter(!(species == "Total Planted Species")) %>%
  group_by(species) %>%
  do(model = lm(avg_abun ~ mean_CTI, data = .))

# Extract coefficients and p-values
results_phace_summary <- results_phace %>%
  rowwise() %>%
  mutate(slope = coef(model)[["mean_CTI"]],
         p_value = summary(model)$coefficients["mean_CTI", "Pr(>|t|)"])
results_jrgce_summary <- results_jrgce %>%
  rowwise() %>%
  mutate(slope = coef(model)[["mean_CTI"]],
         p_value = summary(model)$coefficients["mean_CTI", "Pr(>|t|)"])
results_tera_summary <- results_tera %>%
  rowwise() %>%
  mutate(slope = coef(model)[["mean_CTI"]],
         p_value = summary(model)$coefficients["mean_CTI", "Pr(>|t|)"])

# Apply multiple testing correction (Benjamini-Hochberg)
results_phace_summary <- results_phace_summary %>%
  mutate(adj_p_value = p.adjust(p_value, method = "BH"))
results_jrgce_summary <- results_jrgce_summary %>%
  mutate(adj_p_value = p.adjust(p_value, method = "BH"))
results_tera_summary <- results_tera_summary %>%
  mutate(adj_p_value = p.adjust(p_value, method = "BH"))

# Filter out species with significant effects
results_phace_sig <- results_phace_summary %>%
  filter(adj_p_value <= 0.06)
results_jrgce_sig <- results_jrgce_summary %>%
  filter(adj_p_value <= 0.06)
results_tera_sig <- results_tera_summary %>%
  filter(adj_p_value <= 0.06)

# Filtering data to sig. species
phace_spp_list <- unique(results_phace_sig$species)
phace_sig_spp <- phace_spp_abun %>%
  filter(species %in% phace_spp_list)

jrgce_spp_list <- unique(results_jrgce_sig$species)
jrgce_sig_spp <- jrgce_spp_abun %>%
  filter(species %in% jrgce_spp_list)

tera_spp_list <- unique(results_tera_sig$species)
tera_sig_spp <- tera_spp_abun %>%
  filter(species %in% tera_spp_list)

# Plotting changes in abundance for significant species  
ggplot(phace_sig_spp, aes(x = mean_CTI, y = avg_abun, color = temp_treatment, group = temp_treatment)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ species, scales = "free_y")
ggplot(jrgce_sig_spp, aes(x = mean_CTI, y = avg_abun, color = temp_treatment, group = temp_treatment)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ species, scales = "free_y")
ggplot(tera_sig_spp, aes(x = mean_CTI, y = avg_abun, color = temp_treatment, group = temp_treatment)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red")) +
  facet_wrap(~ species, scales = "free_y")




##### Fig: Species climate niche overlayed w/ treatment effects
# TeRaCON
niche_est_tera_avg <- niche_est_tera[,c(1,6,7)]
niche_est_tera_avg <- niche_est_tera_avg %>%
  distinct()
teracon_niche <- ggplot() +
  #geom_segment(data=CTI_CPI_teracon_lim, aes(x = CTI_HTamb, y = CPI_HTamb, 
  #                                           xend = CTI_HTelv, yend = CPI_HTelv,
  #                                           color = year),
  #             arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(data=CTI_CPI_teracon_lim, aes(x = CTI_HTamb, y = CPI_HTamb, color = year)) +
  geom_point(data=CTI_CPI_teracon_lim, aes(x = CTI_HTelv, y = CPI_HTelv, color = year)) +
  geom_point(data=niche_est_tera_avg, aes(x = temp_niche, y = precip_niche)) +
  labs(x = "Temperature", y = "Precipitation", title = "TeRaCON") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

# JRGCE
niche_est_jrgce_avg <- niche_est_jrgce[,c(1,4,5)]
niche_est_jrgce_avg <- niche_est_jrgce_avg %>%
  distinct()
jrgce_niche <- ggplot(CTI_CPI_jrgce) +
  #geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
  #                 xend = CTI_warmed, yend = CPI_warmed,
  #                 color = year),
  #             arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient, color = year)) +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed, color = year)) +
  geom_point(data=niche_est_jrgce_avg, aes(x = temp_niche, y = precip_niche)) +
  labs(x = "Temperature", y = "Precipitation", title = "JRGCE") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

# PHACE
niche_est_phace_avg <- niche_est_phace[,c(1,6,7)]
niche_est_phace_avg <- niche_est_phace_avg %>%
  distinct()
phace_niche <- ggplot(CTI_CPI_phace) +
  #geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
  #                 xend = CTI_warmed, yend = CPI_warmed,
  #                 color = year),
  #             arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient, color = year)) +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed, color = year)) +
  geom_point(data=niche_est_phace_avg, aes(x = temp_niche, y = precip_niche)) +
  labs(x = "Temperature", y = "Precipitation", title = "PHACE") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

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

