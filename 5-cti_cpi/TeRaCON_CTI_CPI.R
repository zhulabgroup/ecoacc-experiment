# TITLE:          TeRaCON CTI and CPI calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon
# DATA OUTPUT:    CTI and CPI calculations
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" niche_estimate_6month_teracon_limited.csv")
#niche_est <- niche_est %>%
#  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
#  distinct()
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))
full_abun_no_andro <- full_abun_data %>%
  filter(!(species == "Andropogon gerardi"))

# Calculating CTI
CTI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI_var = sum(percent_cover * (temp_niche - CTI)^2) / sum(percent_cover),
          CTI_sd = sqrt(CTI_var),
          CTI_skew = sum(percent_cover * (temp_niche - CTI)^3) / (sum(percent_cover) * CTI_sd^3),
          CTI_kurt = sum(percent_cover * (temp_niche - CTI)^4) / (sum(percent_cover) * CTI_sd^4) - 3,
          mean_C_temp_warmed = mean_C_temp_summer+2.5,
          disequilib = mean_C_temp_summer - CTI) %>%
  distinct()
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
# Note: code below overwrites disequilib formula from above; use this to test separate temps for amb and warm
# Calculate disequilibrium using ambient temps for amb, and warmed temps for elv?
# Need to figure out where elv temp data is; for now, this is a rough proxy of +2.5 above amb
#CTI$disequilib <- NA
#for (i in 1:nrow(CTI)) {
#  if (CTI$temp_treatment[i] == "HTelv") {
#    CTI$disequilib[i] <- CTI$mean_C_temp_warmed[i] - CTI$CTI[i]
#  } else if (CTI$temp_treatment[i] == "HTamb") {
#    CTI$disequilib[i] <- CTI$mean_C_temp_summer[i] - CTI$CTI[i]
#  }
#}

# Calculating CTI sensitivity (warmed - ambient)
CTI_sens <- CTI %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)
CTI_sens_filt <- CTI_filt %>%
  dplyr::select(year,plot,mean_C_temp_summer,temp_treatment,CTI) %>%
  group_by(year, mean_C_temp_summer,temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

# Calculating CPI
CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)
CPI_filt <- full_abun_no_andro %>%
  filter(Season == "August") %>%
  group_by(year,plot,mean_C_temp_summer,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3) %>%
  filter(!(CPI == "NaN"))

# CTI and CPI combined
CTI_CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,temp_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  pivot_wider(names_from = temp_treatment,
              values_from = c(CTI, CPI),
              names_sep = "_")
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


# Plot CTI
ggplot(CTI, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  #geom_smooth() +
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  #geom_line(aes(x = year, y = scaled_temp), color="blue") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

# Plot CTI sensitivity
ggplot(CTI_sens, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

# Plot CPI
ggplot(CPI, aes(x = year, y = CPI, color = water_treatment)) +
  geom_jitter(alpha = 0.2,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "crossbar",
               width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = water_treatment, group = water_treatment)) +
  theme_minimal() +
  scale_color_manual(values = c("H2Oamb" = "blue", "H2Oneg" = "red"))

# Arrow figure
ggplot(CTI_CPI) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "CTI and CPI: Ambient to Elevated") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()


# Models
cti_mod <- lmerTest::lmer(CTI_sd ~ temp_treatment*as.factor(year) + (1|plot), data=CTI)
anova(cti_mod)  
emm <- emmeans(cti_mod, ~ temp_treatment * year)
summary(emm)
pairs(emm, by = "year")


# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(CTI,paste(path_out,'CTI_teracon.csv'))
write.csv(CTI_sens,paste(path_out,'CTI_sens_teracon.csv'))
write.csv(CTI_CPI,paste(path_out,'CTI_CPI_teracon.csv'))
#write.csv(CTI,paste(path_out,'CTI_teracon_limited.csv'))
#write.csv(CTI_sens,paste(path_out,'CTI_sens_teracon_limited.csv'))
#write.csv(CTI_CPI,paste(path_out,'CTI_CPI_teracon_limited.csv'))
#write.csv(CTI,paste(path_out,'CTI_6month_teracon_limited.csv'))
#write.csv(CTI_sens,paste(path_out,'CTI_sens_6month_teracon_limited.csv'))
#write.csv(CTI_CPI,paste(path_out,'CTI_CPI_6month_teracon_limited.csv'))
write.csv(CTI_filt,paste(path_out,'CTI_teracon_nobluestem.csv'))
write.csv(CTI_sens_filt,paste(path_out,'CTI_sens_teracon_nobluestem.csv'))
write.csv(CTI_CPI_filt,paste(path_out,'CTI_CPI_teracon_nobluestem.csv'))

