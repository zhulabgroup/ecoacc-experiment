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
path_home = "/home/kcdobson"
setwd(path_data)

# Load in data
niche_est <- read.csv(" niche_estimate_teracon.csv")
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
teracon <- read.csv(" teracon_clean.csv")
teracon <- teracon %>%
  mutate(scaled_temp = mean_C_temp_summer/5)

# Combining teracon abundance data with niche estimate data
full_abun_data <- left_join(teracon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche))

# Calculating CTI and CPI
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
CTI_sens <- CTI %>%
  dplyr::select(year,plot,temp_treatment,CTI) %>%
  group_by(year, temp_treatment) %>%
  summarize(mean_cti = mean(CTI)) %>%
  pivot_wider(names_from = temp_treatment, values_from = mean_cti) %>%
  mutate(sensitivity = HTelv - HTamb)

# Note: code below overwrites disequilib formula from above; use this to test separate temps for amb and warm
# Calculate disequilibrium using ambient temps for amb, and warmed temps for elv?
# Need to figure out where elv temp data is; for now, this is a rough proxy of +2.5 above amb
CTI$disequilib <- NA

for (i in 1:nrow(CTI)) {
  if (CTI$temp_treatment[i] == "HTelv") {
    CTI$disequilib[i] <- CTI$mean_C_temp_warmed[i] - CTI$CTI[i]
  } else if (CTI$temp_treatment[i] == "HTamb") {
    CTI$disequilib[i] <- CTI$mean_C_temp_summer[i] - CTI$CTI[i]
  }
}

  
CPI <- full_abun_data %>%
  filter(Season == "August") %>%
  group_by(year,plot,water_treatment) %>%
  reframe(CPI = sum(percent_cover * precip_niche) / sum(percent_cover),
          CPI_var = sum(percent_cover * (precip_niche - CPI)^2) / sum(percent_cover),
          CPI_sd = sqrt(CPI_var),
          CPI_skew = sum(percent_cover * (precip_niche - CPI)^3) / (sum(percent_cover) * CPI_sd^3),
          CPI_kurt = sum(percent_cover * (precip_niche - CPI)^4) / (sum(percent_cover) * CPI_sd^4) - 3)


# Plot CTI
ggplot(CTI, aes(x = year, y = CTI_sd, color = temp_treatment, group=temp_treatment)) +
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


# Models
cti_mod <- lmerTest::lmer(CTI_sd ~ temp_treatment*as.factor(year) + (1|plot), data=CTI)
anova(cti_mod)  
emm <- emmeans(cti_mod, ~ temp_treatment * year)
summary(emm)
pairs(emm, by = "year")
