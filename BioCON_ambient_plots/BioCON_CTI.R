# TITLE:          BioCON CTI
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate data for teracon (same spp as biocon)
# DATA OUTPUT:    CTI calculations
# PROJECT:        EcoAcc
# DATE:           Nov 2024


# Load packages
library(tidyverse)
library(lmerTest)
library(emmeans)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)

# Load in data
niche_est <- read.csv(" teracon_niche.csv") # using ecoregion 8 niche estimates
niche_est <- niche_est %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
biocon <- read.csv(" biocon_clean.csv")
teracon <- read.csv(" teracon_clean.csv")

# Combining biocon abundance data with niche estimate data
full_abun_data <- left_join(biocon, niche_est, by = "species")
full_abun_data <- full_abun_data %>%
  filter(!is.na(percent_cover)) %>%
  filter(!is.na(temp_niche)) %>%
  filter(!is.na(precip_niche)) %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "elv"), "warmed", "ambient"))

# Subsetting to plots used in TeRaCON
teracon_plots <- unique(teracon$plot)
biocon_plots <- full_abun_data %>%
  filter(plot %in% teracon_plots)


### All plots
# Calculating CTI
CTI <- biocon_plots %>%
  filter(Season == "August") %>%
  group_by(year,plot,temp_treatment) %>%
  reframe(CTI = sum(percent_cover * temp_niche) / sum(percent_cover)) %>%
  distinct()

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
write.csv(CTI,paste(path_out,'CTI_biocon.csv'),row.names=F)

# Plot CTI
ggplot(CTI, aes(x = year, y = CTI, color=temp_treatment)) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_smooth() +
  labs(x = "Year", y = "CTI") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()


# Models
mod1 <- lm(CTI ~ as.factor(year), data = CTI)
anova(mod1)
summary(mod1)
emm <- emmeans(mod1, ~ year)
pairs(emm, adjust = "tukey")



### Comparing 4, 9, and 16 species plots
species4 <- full_abun_data %>%
  filter(CountOfSpecies == 4)
# Calculating CTI
CTI4 <- species4 %>%
  filter(Season == "August") %>%
  group_by(year) %>%
  reframe(CTI4 = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI4_var = sum(percent_cover * (temp_niche - CTI4)^2) / sum(percent_cover),
          CTI4_sd = sqrt(CTI4_var)) %>%
  distinct()

species9 <- full_abun_data %>%
  filter(CountOfSpecies == 9)# Calculating CTI
CTI9 <- species9 %>%
  filter(Season == "August") %>%
  group_by(year) %>%
  reframe(CTI9 = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI9_var = sum(percent_cover * (temp_niche - CTI9)^2) / sum(percent_cover),
          CTI9_sd = sqrt(CTI9_var)) %>%
  distinct()

species16 <- full_abun_data %>%
  filter(CountOfSpecies == 16)
# Calculating CTI
CTI16 <- species16 %>%
  filter(Season == "August") %>%
  group_by(year) %>%
  reframe(CTI16 = sum(percent_cover * temp_niche) / sum(percent_cover),
          CTI16_var = sum(percent_cover * (temp_niche - CTI16)^2) / sum(percent_cover),
          CTI16_sd = sqrt(CTI16_var)) %>%
  distinct()

spp_type_CTI <- merge(CTI4, CTI9, by=c("year"))
spp_type_CTI <- merge(spp_type_CTI, CTI16, by=c("year"))
spp_type_CTI_long <- spp_type_CTI %>%
  pivot_longer(cols = starts_with("CTI"), names_to = "type", values_to = "CTI") %>%
  filter(type == "CTI4" | type == "CTI9" | type == "CTI16")


# Plot with all 3 plot types
ggplot(spp_type_CTI_long, aes(x = year, y = CTI, color = type)) +
  geom_line(size = 1) +
  geom_line(data = biocon[!is.na(biocon$mean_C_temp_summer),], 
            aes(x = year, y = mean_C_temp_summer/2, color = "Temperature / 2"), 
            size = 1) +
  labs(x = "Year",
       y = "CTI",
       color = "Type") +  # Add a title to the legend
  theme_minimal() +
  scale_color_manual(values = c("CTI4" = "#264653", "CTI9" = "#2a9d8f", "CTI16" = "#e9c46a","Temperature / 2" = "#ff5733"),
                     breaks = c("CTI4", "CTI9", "CTI16","Temperature / 2"))

