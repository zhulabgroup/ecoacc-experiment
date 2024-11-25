# TITLE:          Testing various data calculations/variable selection
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Data from experiments 
# DATA OUTPUT:    Comparisons of various variable manipulations
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)
library(ggpubr)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
# Data using global spp occurrences from GBIF + year-round MAT:
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
# Data using ecoregion 8 spp occurrences from GBIF + year-round MAT:
CTI_sens_teracon_lim <- read.csv(" CTI_sens_teracon_limited.csv")
CTI_teracon_lim <- read.csv(" CTI_teracon_limited.csv")
CTI_CPI_teracon_lim <- read.csv(" CTI_CPI_teracon_limited.csv")
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
NPP_jrgce <- read.csv(" eco_response_jrgce.csv")
NPP_overall_jrgce <- read.csv(" eco_response_overall_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")

# Set path to data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace_limited.csv")
CTI_phace <- read.csv(" CTI_phace_limited.csv")
NPP_phace <- read.csv(" eco_response_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace_limited.csv")


##### Fig: teracon --> comparing global GBIF occurrence data w/ ecoregion 8 GBIF occurrence data
CTI_global_tera <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="All GBIF occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_eco8_tera <- ggplot(CTI_teracon_lim, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="Ecoregion 8 occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

arrow_global <- ggplot(CTI_CPI_teracon) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "All GBIF occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_eco8 <- ggplot(CTI_CPI_teracon_lim) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "Ecoregion 8 occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

CTI_global_smooth <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "All GBIF occurrences") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
CTI_eco8_smooth <- ggplot(CTI_sens_teracon_lim, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "Ecoregion 8 occurrences") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_global_tera, arrow_global, CTI_global_smooth,
          CTI_eco8_tera, arrow_eco8, CTI_eco8_smooth,
          ncol = 3, nrow=2)



##### Fig: teracon --> comparing year-round temps w/ 6-month (March-Aug) temps for niche calculation
CTI_yearround_tera <- ggplot(CTI_teracon_lim, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  ylim(7,22) +
  labs(title="Year-round climate") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_6month_tera <- ggplot(CTI_teracon_6month, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  ylim(7,22) +
  labs(title="6-month climate") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

arrow_yearround <- ggplot(CTI_CPI_teracon_lim) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "Year-round climate") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_6month <- ggplot(CTI_CPI_teracon_6month) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "6-month climate") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

CTI_yearround_smooth <- ggplot(CTI_sens_teracon_lim, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "Year-round climate") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.1) +
  theme_bw()
CTI_6month_smooth <- ggplot(CTI_sens_teracon_6month, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "6-month climate") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.1) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_yearround_tera, arrow_yearround, CTI_yearround_smooth,
          CTI_6month_tera, arrow_6month, CTI_6month_smooth,
          ncol = 3, nrow=2)



##### Fig: teracon --> w/ and w/o big bluestem
CTI_blue_tera <- ggplot(CTI_teracon_6month, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  ylim(7,22) +
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
  ylim(7,22) +
  labs(title="w/o big bluestem") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

arrow_blue <- ggplot(CTI_CPI_teracon_6month) +
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

CTI_blue_smooth <- ggplot(CTI_sens_teracon_6month, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/ big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.3) +
  theme_bw()
CTI_noblue_smooth <- ggplot(CTI_sens_teracon_noblue, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/o big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.3) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_blue_tera, arrow_blue, CTI_blue_smooth,
          CTI_noblue_tera, arrow_noblue, CTI_noblue_smooth,
          ncol = 3, nrow=2)



##### Fig: teracon --> correlating biomass w/ CTI w/ and w/o big bluestem
# TeRaCON
CTI_yearly_avg_tera <- CTI_teracon_6month %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon <- left_join(CTI_yearly_avg_tera, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter <- ggscatter(NPP_CTI_teracon, x = "mean_ab_bio", y = "mean_CTI", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomass", ylab = "TeRaCON\nCTI")
CTI_yearly_avg_tera_noblue <- CTI_teracon_noblue %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon_noblue <- left_join(CTI_yearly_avg_tera_noblue, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter_noblue <- ggscatter(NPP_CTI_teracon_noblue, x = "mean_ab_bio", y = "mean_CTI", 
                                 add = "reg.line", conf.int = TRUE, 
                                 cor.coef = TRUE, cor.method = "pearson",
                                 xlab = "Biomass", ylab = "TeRaCON (No bluestem)\nCTI")

# Combine into one figure
ggarrange(tera_scatter,tera_scatter_noblue,
          nrow=2)
