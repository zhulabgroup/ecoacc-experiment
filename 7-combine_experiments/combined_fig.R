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

