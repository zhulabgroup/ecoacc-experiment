# TITLE:          Figures of CTI over time
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Jan 2025

### Load packages
library(tidyverse)
library(ggpubr)
library(deeptime)
library(patchwork)
library(plotly)
library(maps)
library(mapdata)
library(gridExtra)
library(viridis)



### Set path to turbo to get teracon & biocon data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
tera <- read.csv(" teracon_clean.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
niche_est_tera <- read.csv(" teracon_niche.csv")
biocon <- read.csv(" CTI_biocon.csv")

### Set path to jrgce data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
CTI_jrgce <- read.csv(" CTI_jrgce.csv")
jrgce <- read.csv(" jrgce_clean.csv")
NPP_jrgce <- read.csv(" eco_response_jrgce.csv")
NPP_overall_jrgce <- read.csv(" eco_response_overall_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")
niche_est_jrgce <- read.csv(" jrgce_niche.csv")

### Set path to phace data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace.csv")
CTI_phace <- read.csv(" CTI_phace.csv")
phace <- read.csv(" phace_clean.csv")
NPP_phace <- read.csv(" eco_response_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace.csv")
niche_est_phace <- read.csv(" phace_niche.csv")

### Set path to b4warmed data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
CTI_sens_b4_cfc <- read.csv(" CTI_sens_b4warmed_cfc.csv")
CTI_b4_cfc <- read.csv(" CTI_b4warmed_cfc.csv")
b4_cfc <- read.csv(" b4warmed_cfc_clean.csv")
NPP_b4_cfc <- read.csv(" eco_response_b4warmed_cfc.csv")
NPP_overall_b4_cfc <- read.csv(" eco_response_overall_b4warmed_cfc.csv")
CTI_CPI_b4_cfc <- read.csv(" CTI_CPI_b4warmed_cfc.csv")
niche_est_b4_cfc <- read.csv(" b4warmed_cfc_niche.csv")

CTI_sens_b4_hwrc <- read.csv(" CTI_sens_b4warmed_hwrc.csv")
CTI_b4_hwrc <- read.csv(" CTI_b4warmed_hwrc.csv")
b4_hwrc <- read.csv(" b4warmed_hwrc_clean.csv")
NPP_b4_hwrc <- read.csv(" eco_response_b4warmed_hwrc.csv")
NPP_overall_b4_hwrc <- read.csv(" eco_response_overall_b4warmed_hwrc.csv")
CTI_CPI_b4_hwrc <- read.csv(" CTI_CPI_b4warmed_hwrc.csv")
niche_est_b4_hwrc <- read.csv(" b4warmed_hwrc_niche.csv")

### Set path to oklahoma data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
CTI_sens_ok <- read.csv(" CTI_sens_ok.csv")
CTI_ok <- read.csv(" CTI_ok.csv")
ok <- read.csv(" ok_clean.csv")
NPP_ok <- read.csv(" eco_response_ok.csv")
NPP_overall_ok <- read.csv(" eco_response_overall_ok.csv")
CTI_CPI_ok <- read.csv(" CTI_CPI_ok.csv")
niche_est_ok <- read.csv(" ok_niche.csv")

### Data for testing scale
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/"
setwd(path_data)
# PHACE
CTI_phace_uscan <- read.csv("PHACE/data_for_testing/ CTI_phace_uscan.csv")
CTI_phace_2000 <- read.csv("PHACE/ CTI_phace.csv")
# TeRaCON
CTI_tera_uscan <- read.csv("TeRaCON/data_for_testing/ CTI_teracon_uscan.csv")
CTI_tera_2000 <- read.csv("TeRaCON/ CTI_teracon.csv")
# JRGCE
CTI_jrgce_uscan <- read.csv("JRGCE/data_for_testing/ CTI_jrgce_uscan.csv")
CTI_jrgce_2000 <- read.csv("JRGCE/ CTI_jrgce.csv")

### Set path to model estimates
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
setwd(path_data)
# Load in data
tera_pred <- readRDS(" cti_pred_tera.rds")
jrgce_pred <- readRDS(" cti_pred_jrgce.rds")
phace_pred <- readRDS(" cti_pred_phace.rds")
b4_cfc_pred <- readRDS(" cti_pred_b4warmed_cfc.rds")
b4_hwrc_pred <- readRDS(" cti_pred_b4warmed_hwrc.rds")
ok_pred <- readRDS(" cti_pred_ok.rds")

tera_sens_pred <- readRDS(" cti_sens_pred_tera.rds")
jrgce_sens_pred <- readRDS(" cti_sens_pred_jrgce.rds")
phace_sens_pred <- readRDS(" cti_sens_pred_phace.rds")
b4_cfc_3.4_sens_pred <- readRDS(" cti_sens_pred_b4warmed_cfc_3.4.rds")
b4_cfc_1.7_sens_pred <- readRDS(" cti_sens_pred_b4warmed_cfc_1.7.rds")
b4_hwrc_3.4_sens_pred <- readRDS(" cti_sens_pred_b4warmed_hwrc_3.4.rds")
b4_hwrc_1.7_sens_pred <- readRDS(" cti_sens_pred_b4warmed_hwrc_1.7.rds")
ok_sens_pred <- readRDS(" cti_sens_pred_ok.rds")

# contour plots
contour_phace <- readRDS(" contours_treat_shortterm_phace.rds")
contour_cfc <- readRDS(" contours_treat_shortterm_cfc.rds")
contour_hwrc <- readRDS(" contours_treat_shortterm_hwrc.rds")
contour_ok <- readRDS(" contours_treat_shortterm_ok.rds")
contour_tera <- readRDS(" contours_treat_shortterm_tera.rds")
contour_jrgce <- readRDS(" contours_treat_shortterm_jrgce.rds")



### Conceptual Fig 1 ###
# Panel A data
df <- data.frame(Year = c(1, 2, 3, 4,
                          1, 2, 3, 4),
                 Temp_treatment = c("Ambient", "Ambient", "Ambient", "Ambient",
                                    "Warmed", "Warmed", "Warmed", "Warmed"),
                 CTI = c(5.1, 5.3, 5.5, 5.8,
                         5.1, 5.5, 5.8, 6.2))

# Panel B data
df2 <- data.frame(Year = c(1,2,3,4),
                  CTI_sens = c(0,0.2,0.3,0.4))

# Panel C data
df3 <- data.frame(Year = c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4),
                  spp = c("sp1","sp1","sp1","sp1",
                          "sp2","sp2","sp2","sp2",
                          "sp3","sp3","sp3","sp3",
                          "sp4","sp4","sp4","sp4",
                          "sp5","sp5","sp5","sp5",
                          "sp6","sp6","sp6","sp6",
                          "sp7","sp7","sp7","sp7",
                          "sp8","sp8","sp8","sp8",
                          "sp9","sp9","sp9","sp9",
                          "sp10","sp10","sp10","sp10"),
                  delta_abun = c(0.05,0.1,0.2,0.32,
                                 0,0.1,0.1,0.25,
                                 0.05,0,0.1,0.1,
                                 0,0.1,0.2,0.45,
                                 0,-0.1,0.2,0.3,
                                 0.01,0,0,0.1,
                                 0.02,-0.1,-0.1,-0.2,
                                 -0.05,-0.1,-0.4,-0.4,
                                 0,0.1,0.1,0.3,
                                 0,0,-0.15,-0.2),
                  temp_anom = c(3,3,3,3,
                                2,2,2,2,
                                1,1,1,1,
                                4,4,4,4,
                                1,1,1,1,
                                0,0,0,0,
                                -2,-2,-2,-2,
                                -3,-3,-3,-3,
                                -1,-1,-1,-1,
                                1,1,1,1))
df3 <- df3 %>%
  mutate(spp_contrib = delta_abun * temp_anom)
# Pulling out ranges of temp niches and slopes
temp_anom <- seq(min(df3$temp_anom)-2, max(df3$temp_anom)+2.25, length.out = 50)
delta_abun <- seq(min(df3$delta_abun)-0.5, max(df3$delta_abun)+0.5, length.out = 50)
# Merge data into dataframe
grid_df <- expand.grid(temp_anom = temp_anom, delta_abun = delta_abun)
grid_df <- grid_df %>%
  mutate(spp_contrib = temp_anom * delta_abun)

# Panel D data
df4 <- df3 %>%
  group_by(spp, temp_anom) %>% 
  do({
    mod <- lm(delta_abun ~ Year, data = .)
    tidy_mod <- tidy(mod)
    # extract slope (coefficient for Year)
    slope <- tidy_mod$estimate[tidy_mod$term == "Year"]
    tibble(slope = slope)
  }) %>%
  ungroup()
df4 <- df4 %>%
  mutate(spp_contrib = slope * temp_anom)
# Pulling out ranges of temp niches and slopes
temp_anom2 <- seq(min(df4$temp_anom)-0.5, max(df4$temp_anom)+0.5, length.out = 50)
slope <- seq(min(df4$slope)-0.05, max(df4$slope)+0.05, length.out = 50)
# Merge data into dataframe
grid_df2 <- expand.grid(temp_anom = temp_anom2, slope = slope)
grid_df2 <- grid_df2 %>%
  mutate(spp_contrib = temp_anom * slope)

# Panel A
png("panel_a.png", units="in", width=5, height=4, res=300)
ggplot(df, aes(x = Year, y = CTI, fill = Temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  coord_cartesian(ylim = c(4, 6.3)) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient", "Warmed"),
                    values = c("blue", "red")) +
  labs(x = "Year", y = "CTI") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        #axis.title.x = element_blank(),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) +
  guides(shape = guide_legend(order = 1),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1))
dev.off()

# Panel B
png("panel_b.png", units="in", width=5, height=4, res=300)
ggplot(df2, aes(x = Year, y = CTI_sens)) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI") +
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        #axis.title.x = element_blank(),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) +
  guides(shape = guide_legend(order = 1),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1))
dev.off()

# Panel C
names <- c(
  `1` = "Year 1",
  `2` = "Year 2",
  `3` = "Year 3",
  `4` = "Year 4"
)
png("panel_c.png", units="in", width=5, height=4, res=300)
ggplot(df3, aes(x = temp_anom, y = delta_abun)) +
  facet_wrap(.~Year, labeller = as_labeller(names)) +
  geom_tile(data=grid_df,aes(x = temp_anom, y = delta_abun, fill = spp_contrib)) +
  stat_ellipse(level = 0.95, color = "black",alpha=0.4) + # Add an ellipse
  geom_point() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Species\ncontribution\nto ΔCTI"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    x = "Species temperature anomaly (°C)",
    y = "Δ Abundance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        #axis.title.x = element_blank(),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) +
  guides(shape = guide_legend(order = 1),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  coord_cartesian(expand = FALSE) # Ensure no expansion on axes
dev.off()

# Panel D
png("panel_d.png", units="in", width=5, height=4, res=300)
ggplot(df4, aes(x = temp_anom, y = slope)) +
  geom_tile(data=grid_df2,aes(x = temp_anom, y = slope, fill = spp_contrib)) +
  #stat_ellipse(level = 0.95, color = "black",alpha=0.4) + # Add an ellipse
  geom_point(size=3) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0,
    name = "Species\ncontribution\nto βCTI"
  ) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    x = "Species temperature anomaly (°C)",
    y = "β Abundance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        #axis.title.x = element_blank(),
        legend.text=element_text(size=13),
        legend.title=element_text(size=13)) +
  guides(shape = guide_legend(order = 1),
         size = guide_legend(order = 1),
         alpha = guide_legend(order = 1)) +
  coord_cartesian(expand = FALSE) # Ensure no expansion on axes
dev.off()



### Make unique species list
species_lists <- list(
  b4_cfc = unique(niche_est_b4_cfc$species),
  b4_hwrc = unique(niche_est_b4_hwrc$species),
  jrgce = unique(niche_est_jrgce$species),
  ok = unique(niche_est_ok$species),
  phace = unique(niche_est_phace$species),
  tera = unique(niche_est_tera$species)
)
# Flatten the list into a long format: species - dataframe pairs
species_df_pairs <- stack(species_lists) %>% 
  rename(species = values, dataframe = ind)
# Summarize: number of dataframes and which ones for each species
species_summary <- species_df_pairs %>%
  distinct(species, dataframe) %>%  # remove duplicates
  group_by(species) %>%
  summarize(
    num_dataframes = n(),
    dataframes = paste(sort(unique(dataframe)), collapse = ", "),
    .groups = "drop"
  )



### Mean and SE for CTI in each year
CTI_mean_tera <- CTI_teracon %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_jrgce <- CTI_jrgce %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_phace <- CTI_phace %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_b4_cfc <- CTI_b4_cfc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_b4_hwrc <- CTI_b4_hwrc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_mean_ok <- CTI_ok %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))



### CTI sensitivity figures
CTI_sens_tera_plot <- ggplot(CTI_sens_teracon, aes(x = year, y = sens_scaled)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_scaled, ymin = sens_scaled-SE_diff_scaled, ymax = sens_scaled+SE_diff_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_smooth(method='lm',color="red") +
  labs(x = "Year", y = "ΔCTI / °C",title="TeRaCON") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_sens_tera_plot2 <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity, ymin = sensitivity-SE_diff, ymax = sensitivity+SE_diff), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="black") +
  geom_point(shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="black",fill="black") +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI",title="(i) TeRaCON") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_sens_jrgce_plot <- ggplot(CTI_sens_jrgce, aes(x = year, y = sens_scaled)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_scaled, ymin = sens_scaled-SE_diff_scaled, ymax = sens_scaled+SE_diff_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(aes(x = year, y = sens_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_smooth(method='lm',color="red") +
  labs(x = "Year", y = "ΔCTI / °C", title = "JRGCE") +
  #scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_sens_jrgce_plot2 <- ggplot(CTI_sens_jrgce, aes(x = year, y = sensitivity)) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity, ymin = sensitivity-SE_diff, ymax = sensitivity+SE_diff), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="black",fill="black") +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI", title = "(g) JRGCE") +
  scale_x_continuous(breaks = seq(1999, 2014, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_sens_phace_plot <- ggplot(CTI_sens_phace, aes(x = year, y = sens_scaled)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_scaled, ymin = sens_scaled-SE_diff_scaled, ymax = sens_scaled+SE_diff_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(aes(x = year, y = sens_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_smooth(method='lm',color="red") +
  labs(x = "Year", y = "ΔCTI / °C",title="PHACE") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_sens_phace_plot2 <- ggplot(CTI_sens_phace, aes(x = year, y = sensitivity)) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity, ymin = sensitivity-SE_diff, ymax = sensitivity+SE_diff), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="black",fill="black") +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI",title="(h) PHACE") +
  scale_x_continuous(breaks = seq(2007, 2013,by=1)) +
  theme_classic() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_sens_b4_cfc_plot <- ggplot(CTI_sens_b4_cfc) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_high_temp_scaled,
                    ymin = sens_high_temp_scaled-SE_diff_high_temp_scaled,
                    ymax = sens_high_temp_scaled+SE_diff_high_temp_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(aes(x = year, y = sens_high_temp_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_errorbar(aes(x = year, y = sens_med_temp_scaled,
                    ymin = sens_med_temp_scaled-SE_diff_med_temp_scaled,
                    ymax = sens_med_temp_scaled+SE_diff_med_temp_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="orange") +
  geom_point(aes(x = year, y = sens_med_temp_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="orange",fill="orange") +
  geom_smooth(aes(x = year, y = sens_high_temp_scaled),method='lm',color="red") +
  geom_smooth(aes(x = year, y = sens_med_temp_scaled),method='lm',color="orange") +
  labs(x = "Year", y = "ΔCTI / °C",title = "B4WarmED CFC") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 4)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

linetypes <- c("Warmed - Ambient" = "solid", "Intermediate - Ambient" = "dashed")
CTI_sens_b4_cfc_plot2 <- ggplot(CTI_sens_b4_cfc) +
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

CTI_sens_b4_hwrc_plot <- ggplot(CTI_sens_b4_hwrc) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_high_temp_scaled,
                    ymin = sens_high_temp_scaled-SE_diff_high_temp_scaled,
                    ymax = sens_high_temp_scaled+SE_diff_high_temp_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(aes(x = year, y = sens_high_temp_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_errorbar(aes(x = year, y = sens_med_temp_scaled,
                    ymin = sens_med_temp_scaled-SE_diff_med_temp_scaled,
                    ymax = sens_med_temp_scaled+SE_diff_med_temp_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="orange") +
  geom_point(aes(x = year, y = sens_med_temp_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="orange",fill="orange") +
  geom_smooth(aes(x = year, y = sens_high_temp_scaled),method='lm',color="red") +
  geom_smooth(aes(x = year, y = sens_med_temp_scaled),method='lm',color="orange") +
  labs(x = "Year", y = "ΔCTI / °C",title = "B4WarmED HWRC") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 4)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_sens_b4_hwrc_plot2 <- ggplot(CTI_sens_b4_hwrc) +
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

CTI_sens_ok_plot <- ggplot(CTI_sens_ok, aes(x = year, y = sens_scaled)) +
  geom_hline(yintercept=0,linetype="dashed",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sens_scaled, ymin = sens_scaled-SE_diff_scaled, ymax = sens_scaled+SE_diff_scaled), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="red") +
  geom_point(aes(x = year, y = sens_scaled), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="red",fill="red") +
  geom_smooth(method='lm',color="red") +
  labs(x = "Year", y = "ΔCTI / °C",title = "Oklahoma") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_sens_ok_plot2 <- ggplot(CTI_sens_ok, aes(x = year, y = sensitivity)) +
  geom_hline(yintercept=0,linetype="dotted",alpha=0.5) +
  geom_errorbar(aes(x = year, y = sensitivity, ymin = sensitivity-SE_diff, ymax = sensitivity+SE_diff), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4,color="black") +
  geom_point(aes(x = year, y = sensitivity), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4,color="black",fill="black") +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "ΔCTI",title = "(j) Oklahoma") +
  scale_x_continuous(breaks = seq(2000, 2013, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))



### CTI figures
CTI_mean_tera_plot <- ggplot(CTI_mean_tera, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(8.6, 9.2)) +
  labs(x = "Year", y = "CTI", title = "(c) TeRaCON") +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient", "Warmed"),
                    values = c("blue", "red")) +
  scale_x_discrete(breaks = seq(2012, 2023, by = 2)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12))
CTI_mean_tera_plot2 <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_tera, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_tera, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method='lm') +
  labs(x = "Year", y = "CTI",title = "(c) TeRaCON") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_mean_jrgce_plot <- ggplot(CTI_mean_jrgce, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(14.2, 15.8)) +
  labs(x = "Year", y = "CTI", title = "(a) JRGCE") +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient", "Warmed"),
                    values = c("blue", "red")) +
  scale_x_discrete(breaks = seq(1999, 2014, by = 2)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12))
CTI_mean_jrgce_plot2 <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_jrgce, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_jrgce, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CTI",title = "(a) JRGCE") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_mean_phace_plot <- ggplot(CTI_mean_phace, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(7.5, 9)) +
  labs(x = "Year", y = "CTI", title = "(b) PHACE") +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient", "Warmed"),
                    values = c("blue", "red")) +
  #scale_x_discrete(breaks = seq(1999, 2014, by = 2)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12))
CTI_mean_phace_plot2 <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_phace, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_phace, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CTI",title = "(b) PHACE") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

CTI_mean_b4_cfc$temp_treatment <- factor(
  CTI_mean_b4_cfc$temp_treatment,
  levels = c("amb", "1.7", "3.4")
)
CTI_mean_b4_cfc_plot <- ggplot(CTI_mean_b4_cfc, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI),
                position = position_dodge(width = 0.8),
                width = 0.2, alpha = 0.8) +
  labs(x = "Year", y = "CTI", title = "(e) B4WarmED CFC") +
  scale_fill_manual(name = "Treatment",
                    breaks = c("amb", "1.7", "3.4"),
                    labels = c("Ambient", "Intermediate", "Warmed"),
                    values = c("blue", "orange", "red")) +
  coord_cartesian(ylim = c(6.5, 7.6)) +
  scale_x_discrete(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
CTI_mean_b4_cfc_plot2 <- ggplot(CTI_b4_cfc, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_cfc, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_b4_cfc, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CTI",title = "(e) B4WarmED CFC") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                    breaks=c("amb","1.7","3.4"),
                    labels = c("Ambient","Intermediate","Warmed"),
                    values = c("blue","orange","red")) +
  theme_classic()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))

CTI_mean_b4_hwrc$temp_treatment <- factor(
  CTI_mean_b4_hwrc$temp_treatment,
  levels = c("amb", "1.7", "3.4")
)
CTI_mean_b4_hwrc_plot <- ggplot(CTI_mean_b4_hwrc, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI),
                position = position_dodge(width = 0.8),
                width = 0.2, alpha = 0.8) +
  labs(x = "Year", y = "CTI", title = "(f) B4WarmED HWRC") +
  scale_fill_manual(name = "Treatment",
                    breaks = c("amb", "1.7", "3.4"),
                    labels = c("Ambient", "Intermediate", "Warmed"),
                    values = c("blue", "orange", "red")) +
  coord_cartesian(ylim = c(6.5, 7.6)) +
  scale_x_discrete(breaks = seq(2008, 2020, by = 2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
CTI_mean_b4_hwrc_plot2 <- ggplot(CTI_b4_hwrc, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_hwrc, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_b4_hwrc, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CTI",title = "(f) B4WarmED HWRC") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                    breaks=c("amb","1.7","3.4"),
                    labels = c("Ambient","Intermediate","Warmed"),
                    values = c("blue","orange","red")) +
  theme_classic()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))

CTI_mean_ok_plot <- ggplot(CTI_mean_ok, aes(x = factor(year), y = mean_CTI, fill = temp_treatment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(15, 16.4)) +
  labs(x = "Year", y = "CTI", title = "(d) Oklahoma") +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient", "Warmed"),
                    values = c("blue", "red")) +
  scale_x_discrete(breaks = seq(2000, 2013, by = 2)) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12))
CTI_mean_ok_plot2 <- ggplot(CTI_ok, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_ok, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_ok, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CTI",title = "(d) Oklahoma") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_classic()+
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))



### Combining scales and calculating sensitivity
## Adding scale
CTI_phace_2000$scale <- "2000 km"
CTI_phace_uscan$scale <- "U.S. & Canada"
CTI_tera_2000$scale <- "2000 km"
CTI_tera_uscan$scale <- "U.S. & Canada"
CTI_jrgce_2000$scale <- "2000 km"
CTI_jrgce_uscan$scale <- "U.S. & Canada"

## Selecting columns
CTI_phace_2000 <- CTI_phace_2000 %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)
CTI_phace_uscan <- CTI_phace_uscan %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)
CTI_tera_2000 <- CTI_tera_2000 %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)
CTI_tera_uscan <- CTI_tera_uscan %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)
CTI_jrgce_2000 <- CTI_jrgce_2000 %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)
CTI_jrgce_uscan <- CTI_jrgce_uscan %>%
  dplyr::select(year, plot, temp_treatment, CTI, scale)

## Fixing teracon labeling
CTI_tera_uscan$temp_treatment[CTI_tera_uscan$temp_treatment == "HTamb"] <- "ambient"
CTI_tera_uscan$temp_treatment[CTI_tera_uscan$temp_treatment == "HTelv"] <- "warmed"

## Combining scales for each experiment
phace_scale <- full_join(CTI_phace_2000,CTI_phace_uscan,by=c("year","plot","temp_treatment"))
tera_scale <- full_join(CTI_tera_2000,CTI_tera_uscan,by=c("year","plot","temp_treatment"))
jrgce_scale <- full_join(CTI_jrgce_2000,CTI_jrgce_uscan,by=c("year","plot","temp_treatment"))

## Making long format for plotting
phace_long <- phace_scale %>%
  pivot_longer(
    cols = c(CTI.x, scale.x, CTI.y, scale.y),
    names_to = c(".value", "set"),   # .value keeps CTI & scale as separate columns
    names_pattern = "(CTI|scale)\\.*(.*)"  # captures "CTI"/"scale" and suffix ("", x, y)
  )
phace_long <- phace_long %>%
  select(-set)

tera_long <- tera_scale %>%
  pivot_longer(
    cols = c(CTI.x, scale.x, CTI.y, scale.y),
    names_to = c(".value", "set"),   # .value keeps CTI & scale as separate columns
    names_pattern = "(CTI|scale)\\.*(.*)"  # captures "CTI"/"scale" and suffix ("", x, y)
  )
tera_long <- tera_long %>%
  select(-set)

jrgce_long <- jrgce_scale %>%
  pivot_longer(
    cols = c(CTI.x, scale.x, CTI.y, scale.y),
    names_to = c(".value", "set"),   # .value keeps CTI & scale as separate columns
    names_pattern = "(CTI|scale)\\.*(.*)"  # captures "CTI"/"scale" and suffix ("", x, y)
  )
jrgce_long <- jrgce_long %>%
  select(-set)

## Calculating mean CTI and sensitivity for each experiment
CTI_mean_phace <- phace_long %>%
  group_by(year,temp_treatment,scale) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_sens_phace <- CTI_mean_phace %>%
  select(year,temp_treatment,scale,mean_CTI) %>%
  pivot_wider(
    names_from = temp_treatment,
    values_from = mean_CTI
  ) %>%
  mutate(CTI_sens = warmed - ambient)

CTI_mean_tera <- tera_long %>%
  group_by(year,temp_treatment,scale) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n()))
CTI_sens_tera <- CTI_mean_tera %>%
  select(year,temp_treatment,scale,mean_CTI) %>%
  pivot_wider(
    names_from = temp_treatment,
    values_from = mean_CTI
  ) %>%
  mutate(CTI_sens = warmed - ambient)

CTI_mean_jrgce <- jrgce_long %>%
  group_by(year,temp_treatment,scale) %>%
  summarize(mean_CTI = mean(CTI),
            se_CTI = sd(CTI)/sqrt(n())) %>%
  filter(!is.na(mean_CTI))
CTI_sens_jrgce <- CTI_mean_jrgce %>%
  select(year,temp_treatment,scale,mean_CTI) %>%
  pivot_wider(
    names_from = temp_treatment,
    values_from = mean_CTI
  ) %>%
  mutate(CTI_sens = warmed - ambient) %>%
  filter(!is.na(CTI_sens))

## Labels for plotting
names <- c(
  `ambient` = "Ambient",
  `warmed` = "Warmed"
)

## Plot
a <- ggplot(CTI_mean_phace, aes(x = year, y = mean_CTI, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_wrap(~ temp_treatment, labeller = as_labeller(names)) +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "CTI",color="Scale",title="PHACE") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 10),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))
b <- ggplot(CTI_sens_phace, aes(x = year, y = CTI_sens, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "Δ CTI",color="Scale",title="PHACE") +
  theme_classic()  +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12))

c <- ggplot(CTI_mean_tera, aes(x = year, y = mean_CTI, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_wrap(~ temp_treatment, labeller = as_labeller(names)) +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "CTI",color="Scale",title="TeRaCON") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 10),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))
d <- ggplot(CTI_sens_tera, aes(x = year, y = CTI_sens, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "Δ CTI",color="Scale",title="TeRaCON") +
  theme_classic()   +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12))

e <- ggplot(CTI_mean_jrgce, aes(x = year, y = mean_CTI, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  facet_wrap(~ temp_treatment, labeller = as_labeller(names)) +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "CTI",color="Scale",title="JRGCE") +
  theme_classic() +
  theme(strip.text.x = element_text(size = 10),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))
f <- ggplot(CTI_sens_jrgce, aes(x = year, y = CTI_sens, color = scale)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_color_manual(values = c("2000 km" = "#2C5F2D", "U.S. & Canada" = "#97BC62")) +
  labs(x = "Year", y = "Δ CTI",color="Scale",title="JRGCE") +
  theme_classic()   +
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12))

png("cti_scale.png", units="in", width=8, height=8, res=300)
wrap_plots(a,b,c,d,e,f,ncol=2) +
  plot_layout(guides = "collect",axis_titles = "collect")
dev.off()




##### Merging and saving figures #####
#png("cti_sens.png", units="in", width=10, height=6, res=300)
wrap_plots(CTI_sens_jrgce_plot2,CTI_sens_phace_plot2,CTI_sens_tera_plot2,
           CTI_sens_ok_plot2,CTI_sens_b4_cfc_plot,CTI_sens_b4_hwrc_plot,
           ncol = 3) + plot_layout(guides = "collect",axis_titles = "collect")
#dev.off()

### CTI and CTI warmed-ambient
combined_sens <- wrap_plots(CTI_mean_jrgce_plot,CTI_sens_jrgce_plot,
                            CTI_mean_phace_plot,CTI_sens_phace_plot,
                            CTI_mean_tera_plot,CTI_sens_tera_plot,
                            CTI_mean_ok_plot,CTI_sens_ok_plot,
                            CTI_mean_b4_cfc_plot,CTI_sens_b4_cfc_plot,
                            CTI_mean_b4_hwrc_plot,CTI_sens_b4_hwrc_plot,
                            ncol=2,nrow=6) +
  plot_layout(guides = "collect")+
  theme(plot.margin = margin(0.2,0.1,0.1,0.1,"cm")) 

png("cti_sens.png", units="in", width=10, height=12, res=300)
wrap_plots(CTI_mean_jrgce_plot,CTI_sens_jrgce_plot2,
           CTI_mean_phace_plot,CTI_sens_phace_plot2,
           CTI_mean_tera_plot,CTI_sens_tera_plot2,
           CTI_mean_ok_plot,CTI_sens_ok_plot2,
           CTI_mean_b4_cfc_plot,CTI_sens_b4_cfc_plot2,
           CTI_mean_b4_hwrc_plot,CTI_sens_b4_hwrc_plot2,
           ncol=2,nrow=6) +
  plot_layout(guides = "collect",axis_titles = "collect")+
  theme(plot.margin = margin(0.2,0.1,0.1,0.1,"cm")) 
dev.off()

png("cti_sens_scaled.png", units="in", width=7, height=9, res=300)
wrap_plots(CTI_sens_jrgce_plot,
           CTI_sens_phace_plot,
           CTI_sens_tera_plot,
           CTI_sens_ok_plot,
           CTI_sens_b4_cfc_plot,
           CTI_sens_b4_hwrc_plot,
           ncol=2) +
  plot_layout(guides = "collect",axis_titles = "collect")+
  theme(plot.margin = margin(0.2,0.1,0.1,0.1,"cm")) 
dev.off()


### Long-term CTI plot for Biocon
biocon_CTI <- ggplot(biocon, aes(x = year, y = CTI, color=temp_treatment)) +
  geom_vline(xintercept = 2012, linetype = "dashed") +
  geom_smooth() +
  labs(x = "Year", y = "CTI") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()




# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(combined_CTI, paste(path_out,'cti.rds'))
saveRDS(combined_CTI_sens, paste(path_out,'sens.rds'))
saveRDS(combined_sens, paste(path_out,'cti_sens.rds'))
saveRDS(biocon_CTI, paste(path_out,'biocon_cti.rds'))
saveRDS(temp_CTI, paste(path_out,'temperature_and_cti.rds'))
saveRDS(arrows, paste(path_out,'treatment_arrows.rds'))

saveRDS(CTI_sens_jrgce_plot, paste(path_out,'jrgce_cti_sens.rds'))





