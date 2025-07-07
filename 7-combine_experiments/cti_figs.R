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




### Temperature niche histograms
# Combining teracon abundance data with niche estimate data
full_abun_tera <- left_join(tera, niche_est_tera, by = "species")
# Data for one year and plot
full_abun_tera <- full_abun_tera %>%
  filter(year == 2012 & plot == 7)
tera_niche_hist <- ggplot(niche_est_tera, aes(x = niche)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.5) +
  labs(x = "Temperature Niche", y = "Frequency", title = "TeRaCON") +
  theme_bw()




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
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
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
  theme_bw() +
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))



### CTI figures
CTI_tera_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.2) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "TeRaCON\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()
CTI_facet_tera <- ggplot(CTI_mean_tera, aes(x = temp_treatment, y = mean_CTI, fill = temp_treatment)) +
  facet_wrap(.~year) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(8.6, 9.2)) +
  labs(x = "Treatment", y = "CTI", title = "TeRaCON") +
  scale_fill_manual(name = "Treatment",
                    labels = c("ambient" = "Ambient", "warmed"="Warmed"),
                    values = c("blue3", "red4")) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "warmed"="Warmed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "none")
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
  theme_bw() +
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
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_box_tera <- ggplot(CTI_teracon, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "TeRaCON\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")


CTI_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.1) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "JRGCE\nCTI") +
  #scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()
CTI_facet_jrgce <- ggplot(CTI_mean_jrgce, aes(x = temp_treatment, y = mean_CTI, fill = temp_treatment)) +
  facet_wrap(.~year) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(14.2, 15.8)) +
  labs(x = "Treatment", y = "CTI", title = "JRGCE") +
  scale_fill_manual(name = "Treatment",
                    labels = c("ambient" = "Ambient", "warmed"="Warmed"),
                    values = c("blue3", "red4")) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "warmed"="Warmed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "none")
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
  theme_bw() +
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
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_box_jrgce <- ggplot(CTI_jrgce, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "JRGCE\nCTI") +
  #scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")


CTI_phace_plot <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.2) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "PHACE\nCTI") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()
CTI_facet_phace <- ggplot(CTI_mean_phace, aes(x = temp_treatment, y = mean_CTI, fill = temp_treatment)) +
  facet_wrap(.~year) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = mean_CTI - se_CTI, ymax = mean_CTI + se_CTI), 
                width = 0.2, 
                position = position_dodge(width = 0.9), 
                alpha = 0.7) +
  coord_cartesian(ylim = c(7.5, 9)) +
  labs(x = "Treatment", y = "CTI", title = "PHACE") +
  scale_fill_manual(name = "Treatment",
                    labels = c("ambient" = "Ambient", "warmed"="Warmed"),
                    values = c("blue3", "red4")) +
  scale_x_discrete(labels=c("ambient" = "Ambient", "warmed"="Warmed")) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 15, face = "bold"),
        strip.text.x = element_text(size = 12),
        plot.title = element_text(size=15, face="bold"),
        legend.position = "none")
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
  theme_bw() +
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
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_box_phace <- ggplot(CTI_phace, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "PHACE\nCTI") +
  # scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")


CTI_b4_cfc_plot <- ggplot(CTI_b4_cfc_rem, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.2) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "B4WarmED CFC\nCTI") +
  #scale_x_continuous(breaks = seq(2008, 2021, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","3.4"),
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() 
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
  theme_bw() +
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
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))
CTI_box_cfc <- ggplot(CTI_b4_cfc, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "B4WarmED CFC\nCTI") +
  # scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  theme_bw() +
  theme(legend.position = "none")


CTI_b4_hwrc_plot <- ggplot(CTI_b4_hwrc_rem, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.2) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "B4WarmED HWRC\nCTI") +
  #scale_x_continuous(breaks = seq(2008, 2021, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","3.4"),
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()
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
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13))
CTI_mean_b4_hwrc_plot2 <- ggplot(CTI_b4_hwrc_rem2, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_hwrc_rem2, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_b4_hwrc_rem2, 
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
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=13))
CTI_box_hwrc <- ggplot(CTI_b4_hwrc, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "B4WarmED HWRC\nCTI") +
  # scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                    breaks=c("amb","1.7","3.4"),
                    labels = c("Ambient","Intermediate","Warmed"),
                    values = c("blue","orange","red")) +
  theme_bw() +
  theme(legend.position = "none")

CTI_ok_plot <- ggplot(CTI_ok, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_point(position = position_jitterdodge(),alpha = 0.2) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "Oklahoma\nCTI") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw()
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
  theme_bw() +
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
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))
CTI_box_ok <- ggplot(CTI_ok, aes(x = year, y = CTI, group = interaction(year, temp_treatment), color = temp_treatment, fill=temp_treatment)) +
  geom_boxplot(alpha=0.2, color="grey70") +
  geom_smooth(method="lm",aes(group=temp_treatment)) +
  labs(x = "Year", y = "Oklahoma\nCTI") +
  # scale_x_continuous(breaks = seq(2007, 2013)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")



### Ambient temp change figures
CTI_temp_tera_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth(data=tera, aes(x=year,y=mean_C_temp_summer/2.1),linetype="dotted",alpha=0.2,color="black") +
  geom_smooth() +
  labs(x = "Year", y = "CTI", title="TeRaCON") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")

CTI_temp_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth(data=jrgce, aes(x=year,y=mean_C_temp_summer*1.5),linetype="dotted",alpha=0.2,color="black") +
  geom_smooth() +
  labs(x = "Year", y = "CTI",title="JRGCE") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")



### Plotting arrow figs
arrow_teracon <- ggplot(CTI_CPI_teracon) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "TeRaCON") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

arrow_jrgce <- ggplot(CTI_CPI_jrgce) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "JRGCE") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

arrow_phace <- ggplot(CTI_CPI_phace) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "PHACE") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

arrow_b4_cfc <- ggplot(CTI_CPI_b4_cfc) +
  geom_segment(aes(x = CTI_amb, y = CPI_amb, 
                   xend = CTI_3.4, yend = CPI_3.4,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_amb, y = CPI_amb), color = "black") +
  geom_point(aes(x = CTI_3.4, y = CPI_3.4), color = "red") +
  labs(x = "CTI", y = "CPI", title = "B4Warmed CFC") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

arrow_b4_hwrc <- ggplot(CTI_CPI_b4_hwrc) +
  geom_segment(aes(x = CTI_amb, y = CPI_amb, 
                   xend = CTI_3.4, yend = CPI_3.4,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_amb, y = CPI_amb), color = "black") +
  geom_point(aes(x = CTI_3.4, y = CPI_3.4), color = "red") +
  labs(x = "CTI", y = "CPI", title = "B4Warmed HWRC") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

arrow_ok <- ggplot(CTI_CPI_ok) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "Oklahoma") +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()



##### Merging and saving figures #####
### Merge CTI figures
combined_CTI <- wrap_plots(CTI_jrgce_plot,CTI_phace_plot,CTI_tera_plot,
                       CTI_ok_plot,CTI_b4_cfc_plot,CTI_b4_hwrc_plot,
                       ncol = 3) + plot_layout(guides = "collect")
combined_CTI_sens <- wrap_plots(CTI_sens_jrgce_plot2,CTI_sens_phace_plot2,CTI_sens_tera_plot2,
                           CTI_sens_ok_plot2,CTI_sens_b4_cfc_plot,CTI_sens_b4_hwrc_plot,
                           ncol = 3) + plot_layout(guides = "collect",axis_titles = "collect")
# Save to computer
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


### CTI and contour plots
# Arrange contour plots into multi-panel figure
# Function to remove color, shape, and alpha guides
legend_rem <- function(plt){
  plt <- plt +
    guides(shape = "none", alpha = "none",size="none")
  return(plt)   
}
# Get shape legend
shp_lgnd <- contour_jrgce +
  guides(fill = "none") +
  facet_null() +
  theme(axis.text.y = element_blank(),
        plot.title = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.5, 0.5),
        #legend.title = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = "white", fill='white', size=1)) 
# Specify plot layout
design <- "
  aaaaaa#
  aaaaaa#
  bbbbbbd
  bbbbbbd
  cccccc#
  cccccc#
"
# Plot
p <-legend_rem(contour_jrgce) + 
  legend_rem(contour_phace) + 
  legend_rem(contour_tera) + 
  shp_lgnd +
  plot_layout(design = design, 
              axis_titles = "collect")

# Merge with CTI yearly plots
png("cti_contours.png", units="in", width=15, height=15, res=300)
wrap_plots(
  wrap_plots(CTI_facet_jrgce,CTI_facet_phace,
           CTI_facet_tera,
           nrow=3) +
           plot_layout(guides="collect",axis_titles = "collect"),
  wrap_plots(p)
  )
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

### Merge CTI + ambient temp figures
temp_CTI <- ggarrange2(CTI_temp_jrgce_plot,CTI_temp_tera_plot,
           nrow = 1, byrow = TRUE)

### Merge arrows into one fig
arrows <- ggarrange(arrow_jrgce,arrow_phace,arrow_teracon,arrow_ok,arrow_b4_cfc,arrow_b4_hwrc,
                    ncol = 3, nrow=2)


# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(combined_CTI, paste(path_out,'cti.rds'))
saveRDS(combined_CTI_sens, paste(path_out,'sens.rds'))
saveRDS(combined_sens, paste(path_out,'cti_sens.rds'))
saveRDS(biocon_CTI, paste(path_out,'biocon_cti.rds'))
saveRDS(temp_CTI, paste(path_out,'temperature_and_cti.rds'))
saveRDS(arrows, paste(path_out,'treatment_arrows.rds'))

saveRDS(CTI_sens_jrgce_plot, paste(path_out,'jrgce_cti_sens.rds'))





