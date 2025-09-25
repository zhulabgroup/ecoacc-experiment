# TITLE:          Figures of CTI/CPI/Biomass over time
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           April 2025

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


### Set path to Yu data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)
# Load in data
CPI_sens_knz <- read.csv(" CPI_sens_knz.csv")
CPI_knz <- read.csv(" CPI_knz.csv")
NPP_overall_knz <- read.csv(" knz_biomass.csv")
knz <- read.csv(" knz_clean.csv")

CPI_sens_hys <- read.csv(" CPI_sens_hys.csv")
CPI_hys <- read.csv(" CPI_hys.csv")
NPP_overall_hys <- read.csv(" hys_biomass.csv")
hys <- read.csv(" hys_clean.csv")

CPI_sens_sgs <- read.csv(" CPI_sens_sgs.csv")
CPI_sgs <- read.csv(" CPI_sgs.csv")
NPP_overall_sgs <- read.csv(" sgs_biomass.csv")
sgs <- read.csv(" sgs_clean.csv")

CPI_sens_chy <- read.csv(" CPI_sens_chy.csv")
CPI_chy <- read.csv(" CPI_chy.csv")
NPP_overall_chy <- read.csv(" chy_biomass.csv")
chy <- read.csv(" chy_clean.csv")



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
CPI_mean_knz <- CPI_knz %>%
  group_by(year,treatment) %>%
  summarize(mean_CPI = mean(CPI),
            se_CPI = sd(CPI)/sqrt(n()))
CPI_mean_hys <- CPI_hys %>%
  group_by(year,treatment) %>%
  summarize(mean_CPI = mean(CPI),
            se_CPI = sd(CPI)/sqrt(n()))
CPI_mean_sgs <- CPI_sgs %>%
  group_by(year,treatment) %>%
  summarize(mean_CPI = mean(CPI),
            se_CPI = sd(CPI)/sqrt(n()))
CPI_mean_chy <- CPI_chy %>%
  group_by(year,treatment) %>%
  summarize(mean_CPI = mean(CPI),
            se_CPI = sd(CPI)/sqrt(n()))



### CTI figures
# Teracon
CTI_mean_tera_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment)) +
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

# jrgce
CTI_mean_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment)) +
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

# phace
CTI_mean_phace_plot <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment)) +
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

# B4warmed cfc
CTI_mean_b4_cfc_plot <- ggplot(CTI_b4_cfc_rem2, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_cfc_rem2, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CTI_mean_b4_cfc_rem2, 
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

# B4warmed hwrc
CTI_mean_b4_hwrc_plot <- ggplot(CTI_b4_hwrc_rem2, aes(x = year, y = CTI, color = temp_treatment)) +
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

# Oklahoma
CTI_mean_ok_plot <- ggplot(CTI_ok, aes(x = year, y = CTI, color = temp_treatment)) +
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



### CPI figures
# KNZ
CPI_mean_knz_plot <- ggplot(CPI_knz, aes(x = year, y = CPI, color = treatment)) +
  geom_errorbar(data = CPI_mean_knz, 
                aes(x = year, y = mean_CPI, ymin = mean_CPI-se_CPI, ymax = mean_CPI+se_CPI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CPI_mean_knz, 
             aes(x = year, y = mean_CPI,fill=treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CPI",title = "(a) KNZ") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

# HYS
CPI_mean_hys_plot <- ggplot(CPI_hys, aes(x = year, y = CPI, color = treatment)) +
  geom_errorbar(data = CPI_mean_hys, 
                aes(x = year, y = mean_CPI, ymin = mean_CPI-se_CPI, ymax = mean_CPI+se_CPI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CPI_mean_hys, 
             aes(x = year, y = mean_CPI,fill=treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CPI",title = "(b) HYS") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

# SGS
CPI_mean_sgs_plot <- ggplot(CPI_sgs, aes(x = year, y = CPI, color = treatment)) +
  geom_errorbar(data = CPI_mean_sgs, 
                aes(x = year, y = mean_CPI, ymin = mean_CPI-se_CPI, ymax = mean_CPI+se_CPI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CPI_mean_sgs, 
             aes(x = year, y = mean_CPI,fill=treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CPI",title = "(c) SGS") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))

# CHY
CPI_mean_chy_plot <- ggplot(CPI_chy, aes(x = year, y = CPI, color = treatment)) +
  geom_errorbar(data = CPI_mean_chy, 
                aes(x = year, y = mean_CPI, ymin = mean_CPI-se_CPI, ymax = mean_CPI+se_CPI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.4) +
  geom_point(data = CPI_mean_chy, 
             aes(x = year, y = mean_CPI,fill=treatment), 
             shape = 21, size = 1.5, position = position_dodge(width = 0.9),alpha=0.4) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "CPI",title = "(d) CHY") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme_bw()+
  theme(axis.title = element_text(size=12,face="bold"),
        axis.text = element_text(size=11),
        plot.title = element_text(size=12))



### Disequilibrium figures
dis_phace_plot <- ggplot(CTI_phace, aes(x = year, y = disequilib, color = temp_treatment)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm') +
  labs(x = "Year", y = "CTI - MAT",title="PHACE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
dis_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = disequilib, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth() +
  labs(x = "Year", y = "CTI - MAT",title="JRGCE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
dis_ok_plot <- ggplot(CTI_ok, aes(x = year, y = disequilib, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth() +
  labs(x = "Year", y = "CTI - MAT",title="Oklahoma") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
dis_teracon_plot <- ggplot(CTI_teracon, aes(x = year, y = disequilib, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth() +
  labs(x = "Year", y = "CTI - MAT",title="TeRaCON") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")


### Biomass figures
NPP_teracon <- NPP_teracon %>%
  filter(variable == "mean_ab_bio")
npp_tera_plot <- ggplot(NPP_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = NULL) +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

npp_jrgce_plot <- ggplot(NPP_jrgce, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = NULL, title="Biomass\n(Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw()

npp_phace_plot <- ggplot(NPP_phace, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = NULL) +
  scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()

npp_b4_cfc_plot <- ggplot(NPP_b4_cfc, aes(x = year, y = sensitivity_high_temp)) +
  geom_smooth() +
  labs(x = "Year", y = NULL) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 2)) +
  theme_bw()

npp_b4_hwrc_plot <- ggplot(NPP_b4_hwrc, aes(x = year, y = sensitivity_high_temp)) +
  geom_smooth() +
  labs(x = "Year", y = NULL) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 2)) +
  theme_bw()

npp_ok_plot <- ggplot(NPP_ok, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = NULL) +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()



### Biomass vs MAT
# JRGCE
MAT_jrgce <- CTI_jrgce %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_jrgce <- left_join(NPP_overall_jrgce, MAT_jrgce,by=c("year","temp_treatment"))
jrgce_scatter_mat <- ggscatter(NPP_MAT_jrgce, x = "MAT", y = "mean_ab_bio", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "MAT", ylab = "Biomass",title="JRGCE")
jrgce_scatter_mat_col <- ggscatter(NPP_MAT_jrgce, x = "MAT", y = "mean_ab_bio", 
                                   color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                   cor.coef = FALSE, cor.method = "pearson",
                                   xlab = "MAT", ylab = "Biomass",title="JRGCE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# phace
MAT_phace <- CTI_phace %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_phace <- left_join(NPP_overall_phace, MAT_phace,by=c("year","temp_treatment"))
phace_scatter_mat <- ggscatter(NPP_MAT_phace, x = "MAT", y = "mean_ab_bio", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "MAT", ylab = "Biomass",title="phace")
phace_scatter_mat_col <- ggscatter(NPP_MAT_phace, x = "MAT", y = "mean_ab_bio", 
                                   color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                   cor.coef = FALSE, cor.method = "pearson",
                                   xlab = "MAT", ylab = "Biomass",title="phace") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# teracon
MAT_teracon <- CTI_teracon %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_teracon <- left_join(NPP_overall_teracon, MAT_teracon,by=c("year","temp_treatment"))
teracon_scatter_mat <- ggscatter(NPP_MAT_teracon, x = "MAT", y = "mean_ab_bio", 
                                 add = "reg.line", conf.int = TRUE, 
                                 cor.coef = TRUE, cor.method = "pearson",
                                 xlab = "MAT", ylab = "Biomass",title="teracon")
teracon_scatter_mat_col <- ggscatter(NPP_MAT_teracon, x = "MAT", y = "mean_ab_bio", 
                                     color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                     cor.coef = FALSE, cor.method = "pearson",
                                     xlab = "MAT", ylab = "Biomass",title="teracon") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# ok
MAT_ok <- CTI_ok %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_ok <- left_join(NPP_overall_ok, MAT_ok,by=c("year","temp_treatment"))
ok_scatter_mat <- ggscatter(NPP_MAT_ok, x = "MAT", y = "mean_ab_bio", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "MAT", ylab = "Biomass",title="ok")
ok_scatter_mat_col <- ggscatter(NPP_MAT_ok, x = "MAT", y = "mean_ab_bio", 
                                color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                cor.coef = FALSE, cor.method = "pearson",
                                xlab = "MAT", ylab = "Biomass",title="ok") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# cfc
MAT_cfc <- CTI_b4_cfc %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_cfc <- left_join(NPP_overall_b4_cfc, MAT_cfc,by=c("year","temp_treatment"))
cfc_scatter_mat <- ggscatter(NPP_MAT_cfc, x = "MAT", y = "mean_ab_bio", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "MAT", ylab = "Biomass",title="cfc")
cfc_scatter_mat_col <- ggscatter(NPP_MAT_cfc, x = "MAT", y = "mean_ab_bio", 
                                 color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = FALSE, cor.method = "pearson",
                                 xlab = "MAT", ylab = "Biomass",title="cfc") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")

# hwrc
MAT_hwrc <- CTI_b4_hwrc %>%
  dplyr::select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_hwrc <- left_join(NPP_overall_b4_hwrc, MAT_hwrc,by=c("year","temp_treatment"))
hwrc_scatter_mat <- ggscatter(NPP_MAT_hwrc, x = "MAT", y = "mean_ab_bio", 
                              add = "reg.line", conf.int = TRUE, 
                              cor.coef = TRUE, cor.method = "pearson",
                              xlab = "MAT", ylab = "Biomass",title="hwrc")
hwrc_scatter_mat_col <- ggscatter(NPP_MAT_hwrc, x = "MAT", y = "mean_ab_bio", 
                                  color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                  cor.coef = FALSE, cor.method = "pearson",
                                  xlab = "MAT", ylab = "Biomass",title="hwrc") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")

# knz
MAP_knz <- CPI_knz %>%
  dplyr::select(year,block,treatment,MAP) %>%
  distinct()
NPP_MAP_knz <- left_join(NPP_overall_knz, MAP_knz,by=c("year","block","treatment"))
knz_scatter_MAP <- ggscatter(NPP_MAP_knz, x = "MAP", y = "mean_ab_bio", 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "MAP", ylab = "Biomass",title="knz")
knz_scatter_MAP_col <- ggscatter(NPP_MAP_knz, x = "MAP", y = "biomass", 
                                   color="treatment",add = "reg.line", conf.int = TRUE, 
                                   cor.coef = FALSE, cor.method = "pearson",
                                   xlab = "AP", ylab = "Biomass",title="knz") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# chy
MAP_chy <- CPI_chy %>%
  dplyr::select(year,block,treatment,MAP) %>%
  distinct()
NPP_MAP_chy <- left_join(NPP_overall_chy, MAP_chy,by=c("year","block","treatment"))
chy_scatter_MAP <- ggscatter(NPP_MAP_chy, x = "MAP", y = "mean_ab_bio", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "MAP", ylab = "Biomass",title="chy")
chy_scatter_MAP_col <- ggscatter(NPP_MAP_chy, x = "MAP", y = "biomass", 
                                 color="treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = FALSE, cor.method = "pearson",
                                 xlab = "AP", ylab = "Biomass",title="chy") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# sgs
MAP_sgs <- CPI_sgs %>%
  dplyr::select(year,block,treatment,MAP) %>%
  distinct()
NPP_MAP_sgs <- left_join(NPP_overall_sgs, MAP_sgs,by=c("year","block","treatment"))
sgs_scatter_MAP <- ggscatter(NPP_MAP_sgs, x = "MAP", y = "mean_ab_bio", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "MAP", ylab = "Biomass",title="sgs")
sgs_scatter_MAP_col <- ggscatter(NPP_MAP_sgs, x = "MAP", y = "biomass", 
                                 color="treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = FALSE, cor.method = "pearson",
                                 xlab = "AP", ylab = "Biomass",title="sgs") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# hys
MAP_hys <- CPI_hys %>%
  dplyr::select(year,block,treatment,MAP) %>%
  distinct()
NPP_MAP_hys <- left_join(NPP_overall_hys, MAP_hys,by=c("year","block","treatment"))
hys_scatter_MAP <- ggscatter(NPP_MAP_hys, x = "MAP", y = "mean_ab_bio", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "MAP", ylab = "Biomass",title="hys")
hys_scatter_MAP_col <- ggscatter(NPP_MAP_hys, x = "MAP", y = "biomass", 
                                 color="treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = FALSE, cor.method = "pearson",
                                 xlab = "AP", ylab = "Biomass",title="hys") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")



### CTI vs biomass
# TeRaCON
CTI_yearly_avg_tera <- CTI_teracon %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon <- left_join(CTI_yearly_avg_tera, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter <- ggscatter(NPP_CTI_teracon, x = "mean_CTI", y = "mean_ab_bio", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "CTI", ylab = "Biomass", title="TeRaCON")
tera_scatter_col <- ggscatter(NPP_CTI_teracon, x = "mean_CTI", y = "mean_ab_bio", 
                              color = "temp_treatment",add = "reg.line", conf.int = TRUE, 
                              cor.coef = F, cor.method = "pearson",
                              xlab = "CTI", ylab = "Biomass", title="TeRaCON") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# JRGCE
CTI_yearly_avg_jrgce <- CTI_jrgce %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_jrgce <- left_join(CTI_yearly_avg_jrgce, NPP_overall_jrgce, by=c("year","temp_treatment"))
jrgce_scatter <- ggscatter(NPP_CTI_jrgce, x = "mean_CTI", y = "mean_ab_bio", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "CTI", ylab = "Biomass",title="JRGCE")
jrgce_scatter_col <- ggscatter(NPP_CTI_jrgce, x = "mean_CTI", y = "mean_ab_bio", 
                               color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                               cor.coef = F, cor.method = "pearson",
                               xlab = "CTI", ylab = "Biomass",title="JRGCE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# PHACE
CTI_yearly_avg_phace <- CTI_phace %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_phace <- left_join(CTI_yearly_avg_phace, NPP_overall_phace, by=c("year","temp_treatment"))
phace_scatter <- ggscatter(NPP_CTI_phace, x = "mean_CTI", y = "mean_ab_bio", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "CTI", ylab = "Biomass",title="PHACE")
phace_scatter_col <- ggscatter(NPP_CTI_phace, x = "mean_CTI", y = "mean_ab_bio", 
                               color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                               cor.coef = F, cor.method = "pearson",
                               xlab = "CTI", ylab = "Biomass",title="PHACE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# B4Warmed
CTI_yearly_avg_b4_cfc <- CTI_b4_cfc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
CTI_yearly_avg_b4_hwrc <- CTI_b4_hwrc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_b4_cfc <- left_join(CTI_yearly_avg_b4_cfc, NPP_overall_b4_cfc, by=c("year","temp_treatment"))
NPP_CTI_b4_hwrc <- left_join(CTI_yearly_avg_b4_hwrc, NPP_overall_b4_hwrc, by=c("year","temp_treatment"))
b4_scatter_cfc <- ggscatter(NPP_CTI_b4_cfc, x = "mean_CTI", y = "mean_ab_bio", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "CTI", ylab = "Biomass",title="B4Warmed CFC")
b4_scatter_hwrc <- ggscatter(NPP_CTI_b4_hwrc, x = "mean_CTI", y = "mean_ab_bio", 
                             add = "reg.line", conf.int = TRUE, 
                             cor.coef = TRUE, cor.method = "pearson",
                             xlab = "CTI", ylab = "Biomass",title="B4Warmed HWRC")

b4_scatter_cfc_col <- ggscatter(NPP_CTI_b4_cfc, x = "mean_CTI", y = "mean_ab_bio", 
                                color = "temp_treatment",add = "reg.line", conf.int = TRUE, 
                                cor.coef = F, cor.method = "pearson",
                                xlab = "CTI", ylab = "Biomass",title="B4Warmed CFC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")
b4_scatter_hwrc_col <- ggscatter(NPP_CTI_b4_hwrc, x = "mean_CTI", y = "mean_ab_bio", 
                                 color = "temp_treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = F, cor.method = "pearson",
                                 xlab = "CTI", ylab = "Biomass",title="B4Warmed HWRC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")

# OK
CTI_yearly_avg_ok <- CTI_ok %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_ok <- left_join(CTI_yearly_avg_ok, NPP_overall_ok, by=c("year","temp_treatment"))
ok_scatter <- ggscatter(NPP_CTI_ok, x = "mean_CTI", y = "mean_ab_bio", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "CTI", ylab = "Biomass",title="Oklahoma")
ok_scatter_col <- ggscatter(NPP_CTI_ok, x = "mean_CTI", y = "mean_ab_bio", 
                            color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                            cor.coef = F, cor.method = "pearson",
                            xlab = "CTI", ylab = "Biomass",title="Oklahoma") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# knz
CPI_yearly_avg_knz <- CPI_knz %>%
  group_by(year,block,treatment) %>%
  summarize(mean_CPI = mean(CPI))
NPP_CPI_knz <- left_join(CPI_yearly_avg_knz, NPP_overall_knz, by=c("year","block","treatment"))
knz_scatter <- ggscatter(NPP_CPI_knz, x = "mean_CPI", y = "mean_ab_bio", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "CPI", ylab = "Biomass",title="knzlahoma")
knz_scatter_col <- ggscatter(NPP_CPI_knz, x = "mean_CPI", y = "biomass", 
                            color="treatment",add = "reg.line", conf.int = TRUE, 
                            cor.coef = F, cor.method = "pearson",
                            xlab = "CPI", ylab = "Biomass",title="knz") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# chy
CPI_yearly_avg_chy <- CPI_chy %>%
  group_by(year,block,treatment) %>%
  summarize(mean_CPI = mean(CPI))
NPP_CPI_chy <- left_join(CPI_yearly_avg_chy, NPP_overall_chy, by=c("year","block","treatment"))
chy_scatter <- ggscatter(NPP_CPI_chy, x = "mean_CPI", y = "mean_ab_bio", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "CPI", ylab = "Biomass",title="chylahoma")
chy_scatter_col <- ggscatter(NPP_CPI_chy, x = "mean_CPI", y = "biomass", 
                             color="treatment",add = "reg.line", conf.int = TRUE, 
                             cor.coef = F, cor.method = "pearson",
                             xlab = "CPI", ylab = "Biomass",title="chy") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# sgs
CPI_yearly_avg_sgs <- CPI_sgs %>%
  group_by(year,block,treatment) %>%
  summarize(mean_CPI = mean(CPI))
NPP_CPI_sgs <- left_join(CPI_yearly_avg_sgs, NPP_overall_sgs, by=c("year","block","treatment"))
sgs_scatter <- ggscatter(NPP_CPI_sgs, x = "mean_CPI", y = "mean_ab_bio", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "CPI", ylab = "Biomass",title="sgslahoma")
sgs_scatter_col <- ggscatter(NPP_CPI_sgs, x = "mean_CPI", y = "biomass", 
                             color="treatment",add = "reg.line", conf.int = TRUE, 
                             cor.coef = F, cor.method = "pearson",
                             xlab = "CPI", ylab = "Biomass",title="sgs") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# hys
CPI_yearly_avg_hys <- CPI_hys %>%
  group_by(year,block,treatment) %>%
  summarize(mean_CPI = mean(CPI))
NPP_CPI_hys <- left_join(CPI_yearly_avg_hys, NPP_overall_hys, by=c("year","block","treatment"))
hys_scatter <- ggscatter(NPP_CPI_hys, x = "mean_CPI", y = "mean_ab_bio", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "CPI", ylab = "Biomass",title="hyslahoma")
hys_scatter_col <- ggscatter(NPP_CPI_hys, x = "mean_CPI", y = "biomass", 
                             color="treatment",add = "reg.line", conf.int = TRUE, 
                             cor.coef = F, cor.method = "pearson",
                             xlab = "CPI", ylab = "Biomass",title="hys") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")



### Biomass vs disequilibrium
# PHACE
CTI_yearly_avg_dis_phace <- CTI_phace %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_phace <- left_join(CTI_yearly_avg_dis_phace, NPP_overall_phace, by=c("year","temp_treatment"))
phace_dis <- ggscatter(NPP_CTI_dis_phace, x = "mean_dis", y = "mean_ab_bio", 
                       add = "reg.line", conf.int = TRUE, 
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "CTI - MAT", ylab = "Biomass",title="PHACE")
phace_dis_col <- ggscatter(NPP_CTI_dis_phace, x = "mean_dis", y = "mean_ab_bio", 
                           color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                           cor.coef = FALSE, cor.method = "pearson",
                           xlab = "CTI - MAT", ylab = "Biomass",title="PHACE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# JRGCE
CTI_yearly_avg_dis_jrgce <- CTI_jrgce %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_jrgce <- left_join(CTI_yearly_avg_dis_jrgce, NPP_overall_jrgce, by=c("year","temp_treatment"))
jrgce_dis <- ggscatter(NPP_CTI_dis_jrgce, x = "mean_dis", y = "mean_ab_bio", 
                       add = "reg.line", conf.int = TRUE, 
                       cor.coef = TRUE, cor.method = "pearson",
                       xlab = "CTI - MAT", ylab = "Biomass",title="JRGCE")
jrgce_dis_col <- ggscatter(NPP_CTI_dis_jrgce, x = "mean_dis", y = "mean_ab_bio", 
                           color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                           cor.coef = FALSE, cor.method = "pearson",
                           xlab = "CTI - MAT", ylab = "Biomass",title="JRGCE") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# CFC
CTI_yearly_avg_dis_cfc <- CTI_b4_cfc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_cfc <- left_join(CTI_yearly_avg_dis_cfc, NPP_overall_b4_cfc, by=c("year","temp_treatment"))
cfc_dis <- ggscatter(NPP_CTI_dis_cfc, x = "mean_dis", y = "mean_ab_bio", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "CTI - MAT", ylab = "Biomass",title="cfc")
cfc_dis_col <- ggscatter(NPP_CTI_dis_cfc, x = "mean_dis", y = "mean_ab_bio", 
                         color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                         cor.coef = FALSE, cor.method = "pearson",
                         xlab = "CTI - MAT", ylab = "Biomass",title="CFC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")

# hwrc
CTI_yearly_avg_dis_hwrc <- CTI_b4_hwrc %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_hwrc <- left_join(CTI_yearly_avg_dis_hwrc, NPP_overall_b4_hwrc, by=c("year","temp_treatment"))
hwrc_dis <- ggscatter(NPP_CTI_dis_hwrc, x = "mean_dis", y = "mean_ab_bio", 
                      add = "reg.line", conf.int = TRUE, 
                      cor.coef = TRUE, cor.method = "pearson",
                      xlab = "CTI - MAT", ylab = "Biomass",title="hwrc")
hwrc_dis_col <- ggscatter(NPP_CTI_dis_hwrc, x = "mean_dis", y = "mean_ab_bio", 
                          color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                          cor.coef = FALSE, cor.method = "pearson",
                          xlab = "CTI - MAT", ylab = "Biomass",title="HWRC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue")) +
  theme(legend.position = "none")

# OK
# terraclim MAT
CTI_yearly_avg_dis_ok <- CTI_ok %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_ok <- left_join(CTI_yearly_avg_dis_ok, NPP_overall_ok, by=c("year","temp_treatment"))
ok_dis <- ggscatter(NPP_CTI_dis_ok, x = "mean_dis", y = "mean_ab_bio", 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "pearson",
                    xlab = "CTI - MAT", ylab = "Biomass",title="ok")
ok_dis_col <- ggscatter(NPP_CTI_dis_ok, x = "mean_dis", y = "mean_ab_bio", 
                        color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                        cor.coef = FALSE, cor.method = "pearson",
                        xlab = "CTI - MAT", ylab = "Biomass",title="Oklahoma") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")
# sensor MAT
CTI_yearly_avg_dis_sensors_ok <- CTI_ok %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib_sensors))
NPP_CTI_dis_sensors_ok <- left_join(CTI_yearly_avg_dis_sensors_ok, NPP_overall_ok, by=c("year","temp_treatment"))
ok_dis_sensors <- ggscatter(NPP_CTI_dis_sensors_ok, x = "mean_dis", y = "mean_ab_bio", 
                            add = "reg.line", conf.int = TRUE, 
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "CTI - MAT", ylab = "Biomass",title="ok")
ok_dis_sensors_col <- ggscatter(NPP_CTI_dis_sensors_ok, x = "mean_dis", y = "mean_ab_bio", 
                                color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                                cor.coef = FALSE, cor.method = "pearson",
                                xlab = "CTI - MAT", ylab = "Biomass",title="Oklahoma: Sensor data") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")

# tera
CTI_yearly_avg_dis_teracon <- CTI_teracon %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CTI_dis_teracon <- left_join(CTI_yearly_avg_dis_teracon, NPP_overall_teracon, by=c("year","temp_treatment"))
teracon_dis <- ggscatter(NPP_CTI_dis_teracon, x = "mean_dis", y = "mean_ab_bio", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "CTI - MAT", ylab = "Biomass",title="teracon")
teracon_dis_col <- ggscatter(NPP_CTI_dis_teracon, x = "mean_dis", y = "mean_ab_bio", 
                             color="temp_treatment",add = "reg.line", conf.int = TRUE, 
                             cor.coef = FALSE, cor.method = "pearson",
                             xlab = "CTI - MAT", ylab = "Biomass",title="TeRaCON") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme(legend.position = "none")



# KNZ
CPI_yearly_avg_dis_knz <- CPI_knz %>%
  group_by(year,block,treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CPI_dis_knz <- left_join(CPI_yearly_avg_dis_knz, NPP_overall_knz, by=c("year","block","treatment"))
knz_dis <- ggscatter(NPP_CPI_dis_knz, x = "mean_dis", y = "biomass", 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         xlab = "CPI - MAT", ylab = "Biomass",title="knz")
knz_dis_col <- ggscatter(NPP_CPI_dis_knz, x = "mean_dis", y = "biomass", 
                             color="treatment",add = "reg.line", conf.int = TRUE, 
                             cor.coef = FALSE, cor.method = "pearson",
                             xlab = "CPI - AP",
                         ylab = "Biomass",title="KNZ") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# chy
CPI_yearly_avg_dis_chy <- CPI_chy %>%
  group_by(year,block,treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CPI_dis_chy <- left_join(CPI_yearly_avg_dis_chy, NPP_overall_chy, by=c("year","block","treatment"))
chy_dis <- ggscatter(NPP_CPI_dis_chy, x = "mean_dis", y = "biomass", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "CPI - MAT", ylab = "Biomass",title="chy")
chy_dis_col <- ggscatter(NPP_CPI_dis_chy, x = "mean_dis", y = "biomass", 
                         color="treatment",add = "reg.line", conf.int = TRUE, 
                         cor.coef = FALSE, cor.method = "pearson",
                         xlab = "CPI - AP",
                         ylab = "Biomass",title="CHY") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# sgs
CPI_yearly_avg_dis_sgs <- CPI_sgs %>%
  group_by(year,block,treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CPI_dis_sgs <- left_join(CPI_yearly_avg_dis_sgs, NPP_overall_sgs, by=c("year","block","treatment"))
sgs_dis <- ggscatter(NPP_CPI_dis_sgs, x = "mean_dis", y = "biomass", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "CPI - MAT", ylab = "Biomass",title="sgs")
sgs_dis_col <- ggscatter(NPP_CPI_dis_sgs, x = "mean_dis", y = "biomass", 
                         color="treatment",add = "reg.line", conf.int = TRUE, 
                         cor.coef = FALSE, cor.method = "pearson",
                         xlab = "CPI - AP",
                         ylab = "Biomass",title="SGS") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")

# hys
CPI_yearly_avg_dis_hys <- CPI_hys %>%
  group_by(year,block,treatment) %>%
  summarize(mean_dis = mean(disequilib))
NPP_CPI_dis_hys <- left_join(CPI_yearly_avg_dis_hys, NPP_overall_hys, by=c("year","block","treatment"))
hys_dis <- ggscatter(NPP_CPI_dis_hys, x = "mean_dis", y = "biomass", 
                     add = "reg.line", conf.int = TRUE, 
                     cor.coef = TRUE, cor.method = "pearson",
                     xlab = "CPI - MAT", ylab = "Biomass",title="hys")
hys_dis_col <- ggscatter(NPP_CPI_dis_hys, x = "mean_dis", y = "biomass", 
                         color="treatment",add = "reg.line", conf.int = TRUE, 
                         cor.coef = FALSE, cor.method = "pearson",
                         xlab = "CPI - AP",
                         ylab = "Biomass",title="HYS") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Drought"),
                     values = c("blue","orange")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Drought"),
                    values = c("blue","orange")) +
  theme(legend.position = "none")



### Export figures
### CTI and CPI over time
cti_fig <- wrap_plots(CTI_mean_jrgce_plot, CTI_mean_phace_plot, CTI_mean_tera_plot,
                      CTI_mean_ok_plot, CTI_mean_b4_cfc_plot, CTI_mean_b4_hwrc_plot,
                      ncol=3) +
  plot_layout(guides = "collect",axis_titles = "collect")
cpi_fig <- wrap_plots(CPI_mean_knz_plot,CPI_mean_hys_plot,CPI_mean_sgs_plot,CPI_mean_chy_plot,
                      ncol=2) +
  plot_layout(guides = "collect",axis_titles = "collect")
png("cpi.png", units="in", width=9, height=6, res=300)
wrap_plots(CPI_mean_knz_plot,CPI_mean_hys_plot,CPI_mean_sgs_plot,CPI_mean_chy_plot,
           ncol=2) +
  plot_layout(guides = "collect",axis_titles = "collect")
dev.off()

### Merge CTI vs. Biomass sensitivity fig
biomass_sens_CTI <- ggarrange2(CTI_sens_jrgce_plot,npp_jrgce_plot,
                               CTI_sens_phace_plot,npp_phace_plot,
                               CTI_sens_tera_plot,npp_tera_plot,
                               CTI_sens_ok_plot,npp_ok_plot,
                               CTI_sens_b4_cfc_plot,npp_b4_cfc_plot,
                               CTI_sens_b4_hwrc_plot,npp_b4_hwrc_plot,
                               nrow = 6, byrow = TRUE)

### Merge scatterplots for cti vs. biomass
biomass_CTI <- wrap_plots(jrgce_scatter,phace_scatter,tera_scatter,
                          ok_scatter,b4_scatter_cfc,b4_scatter_hwrc,
                          ncol=3,nrow=2) + plot_layout(axis_titles = "collect")

### Merge scatterplots for mat vs. biomass
mat_biomass <- wrap_plots(jrgce_scatter_mat,phace_scatter_mat,teracon_scatter_mat,
                          ok_scatter_mat,cfc_scatter_mat,hwrc_scatter_mat,
                          ncol=3,nrow=2) + plot_layout(axis_titles = "collect")

### Merge scatterplots for cti vs. biomass across temp treatments
biomass_treat_CTI <- wrap_plots(jrgce_scatter_col,phace_scatter_col,tera_scatter_col,
                                ok_scatter_col,b4_scatter_cfc_col,b4_scatter_hwrc_col,
                                ncol = 3) + plot_layout(axis_titles = "collect")
png("warm_cti.png", units="in", width=10, height=6, res=300)
wrap_plots(jrgce_scatter_col,phace_scatter_col,tera_scatter_col,
           ok_scatter_col,b4_scatter_cfc_col,b4_scatter_hwrc_col,
           ncol = 3) + plot_layout(axis_titles = "collect")
dev.off()

png("drought_cpi.png", units="in", width=10, height=6, res=300)
wrap_plots(knz_scatter_col,hys_scatter_col,
           sgs_scatter_col,chy_scatter_col,
           ncol = 2) + plot_layout(axis_titles = "collect")
dev.off()

### Merge scatterplots for mat vs. biomass across temp treatments
mat_treat_biomass <- wrap_plots(jrgce_scatter_mat_col,phace_scatter_mat_col,teracon_scatter_mat_col,
                                ok_scatter_mat_col,cfc_scatter_mat_col,hwrc_scatter_mat_col,
                                ncol=3,nrow=2) + plot_layout(axis_titles = "collect")
png("warm_mat.png", units="in", width=10, height=6, res=300)
wrap_plots(jrgce_scatter_mat_col,phace_scatter_mat_col,teracon_scatter_mat_col,
           ok_scatter_mat_col,cfc_scatter_mat_col,hwrc_scatter_mat_col,
           ncol=3,nrow=2) + plot_layout(axis_titles = "collect")
dev.off()

png("drought_map.png", units="in", width=10, height=6, res=300)
wrap_plots(knz_scatter_MAP_col,hys_scatter_MAP_col,
           sgs_scatter_MAP_col,chy_scatter_MAP_col,
           ncol=2,nrow=2) + plot_layout(axis_titles = "collect")
dev.off()

### Merge biomass disequilibrium figs
dis_scatter <- wrap_plots(jrgce_dis,phace_dis,teracon_dis,
                          ok_dis,cfc_dis,hwrc_dis,
                          ncol=3,nrow=2) + plot_layout(axis_titles = "collect")

### Merge biomass disequilibrium figs across temp treatments
dis_treat_scatter_w <- wrap_plots(jrgce_dis_col,phace_dis_col,teracon_dis_col,
                                ok_dis_col,cfc_dis_col,hwrc_dis_col,
                                ncol=3,nrow=2) + plot_layout(axis_titles = "collect")
png("warm_dis.png", units="in", width=10, height=6, res=300)
wrap_plots(jrgce_dis_col,phace_dis_col,teracon_dis_col,
           ok_dis_col,cfc_dis_col,hwrc_dis_col,
           ncol=3,nrow=2) + plot_layout(axis_titles = "collect")
dev.off()

dis_treat_scatter_d <- wrap_plots(knz_dis_col,hys_dis_col,
                                sgs_dis_col,chy_dis_col,
                                ncol=2,nrow=2) + plot_layout(axis_titles = "collect")
png("drought_dis.png", units="in", width=10, height=6, res=300)
wrap_plots(knz_dis_col,hys_dis_col,
           sgs_dis_col,chy_dis_col,
           ncol=2,nrow=2) + plot_layout(axis_titles = "collect")
dev.off()



# Export Rdata for plot
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/data_for_plots/"
saveRDS(biomass_sens_CTI, paste(path_out,'biomass_and_cti.rds'))
saveRDS(biomass_CTI, paste(path_out,'biomass_vs_cti.rds'))
saveRDS(biomass_treat_CTI, paste(path_out,'biomass_vs_cti_treatments.rds'))
saveRDS(dis_scatter, paste(path_out,'biomass_vs_disequilib.rds'))
saveRDS(dis_treat_scatter, paste(path_out,'biomass_vs_disequilib_treatments.rds'))
saveRDS(mat_biomass, paste(path_out,'biomass_vs_mat.rds'))
saveRDS(mat_treat_biomass, paste(path_out,'biomass_vs_mat_treatments.rds'))

saveRDS(ok_dis_col, paste(path_out,'ok_disequilib_terraclim.rds'))
saveRDS(ok_dis_sensors_col, paste(path_out,'ok_disequilib_sensors.rds'))

saveRDS(cti_fig, paste(path_out,'cti_yearly.rds'))
saveRDS(cpi_fig, paste(path_out,'cpi_yearly.rds'))


