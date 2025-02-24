# TITLE:          Figures of CTI/CPI/Biomass over time
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
niche_est_tera <- niche_est_tera %>%
  dplyr::select(-c(latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()
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
CTI_sens_tera_plot <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()
CTI_sens_tera_plot2 <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="TeRaCON") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

CTI_sens_jrgce_plot <- ggplot(CTI_sens_jrgce, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  #scale_x_continuous(breaks = seq(1998, 2014, by = 3)) +
  theme_bw()
CTI_sens_jrgce_plot2 <- ggplot(CTI_sens_jrgce, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="JRGCE") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

CTI_sens_phace_plot <- ggplot(CTI_sens_phace, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()
CTI_sens_phace_plot2 <- ggplot(CTI_sens_phace, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="PHACE") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

CTI_sens_b4_cfc_rem <- CTI_sens_b4_cfc %>%
  filter(year != 2021)
CTI_sens_b4_cfc_plot <- ggplot(CTI_sens_b4_cfc_rem, aes(x = year, y = sensitivity_high_temp)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  #scale_x_continuous(breaks = seq(2008, 2021, by = 2)) +
  theme_bw()
CTI_sens_b4_cfc_plot2 <- ggplot(CTI_sens_b4_cfc_rem, aes(x = year, y = sensitivity_high_temp)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="B4WarmED CFC") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 3)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

CTI_sens_b4_hwrc_rem <- CTI_sens_b4_hwrc %>%
  filter(year != 2021)
CTI_sens_b4_hwrc_plot <- ggplot(CTI_sens_b4_hwrc_rem, aes(x = year, y = sensitivity_high_temp)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 3)) +
  theme_bw()
CTI_sens_b4_hwrc_plot2 <- ggplot(CTI_sens_b4_hwrc_rem, aes(x = year, y = sensitivity_high_temp)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="B4WarmED HWRC") +
  scale_x_continuous(breaks = seq(2008, 2020, by = 3)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))

CTI_sens_ok_plot <- ggplot(CTI_sens_ok, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI\n(Warmed - Ambient)") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw()
CTI_sens_ok_plot2 <- ggplot(CTI_sens_ok, aes(x = year, y = sensitivity)) +
  geom_jitter(alpha=0.2) +
  geom_smooth(method='lm',color="black") +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title="Oklahoma") +
  #scale_x_continuous(breaks = seq(2007, 2013)) +
  theme_bw() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=12))



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
CTI_mean_tera_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_tera, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_tera, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
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
CTI_mean_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_jrgce, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_jrgce, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "JRGCE\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
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
CTI_mean_phace_plot <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_phace, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_phace, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "PHACE\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
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

CTI_b4_cfc_rem <- CTI_b4_cfc %>%
  filter(year != 2021) %>%
  filter(temp_treatment != "1.7")
CTI_b4_cfc_rem2 <- CTI_b4_cfc %>%
  filter(year != 2021)
CTI_mean_b4_cfc_rem2 <- CTI_mean_b4_cfc %>%
  filter(year != 2021)
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
CTI_mean_b4_cfc_plot <- ggplot(CTI_b4_cfc_rem2, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_cfc_rem2, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_b4_cfc_rem2, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "B4WarmED CFC\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                    breaks=c("amb","1.7","3.4"),
                    labels = c("Ambient","Intermediate","Warmed"),
                    values = c("blue","orange","red")) +
  theme_bw()
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


CTI_b4_hwrc_rem <- CTI_b4_hwrc %>%
  filter(year != 2021) %>%
  filter(temp_treatment != "1.7")
CTI_b4_hwrc_rem2 <- CTI_b4_hwrc %>%
  filter(year != 2021)
CTI_mean_b4_hwrc_rem2 <- CTI_mean_b4_hwrc %>%
  filter(year != 2021)
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
CTI_mean_b4_hwrc_plot <- ggplot(CTI_b4_hwrc_rem2, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_b4_hwrc_rem2, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_b4_hwrc_rem2, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "B4WarmED HWRC\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     breaks=c("amb","1.7","3.4"),
                     labels = c("Ambient","Intermediate","Warmed"),
                     values = c("blue","orange","red")) +
  scale_fill_manual(name = "Treatment",
                    breaks=c("amb","1.7","3.4"),
                    labels = c("Ambient","Intermediate","Warmed"),
                    values = c("blue","orange","red")) +
  theme_bw()
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
CTI_mean_ok_plot <- ggplot(CTI_ok, aes(x = year, y = CTI, color = temp_treatment)) +
  geom_errorbar(data = CTI_mean_ok, 
                aes(x = year, y = mean_CTI, ymin = mean_CTI-se_CTI, ymax = mean_CTI+se_CTI), 
                width = 0.2, position = position_dodge(width = 0.9), alpha=0.5) +
  geom_point(data = CTI_mean_ok, 
             aes(x = year, y = mean_CTI,fill=temp_treatment), 
             shape = 21, size = 2, position = position_dodge(width = 0.9),alpha=0.5) +
  geom_smooth(method="lm") +
  labs(x = "Year", y = "Oklahoma\nCTI") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
  theme_bw() +
  theme(legend.position = "none")
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



### Disequilibrium figures
dis_phace_plot <- ggplot(CTI_phace, aes(x = year, y = disequilib, color = temp_treatment)) +
  #geom_jitter(alpha=0.2) +
  geom_smooth() +
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



### Biomass vs MAT
# JRGCE
MAT_jrgce <- CTI_jrgce %>%
  select(year,temp_treatment,MAT) %>%
  distinct()
NPP_MAT_jrgce <- left_join(NPP_overall_jrgce, MAT_jrgce,by=c("year","temp_treatment"))
jrgce_scatter <- ggscatter(NPP_MAT_jrgce, x = "MAT", y = "mean_ab_bio", 
                           add = "reg.line", conf.int = TRUE, 
                           cor.coef = TRUE, cor.method = "pearson",
                           xlab = "MAT", ylab = "Biomass",title="JRGCE")
jrgce_scatter_col <- ggscatter(NPP_MAT_jrgce, x = "MAT", y = "mean_ab_bio", 
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
                              cor.coef = TRUE, cor.method = "pearson",
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
                               cor.coef = TRUE, cor.method = "pearson",
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
                               cor.coef = TRUE, cor.method = "pearson",
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
                                cor.coef = TRUE, cor.method = "pearson",
                                xlab = "CTI", ylab = "Biomass",title="B4Warmed CFC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue"))
b4_scatter_hwrc_col <- ggscatter(NPP_CTI_b4_hwrc, x = "mean_CTI", y = "mean_ab_bio", 
                                 color = "temp_treatment",add = "reg.line", conf.int = TRUE, 
                                 cor.coef = TRUE, cor.method = "pearson",
                                 xlab = "CTI", ylab = "Biomass",title="B4Warmed HWRC") +
  scale_color_manual(name = "Treatment",
                     labels = c("Intermediate","Warmed","Ambient"),
                     values = c("orange","red","blue")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Intermediate","Warmed","Ambient"),
                    values = c("orange","red","blue"))

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
                            cor.coef = TRUE, cor.method = "pearson",
                            xlab = "CTI", ylab = "Biomass",title="Oklahoma") +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red")) +
  scale_fill_manual(name = "Treatment",
                    labels = c("Ambient","Warmed"),
                    values = c("blue","red")) +
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
                           CTI_sens_ok_plot2,CTI_sens_b4_cfc_plot2,CTI_sens_b4_hwrc_plot2,
                           ncol = 3) + plot_layout(guides = "collect",axis_titles = "collect")
# Save to computer
png("cti_sens.png", units="in", width=10, height=6, res=300)
wrap_plots(CTI_sens_jrgce_plot2,CTI_sens_phace_plot2,CTI_sens_tera_plot2,
           CTI_sens_ok_plot2,CTI_sens_b4_cfc_plot2,CTI_sens_b4_hwrc_plot2,
           ncol = 3) + plot_layout(guides = "collect",axis_titles = "collect")
dev.off()

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

png("cti_sens.png", units="in", width=9, height=10, res=300)
wrap_plots(CTI_mean_jrgce_plot,CTI_sens_jrgce_plot,
           CTI_mean_phace_plot,CTI_sens_phace_plot,
           CTI_mean_tera_plot,CTI_sens_tera_plot,
           CTI_mean_ok_plot,CTI_sens_ok_plot,
           CTI_mean_b4_cfc_plot,CTI_sens_b4_cfc_plot,
           CTI_mean_b4_hwrc_plot,CTI_sens_b4_hwrc_plot,
           ncol=2,nrow=6) +
  plot_layout(guides = "collect")+
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

### Merge CTI + ambient temp figures
temp_CTI <- ggarrange2(CTI_temp_jrgce_plot,CTI_temp_tera_plot,
           nrow = 1, byrow = TRUE)

### Merge CTI vs. Biomass sensitivity fig
biomass_sens_CTI <- ggarrange2(CTI_sens_jrgce_plot,npp_jrgce_plot,
           CTI_sens_phace_plot,npp_phace_plot,
           CTI_sens_tera_plot,npp_tera_plot,
           CTI_sens_ok_plot,npp_ok_plot,
           CTI_sens_b4_cfc_plot,npp_b4_cfc_plot,
           CTI_sens_b4_hwrc_plot,npp_b4_hwrc_plot,
           nrow = 6, byrow = TRUE)

### Merge scatterplots for cti vs. biomass
biomass_CTI <- ggarrange(jrgce_scatter,phace_scatter,tera_scatter,ok_scatter,b4_scatter_cfc,b4_scatter_hwrc,
          ncol=3,nrow=2)

### Merge biomass disequilibrium figs
dis_scatter <- ggarrange(jrgce_dis_col,phace_dis_col,teracon_dis_col,
                         ok_dis_col,cfc_dis_col,hwrc_dis_col,
                         ncol=3,nrow=2)

### Merge scatterplots for cti vs. biomas across temp treatments
biomass_treat_CTI <- wrap_plots(jrgce_scatter_col,phace_scatter_col,tera_scatter_col,
                        ok_scatter_col,b4_scatter_cfc_col,b4_scatter_hwrc_col,
                        ncol = 3) & theme(legend.position = "bottom") + plot_layout(guides = "collect")

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
saveRDS(biomass_sens_CTI, paste(path_out,'biomass_and_cti.rds'))
saveRDS(biomass_CTI, paste(path_out,'biomass_vs_cti.rds'))
saveRDS(biomass_treat_CTI, paste(path_out,'biomass_vs_cti_treatments.rds'))
saveRDS(arrows, paste(path_out,'treatment_arrows.rds'))
saveRDS(dis_scatter, paste(path_out,'biomass_vs_disequilib.rds'))
