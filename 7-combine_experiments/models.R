# TITLE:          Stats for CTI changes over time btwn treatments
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Feb 2025


### Load packages
library(tidyverse)
library(lmerTest)



### Set path to turbo to get teracon data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")

### Set path to jrgce data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
CTI_jrgce <- read.csv(" CTI_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")

### Set path to phace data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace.csv")
CTI_phace <- read.csv(" CTI_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace.csv")

### Set path to b4warmed data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
CTI_sens_b4_cfc <- read.csv(" CTI_sens_b4warmed_cfc.csv")
CTI_b4_cfc <- read.csv(" CTI_b4warmed_cfc.csv")
CTI_CPI_b4_cfc <- read.csv(" CTI_CPI_b4warmed_cfc.csv")

CTI_sens_b4_hwrc <- read.csv(" CTI_sens_b4warmed_hwrc.csv")
CTI_b4_hwrc <- read.csv(" CTI_b4warmed_hwrc.csv")
CTI_CPI_b4_hwrc <- read.csv(" CTI_CPI_b4warmed_hwrc.csv")

### Set path to oklahoma data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
CTI_sens_ok <- read.csv(" CTI_sens_ok.csv")
CTI_ok <- read.csv(" CTI_ok.csv")
CTI_CPI_ok <- read.csv(" CTI_CPI_ok.csv")



### Models to test for CTI changes over time and due to warming treatment
# Teracon
mod.tera <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_teracon)
anova(mod.tera)
summary(mod.tera)

# JRGCE
mod.jrgce <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_jrgce)
anova(mod.jrgce)
summary(mod.jrgce)

# PHACE
mod.phace <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_phace)
anova(mod.phace)
summary(mod.phace)

# B4Warmed
mod.b4_cfc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_cfc)
anova(mod.b4_cfc)
summary(mod.b4_cfc)

mod.b4_hwrc <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_b4_hwrc)
anova(mod.b4_hwrc)
summary(mod.b4_hwrc)

# OK
mod.ok <- lmer(CTI ~ year * temp_treatment + (1|plot), data = CTI_ok)
anova(mod.ok)
summary(mod.ok)



### Models testing for CTI sensitivity over time
# Teracon
mod.tera_sens <- lm(sensitivity ~ year, data = CTI_sens_teracon)
anova(mod.tera_sens)
summary(mod.tera_sens)

# JRGCE
mod.jrgce_sens <- lm(sensitivity ~ year, data = CTI_sens_jrgce)
anova(mod.jrgce_sens)
summary(mod.jrgce_sens)

# PHACE
mod.phace_sens <- lm(sensitivity ~ year, data = CTI_sens_phace)
anova(mod.phace_sens)
summary(mod.phace_sens)

# B4Warmed
mod.b4_cfc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_cfc)
anova(mod.b4_cfc_sens_high)
summary(mod.b4_cfc_sens_high)

mod.b4_cfc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_cfc)
anova(mod.b4_cfc_sens_med)
summary(mod.b4_cfc_sens_med)

mod.b4_hwrc_sens_high <- lm(sensitivity_high_temp ~ year, data = CTI_sens_b4_hwrc)
anova(mod.b4_hwrc_sens_high)
summary(mod.b4_hwrc_sens_high)

mod.b4_hwrc_sens_med <- lm(sensitivity_med_temp ~ year, data = CTI_sens_b4_hwrc)
anova(mod.b4_hwrc_sens_med)
summary(mod.b4_hwrc_sens_med)

# OK
mod.ok_sens <- lm(sensitivity ~ year, data = CTI_sens_ok)
anova(mod.ok_sens)
summary(mod.ok_sens)


