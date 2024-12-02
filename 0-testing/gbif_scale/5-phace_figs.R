# TITLE:          Testing various data calculations/variable selection
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Data from experiments 
# DATA OUTPUT:    Comparisons of various variable manipulations
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(ggpubr)

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/data_for_testing/"
setwd(path_data)
# Load in data
CTI_phace_uscan <- read.csv(" CTI_phace_uscan.csv")
CTI_phace_1000 <- read.csv(" CTI_phace_1000.csv")
CTI_phace_500 <- read.csv(" CTI_phace_500.csv")

CTI_sens_phace_uscan <- read.csv(" CTI_sens_phace_uscan.csv")
CTI_sens_phace_1000 <- read.csv(" CTI_sens_phace_1000.csv")
CTI_sens_phace_500 <- read.csv(" CTI_sens_phace_500.csv")

CTI_CPI_phace_uscan <- read.csv(" CTI_CPI_phace_uscan.csv")
CTI_CPI_phace_1000 <- read.csv(" CTI_CPI_phace_1000.csv")
CTI_CPI_phace_500 <- read.csv(" CTI_CPI_phace_500.csv")

# Set path to turbo to get data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
CTI_CPI_phace_eco9 <- read.csv(" CTI_CPI_phace_limited.csv")
CTI_phace_eco9 <- read.csv(" CTI_phace_limited.csv")
CTI_sens_phace_eco9 <- read.csv(" CTI_sens_phace_limited.csv")


##### Fig: phace --> comparing different scales of GBIF data on CTI change over time
CTI_uscan_phace <- ggplot(CTI_phace_uscan, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="US/Canada GBIF occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

CTI_eco9_phace <- ggplot(CTI_phace_eco9, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="US/Canada GBIF occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

CTI_1000_phace <- ggplot(CTI_phace_1000, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="1000km buffer occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

CTI_500_phace <- ggplot(CTI_phace_500, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="500km buffer occurrences") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))

# Combine figures into one multi-panel plot
ggarrange(CTI_uscan_phace,CTI_eco9_phace,CTI_1000_phace,CTI_500_phace,
          ncol = 4)




#### Fig: phace --> arrow comparisons for different gbif scales
arrow_uscan <- ggplot(CTI_CPI_phace_uscan) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "US/Canada GBIF occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

arrow_eco9 <- ggplot(CTI_CPI_phace_eco9) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "Ecoregion 9 GBIF occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

arrow_1000 <- ggplot(CTI_CPI_phace_1000) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "1000km buffer occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

arrow_500 <- ggplot(CTI_CPI_phace_500) +
  geom_segment(aes(x = CTI_ambient, y = CPI_ambient, 
                   xend = CTI_warmed, yend = CPI_warmed,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_ambient, y = CPI_ambient), color = "black") +
  geom_point(aes(x = CTI_warmed, y = CPI_warmed), color = "red") +
  labs(x = "CTI", y = "CPI", title = "500km buffer occurrences") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()

# Combine figures into one multi-panel plot
ggarrange(arrow_uscan,arrow_eco9,arrow_1000,arrow_500,
          ncol = 4)



### Fig: phace --> comparing sensitivity btwn different gbif scales
CTI_uscan_smooth <- ggplot(CTI_sens_phace_uscan, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "US/Canada occurrences") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

CTI_eco9_smooth <- ggplot(CTI_sens_phace_eco9, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "Ecoregion 8 occurrences") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

CTI_1000_smooth <- ggplot(CTI_sens_phace_1000, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "1000km buffer occurrences") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

CTI_500_smooth <- ggplot(CTI_sens_phace_500, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "500km buffer occurrences") +
  #scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  theme_bw()

# Combine figures into one multi-panel plot
ggarrange(CTI_uscan_smooth,CTI_eco9_smooth,CTI_1000_smooth,CTI_500_smooth,
          ncol = 4)
