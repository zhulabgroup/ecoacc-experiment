# TITLE:          PHACE data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned PHACE experiment data
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/PHACE"
setwd(path_data)

# Read in data
phace_data <- read.csv("PHACE biomass by species for Kara.csv", fileEncoding = "Latin1")
temp_data <- read.csv("dbo_tblHourlyThermocouple.csv")
phace_origin <- read.csv("PHACE_origin.csv")


### Cleaning abundance/biomass data
# Wide to long for biomass values for relative abundance calculation
phace_data_long <- phace_data %>%
  pivot_longer(cols = -c(YEAR,PLOT,COLOR,REP,BLOCK,CO2,carbon.dioxide,TEMP,temperature,Treatment,Aboveground.biomass..g.per.m2.),
               names_to = "species", values_to = "biomass")
phace_data_long$species[phace_data_long$species == "MachaerantheraÊpinnatifida"] <- "Machaeranthera pinnatifida" # Fix spp name
phace_data_long$species[phace_data_long$species == "Astragalus.Spp."] <- "Astragalus sp" # Fix spp name

# Calculating relative abundance and fixing column names
phace_abun <- phace_data_long %>%
  group_by(YEAR,PLOT) %>%
  mutate(total_biomass = sum(biomass)) %>%
  mutate(rel_abun = biomass/total_biomass) %>%
  rename_all(~ str_to_lower(.)) %>%
  rename(temp_treatment = temperature) %>%
  rename(co2_treatment = carbon.dioxide)

# Use gsub to replace periods with spaces
phace_abun$species <- gsub("\\.", " ", phace_abun$species)

# Subsetting data to begin in 2007 when the warming treatment
phace_rel_abun <- phace_abun %>%
  filter(year >= 2007) %>%
  dplyr::select(year,plot,temp_treatment,species,rel_abun)

# Selecting biomass data
phace_biomass <- phace_abun %>%
  filter(year >= 2007) %>%
  dplyr::select(year,plot,temp_treatment,species,total_biomass)



### Cleaning temp data
# Making data column a date
temp_data$SampDate2 <- as.Date(temp_data$SampDate, format = "%m/%d/%y")

# Remove error values and selecting DST 1 (temps at 10cm above soil surface)
temp_data <- temp_data %>%
  filter(!(TempC == -9999)) %>%
  filter(DST_Code == 1)

# Pulling out each year and month
temp_data$year <- format(temp_data$SampDate2,format="%Y")
temp_data$month <- format(temp_data$SampDate2,format="%m")
temp_data$year_month <- format(temp_data$SampDate2,format="%Y-%m")

# Average temperature per treatment, per year
# Note: 2006 starts in July (not Jan), and 2013 ends in June (not Dec)
temp_avg <- temp_data %>%
  group_by(TreatmentCode,year) %>%
  summarize(mean_temp = mean(TempC))

# Merge with species origin data
phace_origin <- phace_origin %>%
  rename_all(~ str_to_lower(.)) %>%
  rename(origin = native.introduced,
         invasive = invasive..yes.no.) %>%
  dplyr::select(c(species,origin,invasive))
phace_origin$species[phace_origin$species == "Machaeranthera\xcapinnatifida"] <- "Machaeranthera pinnatifida" # Fix spp name
phace_origin$species[phace_origin$species == "Astragalus Spp."] <- "Astragalus sp" # Fix spp name

phace_rel_abun <- left_join(phace_rel_abun, phace_origin, by = "species")
phace_rel_abun$origin[phace_rel_abun$origin == "Native"] <- "native"
phace_rel_abun$origin[phace_rel_abun$origin == "Introduced"] <- "non-native"
phace_rel_abun$invasive[phace_rel_abun$invasive == "N"] <- "not invasive"
phace_rel_abun$invasive[phace_rel_abun$invasive == "Y"] <- "invasive"


### Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
write.csv(phace_rel_abun,paste(path_out,'phace_clean.csv'), row.names=F)
write.csv(phace_biomass,paste(path_out,'phace_ecosystem_dat_clean.csv'), row.names=F)


