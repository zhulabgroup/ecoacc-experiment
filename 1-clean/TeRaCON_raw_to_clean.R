# TITLE:          TeRaCON data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned TeRaCON experiment data
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/TeRaCON"
setwd(path_data)

# Read in data
teracon_data <- read.csv("teracon harvest file for Kara Dobson_241021.csv")

# Selecting columns to keep
colnames(teracon_data)
teracon_data_sub <- teracon_data[,c(2:3,5,11,27,109:125)] # for % cover
teracon_data_sub_eco <- teracon_data[,c(2:3,5,11,27,55,68:69,74,76,78,80,82,84)] # for ecosystem response

# Removing first test row
teracon_data_sub <- teracon_data_sub[2:1153,]
teracon_data_sub_eco <- teracon_data_sub_eco[2:1153,]

# Wide to long for just percent cover values
teracon_data_long <- teracon_data_sub %>%
  pivot_longer(cols = -c(year, Season, Plot, Temp.Treatment,Mean.Amb.May.June.July.Temp..F.),
               names_to = "Species", values_to = "Percent_cover")

# Fixing species names
transform_species_name <- function(name) {
  # Remove the prefix
  name <- sub("^X\\.\\.cover\\.", "", name)
  # Replace periods with spaces
  name <- gsub("\\.", " ", name)
  return(name)
}

# Apply the transformation to the Species column
teracon_data_long$Species <- sapply(teracon_data_long$Species, transform_species_name)
# Fixing species name
teracon_data_long$Species[teracon_data_long$Species == "Andropogon gerardi"] <- "Andropogon gerardii"

# Scaling temperature data from F to C
teracon_data_long <- teracon_data_long %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)
teracon_data_sub_eco <- teracon_data_sub_eco %>%
  rename(mean_temp_summer = Mean.Amb.May.June.July.Temp..F.) %>%
  mutate(mean_C_temp_summer = (mean_temp_summer-32)*5/9)

# Harmonizing column names - these should match across all experiments
teracon_data_long <- teracon_data_long %>%
  rename(plot = Plot) %>%
  rename(species = Species) %>%
  rename(percent_cover = Percent_cover) %>%
  rename(temp_treatment = Temp.Treatment)
teracon_data_sub_eco <- teracon_data_sub_eco %>%
  rename(plot = Plot) %>%
  rename(temp_treatment = Temp.Treatment) %>%
  rename(ab_biomass = AbovegroundTotal.Biomass..g.m.2.) %>%
  rename(bl_biomass = Total.root.biomass.0.20..g.m.2.) %>%
  rename(total_biomass = Total.Biomass) %>%
  rename(root_ingrowth = Annual.Total.Root.Ingrowth..g.m.2.) %>%
  rename(total_n = Whole.Plot.Total.N..g.m.2.) %>%
  rename(bl_n = Belowground.N..total....g.m.2.) %>%
  rename(bl_c = Belowground.Carbon...) %>%
  rename(ab_n = Aboveground.N..total....g.m.2.) %>%
  rename(ab_c = Aboveground.Carbon...) %>%
  mutate(biomass_plus_root = ab_biomass+root_ingrowth)


# Selecting August as the season and fixing treatment names
teracon_data_long2 <- teracon_data_long %>%
  filter(Season == "August") %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "elv"), "warmed", "ambient")) %>%
  select(year,plot,temp_treatment,species,percent_cover,mean_C_temp_summer)
teracon_data_sub_eco2 <- teracon_data_sub_eco %>%
  filter(Season == "August") %>%
  mutate(temp_treatment = if_else(str_detect(temp_treatment, "elv"), "warmed", "ambient")) %>%
  select(year,plot,temp_treatment,ab_biomass,bl_biomass,total_biomass,root_ingrowth,total_n,bl_n,bl_c,ab_n,ab_c,biomass_plus_root)



# Calculating relative abundance from percent cover
rel_abun_calc <- function(df) {
  df %>%
    filter(!(species == "Total Planted Species" | # removing non-spp and spp without niche values
               species == "Petalostemum villosum")) %>%
    mutate(percent_cover = if_else(is.na(percent_cover), 0, percent_cover)) %>%
    group_by(year, plot) %>%
    mutate(total_cover = sum(percent_cover,na.rm=T)) %>%
    mutate(rel_abun = percent_cover / total_cover) %>%
    filter(!is.na(rel_abun) & rel_abun != "NaN") %>%
    ungroup() %>%
    dplyr::select(year,plot,species,temp_treatment,mean_C_temp_summer,rel_abun)
}
rel_abun_tera <- rel_abun_calc(teracon_data_long2)

  

# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
write.csv(rel_abun_tera,paste(path_out,'teracon_clean.csv'),row.names=F)
write.csv(teracon_data_sub_eco2,paste(path_out,'teracon_ecosystem_dat_clean.csv'))

