# TITLE:          JRGCE data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Semi-cleaned data imported as csv file from grassland proj
# DATA OUTPUT:    Cleaned JRGCE experiment data
# PROJECT:        EcoAcc
# DATE:           Nov 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_abun_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_abun_data)
# Read in data
load("dat_community.rda")
exp <- dat_community[[2]]
jrgce_abun_data <- exp %>%
  filter(site == 'jrgce')

# Set path to turbo to get data
path_bio_data = "/Volumes/seas-zhukai/datasets/vegetation/JRGCE/JRGCE Harvest1 AGB/"
setwd(path_bio_data)
# Read in all biomass data files at once
temp = list.files(pattern="\\.csv$")
for (i in 1:length(temp)) {
  file_name <- gsub("\\.csv$", "", temp[i])
  var_name <- make.names(file_name)
  data <- read.csv(temp[i])
  year <- as.numeric(file_name)
  data$Year <- year
  assign(var_name, data)
}

# Set path to turbo to get data
path_meta_data = "/Volumes/seas-zhukai/datasets/vegetation/JRGCE/"
setwd(path_meta_data)
# Read in data
jrgce_meta_data <- read.csv("JRGCE treatments 1998_2014 plus single code for all treatments.csv")
jrgce_temp_data <- read.csv("JRGCE environmental summary for MIDPOINT, H1, H1, DOY 308_126.csv")

# Fixing data to match before merging all files
X2006 <- X2006[,c(1:10,13)]
colnames(X2014)[which(names(X2014) == "Species")] <- "name_during_sort"
colnames(X1998) <- colnames(X2014)
colnames(X1999) <- colnames(X2014)
colnames(X2000) <- colnames(X2014)
colnames(X2001) <- colnames(X2014)
colnames(X2002) <- colnames(X2014)
colnames(X2003) <- colnames(X2014)
colnames(X2004) <- colnames(X2014)
colnames(X2005) <- colnames(X2014)
colnames(X2006) <- colnames(X2014)
colnames(X2007) <- colnames(X2014)
colnames(X2008) <- colnames(X2014)
colnames(X2009) <- colnames(X2014)
colnames(X2010) <- colnames(X2014)
colnames(X2011) <- colnames(X2014)
colnames(X2012) <- colnames(X2014)
colnames(X2013) <- colnames(X2014)
# Merging all biomass files into one
jrgce_bio_data <- rbind(X1998,X1999,X2000,X2001,X2002,X2003,X2004,X2005,X2006,X2007,X2008,
                        X2009,X2010,X2011,X2012,X2013,X2014)
colnames(jrgce_bio_data) <- tolower(colnames(jrgce_bio_data))

# Merging with meta-data on plot treatments
jrgce_bio_data <- jrgce_bio_data %>%
  left_join(jrgce_meta_data %>% dplyr::select(id, plot, quad, treatmentsummary),
            by = c("id", "plot", "quad"))

# Fixing column nmes
colnames(jrgce_bio_data)[which(names(jrgce_bio_data) == "mass_gm2")] <- "ab_biomass"
colnames(jrgce_bio_data)[which(names(jrgce_bio_data) == "treatmentsummary")] <- "treatment"
colnames(jrgce_bio_data)[which(names(jrgce_bio_data) == "species_or_other.name")] <- "species"
colnames(jrgce_abun_data)[which(names(jrgce_abun_data) == "abund")] <- "percent_cover"
colnames(jrgce_abun_data)[which(names(jrgce_abun_data) == "treat")] <- "treatment"

# Merging with temp data
jrgce_temp_data <- jrgce_temp_data %>%
  filter(VARIABLE == "AVG TEMP SEASON START TO MIDPT BETWEEN HARVESTS" &
           TREATMENT == "AMB")
jrgce_temp_filtered <- jrgce_temp_data[,c(1,5)]
colnames(jrgce_temp_filtered)[which(names(jrgce_temp_filtered) == "VALUE")] <- "mean_C_temp_summer"
colnames(jrgce_temp_filtered)[which(names(jrgce_temp_filtered) == "harvest_year")] <- "year"
jrgce_abun_data <- left_join(jrgce_abun_data, jrgce_temp_filtered, by="year")
jrgce_bio_data <- left_join(jrgce_bio_data, jrgce_temp_filtered, by="year")

# Fixing treatment names
jrgce_abun_data <- jrgce_abun_data %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "T"), "warmed", "ambient")) %>%
  dplyr::select(year,plot,species,percent_cover,temp_treatment,mean_C_temp_summer) %>%
  filter(!(year == 1998))
jrgce_bio_data <- jrgce_bio_data %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "H"), "warmed", "ambient")) %>%
  dplyr::select(year,plot,species,ab_biomass,temp_treatment) %>%
  filter(!(year == 1998))


# Calculating relative abundance from percent cover
rel_abun_calc <- function(df) {
  df %>%
    filter(!(species == "Avena DUMMY" | # remove dummy spp and spp with no temp niches
               species == "Festuca DUMMY")) %>%
    group_by(year, plot) %>%
    mutate(total_cover = sum(percent_cover,na.rm=T)) %>%
    mutate(rel_abun = percent_cover / total_cover) %>%
    filter(!is.na(rel_abun) & rel_abun != "NaN") %>%
    ungroup() %>%
    dplyr::select(year,plot,species,temp_treatment,mean_C_temp_summer,rel_abun)
}
rel_abun_jrgce <- rel_abun_calc(jrgce_abun_data)




# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
write.csv(rel_abun_jrgce,paste(path_out,'jrgce_clean.csv'),row.names=F)
write.csv(jrgce_bio_data,paste(path_out,'jrgce_ecosystem_dat_clean.csv'),row.names=F)



