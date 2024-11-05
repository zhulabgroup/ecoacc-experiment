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
path_abun_data = "/nfs/turbo/seas-zhukai/proj-grassland-cfp/intermediate/observation-experiment/final-community/"
path_bio_data = "/nfs/turbo/seas-zhukai/datasets/vegetation/JRGCE/JRGCE Harvest1 AGB/"
path_meta_data = "/nfs/turbo/seas-zhukai/datasets/vegetation/JRGCE/"
path_home = "/home/kcdobson"
setwd(path_abun_data)

# Read in data
jrgce_abun_data <- read.csv("jrgce.csv")
jrgce_meta_data <- read.csv("JRGCE treatments 1998_2014 plus single code for all treatments.csv")
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
  left_join(jrgce_meta_data %>% select(id, plot, quad, treatmentsummary),
            by = c("id", "plot", "quad"))

# Fixing column nmes
colnames(jrgce_bio_data)[which(names(jrgce_bio_data) == "mass_gm2")] <- "ab_biomass"
colnames(jrgce_bio_data)[which(names(jrgce_bio_data) == "treatmentsummary")] <- "treatment"
colnames(jrgce_abun_data)[which(names(jrgce_abun_data) == "abund")] <- "percent_cover"
colnames(jrgce_abun_data)[which(names(jrgce_abun_data) == "treat")] <- "treatment"

# Upload data
path_out = "/nfs/turbo/seas-zhukai/proj-ecoacc/JRGCE/"
write.csv(jrgce_abun_data,paste(path_out,'jrgce_clean.csv'))
write.csv(jrgce_bio_data,paste(path_out,'jrgce_ecosystem_dat_clean.csv'))
s


