# TITLE:          Oklahoma exp. data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Raw data imported as csv file
# DATA OUTPUT:    Cleaned OK experiment data
# PROJECT:        EcoAcc
# DATE:           Oct 2024

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/Oklahoma_warming_exp"
setwd(path_data)

# Read in data
ok_meta <- read.csv("OK_metadata.csv")
ok_2000 <- read.csv("OK_rel_abun_2000.csv")
ok_2001 <- read.csv("OK_rel_abun_2001.csv")
ok_2002 <- read.csv("OK_rel_abun_2002.csv")
ok_2003 <- read.csv("OK_rel_abun_2003.csv")
ok_2004 <- read.csv("OK_rel_abun_2004.csv")
ok_2005 <- read.csv("OK_rel_abun_2005.csv")
ok_2006 <- read.csv("OK_rel_abun_2006.csv")
ok_2007 <- read.csv("OK_rel_abun_2007.csv")
ok_2008 <- read.csv("OK_rel_abun_2008.csv")
ok_2009 <- read.csv("OK_rel_abun_2009.csv")
ok_2010 <- read.csv("OK_rel_abun_2010.csv")
ok_2011 <- read.csv("OK_rel_abun_2011.csv")
ok_2012 <- read.csv("OK_rel_abun_2012.csv")
ok_2013 <- read.csv("OK_rel_abun_2013.csv")
ok_anpp <- read.csv("OK_ANPP_OldWarmingSite(2000-2013).csv")

# Wide to long for the relative abundance data
df_list <- list(ok_2000, ok_2001, ok_2002, ok_2003, ok_2004, ok_2005, ok_2006, ok_2007, ok_2008, ok_2009, ok_2010, ok_2011, ok_2012, ok_2013)
wide_to_long <- function(df){
  df_long <- df %>%
    pivot_longer(cols = -c(Year, Plot),
                 names_to = "species", values_to = "rel_abun")
  return(df_long)
}
df_long_list <- lapply(df_list, wide_to_long)

# Merge all the long dataframes
ok_rel_abun <- bind_rows(df_long_list)

# Replace the dot in the species names with a space
ok_rel_abun$species <- gsub("\\.", " ", ok_rel_abun$species)

# Fixing species names to match the names in the meta data
ok_rel_abun$species <- gsub("Tra  bet", "Tra bet", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Tri cam", "Tri can", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Tri  fla", "Tri fla", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Mel alb", "Med alb", ok_rel_abun$species)
ok_rel_abun$species <- gsub("lia squ", "Lia squ", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Gua par", "Gau par", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Gail aes", "Gai aes", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Ele sp ", "Ele sp", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Dio ocy", "DO", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Cha fac", "Cha fas", ok_rel_abun$species)
ok_rel_abun$species <- gsub("Cha fac", "Cha fas", ok_rel_abun$species)
# What about the species that are not in the metadata?

# Merge abundance data with meta data by species code
ok_rel_spp <- ok_rel_abun %>%
  left_join(ok_meta, by = c("species" = "Abbreviation"))

# Selecting columns
ok_rel_spp <- ok_rel_spp %>%
  select(Year, Plot, rel_abun, Lartin.name)

# Renaming columns
colnames(ok_rel_spp) <- c("year", "plot", "rel_abun", "species")

# Assigning treatments to plots
ok_rel_spp <- ok_rel_spp %>%
  mutate(temp_treatment = if_else(str_detect(plot, "W"), "warmed", "ambient"))


# ANPP data
# Selecting columns
ok_anpp <- ok_anpp %>%
  select(year, plot, warming, ANPP)

# Renaming columns
colnames(ok_anpp) <- c("year", "plot", "temp_treatment", "ab_biomass")


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
write.csv(ok_rel_spp,paste(path_out,'ok_clean.csv'))
write.csv(ok_anpp,paste(path_out,'ok_ecosystem_dat_clean.csv'))


