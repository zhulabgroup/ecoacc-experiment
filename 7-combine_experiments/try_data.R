# TITLE:          Getting plant growth form from TRY for all species
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(rtry)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
tera <- read.csv(" teracon_clean.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)
# Load in data
jrgce <- read.csv(" jrgce_clean.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
phace <- read.csv(" phace_clean.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)
# Load in data
b4 <- read.csv(" b4warmed_clean.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/traits/TRY_growthform_species/"
setwd(path_data)
# Load in data
try <- read.csv("TryAccSpecies.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TRY_data/"
setwd(path_data)
# Load in data
try_full_data <- read.csv("species_try_growthform.csv")


# Make lists of all species in each experiment
tera_species <- unique(tera$species)
jrgce_species <- unique(jrgce$species)
phace_species <- unique(phace$species)
b4_species <- unique(b4$species)

# Combine species lists and only keep one record if there are duplicates
all_species <- unique(c(tera_species, jrgce_species, phace_species, b4_species))

# Make species list into a dataframe with a column called AccSpeciesName to match try
all_species <- data.frame(AccSpeciesName = all_species)

# Renaming species to match the names found in Try
all_species$AccSpeciesName[all_species$AccSpeciesName == "Agropyron repens"] <- "Elymus repens"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Koeleria cristata"] <- "Koeleria macrantha"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Lysimachia arvensis"] <- "Anagallis arvensis"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Stipa pulchra"] <- "Nassella pulchra"

# Merging try growth form species with my species list
try_spp <- left_join(all_species, try, by = c("AccSpeciesName"))

# Print a list of AccSpeciesID without NAs
try_spp$AccSpeciesID[!is.na(try_spp$AccSpeciesID)]

# Get growth form for all species?

