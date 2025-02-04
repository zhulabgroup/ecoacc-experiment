# TITLE:          Phylogenetic tree for species at sites
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     species list from each site
# DATA OUTPUT:    phylogenetic tree estimates
# PROJECT:        EcoAcc
# DATE:           Jan 2025



# Load packages
library(tidyverse)
library(V.PhyloMaker2)
library(taxize)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
phace <- read.csv(" phace_clean.csv")

# Make species list
species <- unique(phace$species)

# Getting family names
# Note: fix Lesquerellum species
families <- tax_name(species, get = "family", db = "ncbi")

# Pulling out genus name into its own column
families2 <- families %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)

# Make tree
phylo <- phylo.maker(families2)
phylo_scen3 <- phylo[[1]]


### Save tree information
### Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
saveRDS(phylo_scen3,paste(path_out,'phace_phylo_tree.rds'))


