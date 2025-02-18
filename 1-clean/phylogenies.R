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
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
phace <- read.csv(" phace_clean.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
jrgce <- read.csv(" jrgce_clean.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
tera <- read.csv(" teracon_clean.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
ok <- read.csv(" ok_clean.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
cfc <- read.csv(" b4warmed_cfc_clean.csv")
hwrc <- read.csv(" b4warmed_hwrc_clean.csv")

# Make species list
species_phace <- unique(phace$species)
species_jrgce <- unique(jrgce$species)
species_tera <- unique(tera$species)
species_tera[species_tera == "Andropogon gerardi"] <- "Andropogon gerardii"
species_ok <- unique(ok$species)
species_b4 <- unique(cfc$species) # same species in cfc and hwrc

# Merging species lists and removing duplicates
species <- unique(c(species_phace,species_jrgce,species_tera,species_ok,species_b4))
species <- species[species != "Andropogon gerardi"]

# Getting family names
# Note: species that do not get a family designation
families_phace <- tax_name(species_phace, get = "family", db = "ncbi")
families_jrgce <- tax_name(species_jrgce, get = "family", db = "ncbi")
families_tera <- tax_name(species_tera, get = "family", db = "ncbi")
families_ok <- tax_name(species_ok, get = "family", db = "ncbi")
families_b4 <- tax_name(species_b4, get = "family", db = "ncbi")
# All species
families <- tax_name(species, get = "family", db = "ncbi")

# Pulling out genus name into its own column
families_phace2 <- families_phace %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)
families_jrgce2 <- families_jrgce %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)
families_tera2 <- families_tera %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)
families_ok2 <- families_ok %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)
families_b42 <- families_b4 %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)
# All species
families2 <- families %>%
  separate(query, into = c("genus", "species_ep"), sep = " ",remove=F) %>%
  rename(species = query) %>%
  dplyr::select(species,genus,family)

# Removing non-species and NAs
families2 <- families2 %>%
  filter(!is.na(species)) %>%
  filter(!grepl("spp",species))

# Manually adding families the package missed
families2$family[families2$genus == "Lesquerella"] <- "Brassicaceae"
families2$family[families2$genus == "Zeltnera"] <- "Gentianaceae"
families2$family[families2$genus == "Calylophus"] <- "Onagraceae"
families2$family[families2$genus == "Solidago"] <- "Asteraceae"
families2$family[families2$genus == "Rubus"] <- "Rosaceae"
families2$family[families2$genus == "Symphyotrichum"] <- "Asteraceae"
families2$family[families2$genus == "Haplopappus"] <- "Asteraceae"
families2$family[families2$genus == "Ambrosia"] <- "Asteraceae"
families2$family[families2$genus == "Diodia"] <- "Rubiaceae"
families2$family[families2$genus == "Grindelia"] <- "Asteraceae"
families2$family[families2$genus == "Tragia"] <- "Euphorbiaceae"
families2$family[families2$genus == "Populus"] <- "Salicaceae"

# Make tree
phylo_maker_phace <- phylo.maker(families_phace2)
phylo_phace <- phylo_maker_phace[[1]]

phylo_maker_jrgce <- phylo.maker(families_jrgce2)
phylo_jrgce <- phylo_maker_jrgce[[1]]

phylo_maker_tera <- phylo.maker(families_tera2)
phylo_tera <- phylo_maker_tera[[1]]

phylo_maker_ok <- phylo.maker(families_ok2)
phylo_ok <- phylo_maker_ok[[1]]

phylo_maker_b4 <- phylo.maker(families_b42)
phylo_b4 <- phylo_maker_b4[[1]]

phylo_maker <- phylo.maker(families2)
total_phylo <- phylo_maker[[1]]


### Save tree information
### Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/phylogenies/"
saveRDS(phylo_phace,paste(path_out,'phace_phylo_tree.rds'))
saveRDS(phylo_jrgce,paste(path_out,'jrgce_phylo_tree.rds'))
saveRDS(phylo_tera,paste(path_out,'tera_phylo_tree.rds'))
saveRDS(phylo_ok,paste(path_out,'ok_phylo_tree.rds'))
saveRDS(phylo_b4,paste(path_out,'b4_phylo_tree.rds'))
saveRDS(total_phylo,paste(path_out,'all_species_phylo.rds'))


