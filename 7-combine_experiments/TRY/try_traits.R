# TITLE:          Getting trait data from TRY for all species
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Jan 2025

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
path_data = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)
# Load in data
ok <- read.csv(" ok_clean.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/datasets/vegetation/traits/TRY/"
setwd(path_data)
# Load in data
try <- read.csv("TRY_species_list.csv")


# Make lists of all species in each experiment
tera_species <- unique(tera$species)
jrgce_species <- unique(jrgce$species)
phace_species <- unique(phace$species)
b4_species <- unique(b4$species)
ok_species <- unique(ok$species)

# Combine species lists and only keep one record if there are duplicates
all_species <- unique(c(tera_species, jrgce_species, phace_species, b4_species, ok_species))

# Make species list into a dataframe with a column called AccSpeciesName to match try
all_species <- data.frame(AccSpeciesName = all_species)
all_species <- all_species %>%
  filter(!(AccSpeciesName == "Total Planted Species" |
             AccSpeciesName == "Festuca DUMMY" |
             AccSpeciesName == "Avena DUMMY" |
             AccSpeciesName == "Eleocharis spp" |
             AccSpeciesName == "Cyperus spp"))

# Renaming species to match the names found in Try
all_species$AccSpeciesName[all_species$AccSpeciesName == "Agropyron repens"] <- "Elymus repens"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Lysimachia arvensis"] <- "Anagallis arvensis"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Stipa pulchra"] <- "Nassella pulchra"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Andropogon gerardi"] <- "Andropogon gerardii"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Koeleria cristata"] <- "Koeleria macrantha"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Festuca perennis"] <- "Lolium perenne"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Taraxia ovata"] <- "Camissonia ovata"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Festuca bromoides"] <- "Vulpia bromoides"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Logfia gallica"] <- "Filago gallica"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Pascopyrum smithii"] <- "Elymus smithii"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Hesperostipa comata"] <- "Stipa comata"                       
all_species$AccSpeciesName[all_species$AccSpeciesName == "Picradeniopsis oppositifolia"] <- "Bahia oppositifolia"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Heterotheca villosa"] <- "Chrysopsis villosa"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Cryptantha thyrsiflora"] <- "Oreocarya thyrsiflora"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Cryptantha cinerea"] <- "Oreocarya suffruticosa"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Draba reptans"] <- "Tomostima reptans"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Machaeranthera pinnatifida"] <- "Xanthisma spinulosum"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Lesquerella montana"] <- "Physaria montana"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Machaeranthera canescens"] <- "Dieteria canescens"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Salsola tragus"] <- "Kali tragus"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Populus tremouloides"] <- "Populus tremuloides"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Calylophus serrulatus"] <- "Oenothera serrulata"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Coreopsis grandiflora"] <- "Bidens sweetiana"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Dichanthelium oligosanthes"] <- "Panicum oligosanthes"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Gaura parviflora"] <- "Oenothera curtiflora"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Hedyotis nigricans"] <- "Stenaria nigricans"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Psoralidium tenuiflorum"] <- "Pediomelum tenuiflorum"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Solidago ludiviciana"] <- "Solidago ludoviciana"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Stenosiphon linifolius"] <- "Oenothera glaucifolia"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Symphyotrichum ericoides "] <- "Symphyotrichum ericoides"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Amphiachyris dracunculoides"] <- "Gutierrezia dracunculoides"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Diodia teres"] <- "Hexasepalum teres"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Stipa tenuissima"] <- "Nassella tenuissima"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Rubus calycinoides"] <- "Rubus rolfei"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Rubus pentalobus"] <- "Rubus rolfei"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Ruellia hirta"] <- "Strobilanthes glutinosus"
all_species$AccSpeciesName[all_species$AccSpeciesName == "solidago drummondii"] <- "Solidago drummondii"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Symphyotrichum ontarionsis"] <- "Symphyotrichum ontarionis"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Haplopappus ciliatus"] <- "Grindelia ciliata"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Heterotheca latifolia"] <- "Heterotheca subaxillaris"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Muhlenbergia capillaris "] <- "Muhlenbergia capillaris"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Setaria glauca"] <- "Pennisetum glaucum"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Ambrosia trida"] <- "Ambrosia trifida"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Diodia ocymifolia"] <- "Spermacoce ocymifolia"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Grindelia papposa"] <- "Grindelia ciliata"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Tragia cannabina"] <- "Tragia plukenetii"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Trifolium purpureum "] <- "Trifolium purpureum"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Melilotus alba"] <- "Trigonella alba"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Coreopsis tinctoria"] <- "Bidens tinctoria"

# Remove duplicate rows
all_species <- all_species %>%
  distinct()

# Merging try growth form species with my species list
try_spp <- left_join(all_species, try, by = c("AccSpeciesName"))

# Print a list of AccSpeciesID without NAs (used this list to download data from TRY for these species IDs)
try_spp$AccSpeciesID[!is.na(try_spp$AccSpeciesID)]

# Traits to get from TRY
# Leaf thickness (46), SLA (3117), fine root length (2026), specific fine root length (614), 
# coarse root length (1770), specific coarse root length (1550), specific root length (1080),
# dispersal syndrome (28), plant height (3106), LDMC (47), stem diameter (21), growth form (3400), PFT (197)


# Read in data from TRY for the species and traits above


