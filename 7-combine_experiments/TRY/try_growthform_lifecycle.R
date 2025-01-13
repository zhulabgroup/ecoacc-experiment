# TITLE:          Getting plant growth form from TRY for all species
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

# Combine species lists and only keep one record if there are duplicates
all_species <- unique(c(tera_species, jrgce_species, phace_species, b4_species))

# Make species list into a dataframe with a column called AccSpeciesName to match try
all_species <- data.frame(AccSpeciesName = all_species)

# Renaming species to match the names found in Try
all_species$AccSpeciesName[all_species$AccSpeciesName == "Agropyron repens"] <- "Elymus repens"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Lysimachia arvensis"] <- "Anagallis arvensis"
all_species$AccSpeciesName[all_species$AccSpeciesName == "Stipa pulchra"] <- "Nassella pulchra"

# Merging try growth form species with my species list
try_spp <- left_join(all_species, try, by = c("AccSpeciesName"))

# Print a comma separated list of all AccSpeciesID to input into TRY
try_spp$AccSpeciesID[!is.na(try_spp$AccSpeciesID)]


# Data downloaded from TRY using above species list
# Set path to data
path_data = "/nfs/turbo/seas-zhukai/proj-ecoacc/TRY_data/"
setwd(path_data)
# Load in data
try_full_data <- read.csv("species_try_growthform.csv")



# Data from TRY: subsetting to just rows w/ growth form information
try_full_data <- try_full_data %>%
  filter(TraitID == 42)

# Selecting only species and growth form columns
try_sub_data <- try_full_data[,c(7,15)]
try_sub_data_dist <- try_sub_data %>%
  distinct()

# Using the growth forms from try & determining life cycle from google
all_species$growth_form <- NA
all_species$life_cycle <- NA

all_species$growth_form[all_species$AccSpeciesName == "Abies balsamea"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Abies balsamea"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Acer rubrum"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Acer rubrum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Acer saccharum"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Acer saccharum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Achillea millefolium"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Achillea millefolium"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Acmispon americanus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Acmispon americanus"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Aira caryophyllea"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Aira caryophyllea"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Allium textile"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Allium textile"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Amorpha canescens"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Amorpha canescens"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Anagallis arvensis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Anagallis arvensis"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Andropogon gerardi"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Andropogon gerardi"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Anemone cylindrica"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Anemone cylindrica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Antennaria rosea"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Antennaria rosea"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Artemisia dracunculus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Artemisia dracunculus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Artemisia frigida"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Artemisia frigida"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Asclepias tuberosa"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Asclepias tuberosa"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Avena barbata"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Avena barbata"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Avena fatua"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Avena fatua"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Baccharis pilularis"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Baccharis pilularis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Betula alleghaniensis"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Betula alleghaniensis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Betula papyrifera"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Betula papyrifera"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Bouteloua dactyloides"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Bouteloua dactyloides"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Bouteloua gracilis"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Bouteloua gracilis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Brachypodium distachyon"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Brachypodium distachyon"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Briza maxima"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Briza maxima"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Briza minor"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Briza minor"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Bromus diandrus"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Bromus diandrus"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Bromus hordeaceus"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Bromus hordeaceus"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Bromus inermis"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Bromus inermis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Carduus pycnocephalus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Carduus pycnocephalus"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Carex duriuscula"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Carex duriuscula"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Centaurea diffusa"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Centaurea diffusa"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Centaurea solstitialis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Centaurea solstitialis"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Chenopodium leptophyllum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Chenopodium leptophyllum"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Cirsium undulatum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Cirsium undulatum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Comandra umbellata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Comandra umbellata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Convolvulus arvensis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Convolvulus arvensis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Crepis vesicaria"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Crepis vesicaria"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Cryptantha cinerea"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Cryptantha cinerea"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Cryptantha thyrsiflora"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Cryptantha thyrsiflora"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Danthonia californica"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Danthonia californica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Delphinium geyeri"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Delphinium geyeri"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Descurainia pinnata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Descurainia pinnata"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Descurainia sophia"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Descurainia sophia"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Dipsacus sativus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Dipsacus sativus"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Draba reptans"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Draba reptans"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Elymus glaucus"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Elymus glaucus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Elymus repens"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Elymus repens"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Epilobium brachycarpum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Epilobium brachycarpum"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Erigeron canadensis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Erigeron canadensis"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Erigeron pumilus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Erigeron pumilus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Eriogonum alatum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Eriogonum alatum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Eriogonum effusum"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Eriogonum effusum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Erodium botrys"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Erodium botrys"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Erodium cicutarium"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Erodium cicutarium"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Erysimum asperum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Erysimum asperum"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Escobaria vivipara"] <- "Cactus"
all_species$life_cycle[all_species$AccSpeciesName == "Escobaria vivipara"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Festuca bromoides"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Festuca bromoides"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Festuca perennis"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Festuca perennis"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Frangula alnus"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Frangula alnus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Gastridium phleoides"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Gastridium phleoides"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Genista monspessulana"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Genista monspessulana"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Geranium dissectum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Geranium dissectum"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Grindelia squarrosa"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Grindelia squarrosa"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Helminthotheca echioides"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Helminthotheca echioides"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Hemizonia congesta"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Hemizonia congesta"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Hesperostipa comata"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Hesperostipa comata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Heterotheca villosa"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Heterotheca villosa"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Hypochaeris glabra"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Hypochaeris glabra"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Juncus bufonius"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Juncus bufonius"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Kickxia spuria"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Kickxia spuria"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Koeleria macrantha"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Koeleria macrantha"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Koeleria cristata"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Koeleria cristata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lactuca serriola"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lactuca serriola"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Lappula occidentalis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lappula occidentalis"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Lepidium densiflorum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lepidium densiflorum"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Lespedeza capitata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lespedeza capitata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lesquerella montana"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lesquerella montana"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Leucocrinum montanum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Leucocrinum montanum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Liatris punctata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Liatris punctata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Linaria dalmatica"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Linaria dalmatica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Logfia gallica"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Logfia gallica"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Lonicera morrowii"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Lonicera morrowii"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lonicera tatarica"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Lonicera tatarica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lupinus perennis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lupinus perennis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lygodesmia juncea"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lygodesmia juncea"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Lythrum hyssopifolia"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Lythrum hyssopifolia"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Machaeranthera canescens"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Machaeranthera canescens"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Machaeranthera pinnatifida"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Machaeranthera pinnatifida"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Madia gracilis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Madia gracilis"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Medicago polymorpha"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Medicago polymorpha"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Mertensia lanceolata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Mertensia lanceolata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Mirabilis linearis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Mirabilis linearis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Musineon divaricatum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Musineon divaricatum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Nassella pulchra"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Nassella pulchra"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Oenothera coronopifolia"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Oenothera coronopifolia"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Oenothera suffrutescens"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Oenothera suffrutescens"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Pascopyrum smithii"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Pascopyrum smithii"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Penstemon albidus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Penstemon albidus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Petalostemum villosum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Petalostemum villosum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Phalaris aquatica"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Phalaris aquatica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Phlox hoodii"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Phlox hoodii"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Picea glauca"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Picea glauca"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Picradeniopsis oppositifolia"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Picradeniopsis oppositifolia"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Pinus banksiana"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Pinus banksiana"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Pinus resinosa"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Pinus resinosa"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Pinus strobus"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Pinus strobus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Pistacia atlantica"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Pistacia atlantica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Plantago patagonica"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Plantago patagonica"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Poa pratensis"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Poa pratensis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Poa secunda"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Poa secunda"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Populus tremouloides"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Populus tremouloides"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Potentilla pensylvanica"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Potentilla pensylvanica"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Pseudognaphalium californicum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Pseudognaphalium californicum"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Quercus agrifolia"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Quercus agrifolia"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Quercus lobata"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Quercus lobata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Quercus macrocarpa"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Quercus macrocarpa"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Quercus rubra"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Quercus rubra"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Rhamnus cathartica"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Rhamnus cathartica"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Rumex acetosella"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Rumex acetosella"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Rumex crispus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Rumex crispus"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Salsola tragus"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Salsola tragus"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Schizachyrium scoparium"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Schizachyrium scoparium"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Sisymbrium altissimum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Sisymbrium altissimum"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Sisyrinchium bellum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Sisyrinchium bellum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Solidago rigida"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Solidago rigida"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Sonchus asper"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Sonchus asper"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Sorghastrum nutans"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Sorghastrum nutans"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Sphaeralcea coccinea"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Sphaeralcea coccinea"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Taraxia ovata"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Taraxia ovata"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Tetraneuris acaulis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Tetraneuris acaulis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Thuja occidentalis"] <- "Tree"
all_species$life_cycle[all_species$AccSpeciesName == "Thuja occidentalis"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Torilis arvensis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Torilis arvensis"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Toxicodendron diversilobum"] <- "Shrub"
all_species$life_cycle[all_species$AccSpeciesName == "Toxicodendron diversilobum"] <- "Perennial"

all_species$growth_form[all_species$AccSpeciesName == "Tragopogon pratensis"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Tragopogon pratensis"] <- "Biennial"

all_species$growth_form[all_species$AccSpeciesName == "Trifolium dubium"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Trifolium dubium"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Trifolium hirtum"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Trifolium hirtum"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Urospermum picroides"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Urospermum picroides"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Vicia sativa"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Vicia sativa"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Vicia tetrasperma"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Vicia tetrasperma"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Vulpia octoflora"] <- "Graminoid"
all_species$life_cycle[all_species$AccSpeciesName == "Vulpia octoflora"] <- "Annual"

all_species$growth_form[all_species$AccSpeciesName == "Zeltnera davyi"] <- "Forb"
all_species$life_cycle[all_species$AccSpeciesName == "Zeltnera davyi"] <- "Annual"

# Fix column name
colnames(all_species)[which(names(all_species) == "AccSpeciesName")] <- "species"
all_species$species[all_species$species == "Elymus repens"] <- "Agropyron repens"
all_species$species[all_species$species == "Anagallis arvensis"] <- "Lysimachia arvensis"
all_species$species[all_species$species == "Nassella pulchra"] <- "Stipa pulchra"


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc/TRY_data/"
write.csv(all_species,paste(path_out,'exp_species_growth_lifecycle.csv'),row.names=F)

