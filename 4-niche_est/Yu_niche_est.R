# TITLE:          Yu niche calculation 
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     CHELSA data w/ GBIF lat and long
# DATA OUTPUT:    Species temperature and precipitation niche calculations
# PROJECT:        EcoAcc
# DATE:           April 2025


# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)

# Load in data
chelsa_data <- read.csv(" CHELSA_chy.csv")

## Fixing spp names
# KNZ
chelsa_data$species[chelsa_data$species == "Symphyotrichum oblongifolium"] <- "Aster oblongifolius" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Bouteloua dactyloides"] <- "Buchloe dactyloides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Carex inops"] <- "Carex heliophila" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Erigeron canadensis"] <- "Conyza canadensis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Eleocharis sp"] <- "Eleocharis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Juncus sp"] <- "Juncus" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Brickellia eupatorioides"] <- "Kuhnia eupatorioides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pediomelum tenuiflorum"] <- "Psoralea tenuiflora" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Mimosa quadrivalvis"] <- "Schrankia nuttallii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Packera plattensis"] <- "Senecio plattensis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Andropogon gerardi"] <- "Andropogon gerardii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Sporobolus compositus"] <- "Sporobolus asper" # fixing spp name to match phace
# HYS
chelsa_data$species[chelsa_data$species == "Andropogon gerardi"] <- "Andropogon gerardii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Asclepias sp"] <- "Asclepias" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Symphyotrichum oblongifolium"] <- "Aster oblongifolius" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Astragalus sp"] <- "Astragalus" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Bouteloua dactyloides"] <- "Buchloe dactyloides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Oenothera serrulata"] <- "Calylophus serrulatus" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Chloris sp"] <- "Chloris" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Erigeron canadensis"] <- "Conyza canadensis"
chelsa_data$species[chelsa_data$species == "Croton sp"] <- "Croton" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Eriogonum sp"] <- "Eriogonum" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pelecyphora vivipara"] <- "Escobaria vivipara" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Oenothera suffrutescens"] <- "Gaura coccinea" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Oenothera curtiflora"] <- "Gaura parviflora" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Helianthus sp"] <- "Helianthus" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Brickellia eupatorioides"] <- "Kuhnia eupatorioides" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Xanthisma spinulosum"] <- "Machaeranthera pinnatifida" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Panicum sp"] <- "Panicum" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Elymus smithii"] <- "Pascopyrum smithii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Polygala sp"] <- "Polygala" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Polygonum sp"] <- "Polygonum" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pediomelum cuspidatum"] <- "Psoralea cuspidata" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pediomelum esculentum"] <- "Psoralea esculenta" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Pediomelum tenuiflorum"] <- "Psoralea tenuiflora" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Mimosa quadrivalvis"] <- "Schrankia nuttallii"
chelsa_data$species[chelsa_data$species == "Packera plattensis"] <- "Senecio plattensis"
chelsa_data$species[chelsa_data$species == "Sporobolus compositus"] <- "Sporobolus asper"
chelsa_data$species[chelsa_data$species == "Tradescantia sp"] <- "Tradescantia" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Triodanis sp"] <- "Triodanis" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Viola rafinesquei"] <- "Viola rafinesquii" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Festuca octoflora"] <- "Vulpia octoflora" # fixing spp name to match phace
chelsa_data$species[chelsa_data$species == "Castilleja purpurea"] <- "Castille japurpurea"
chelsa_data$species[chelsa_data$species == "Tomostima reptans"] <- "Draba reptans"
chelsa_data$species[chelsa_data$species == "Microseris cuspidata"] <- "Nothocalais cuspidata"
chelsa_data$species[chelsa_data$species == "Muhlenbergia paniculata"] <- "Schedonnardus paniculatus"
# SGS
chelsa_data$species[chelsa_data$species == "Bouteloua dactyloides"] <- "Buchloe dactyloides"
chelsa_data$species[chelsa_data$species == "Carex duriuscula"] <- "Carex eleocharis"
chelsa_data$species[chelsa_data$species == "Erigeron canadensis"] <- "Conyza canadensis"
chelsa_data$species[chelsa_data$species == "Tomostima reptans"] <- "Draba reptans"
chelsa_data$species[chelsa_data$species == "Pelecyphora vivipara"] <- "Escobaria vivipara"
chelsa_data$species[chelsa_data$species == "Evolvulus nuttallianus"] <- "Evolvulus nuttalliana"
chelsa_data$species[chelsa_data$species == "Oenothera suffrutescens"] <- "Gaura coccinea"
chelsa_data$species[chelsa_data$species == "Bassia scoparia"] <- "Kochia scoparia"
chelsa_data$species[chelsa_data$species == "Xanthisma spinulosum"] <- "Machaeranthera pinnatifida"
chelsa_data$species[chelsa_data$species == "Oenothera sp"] <- "Oenothera"
chelsa_data$species[chelsa_data$species == "Elymus smithii"] <- "Pascopyrum smithii"
chelsa_data$species[chelsa_data$species == "Pediomelum tenuiflorum"] <- "Psoralea tenuiflora" 
chelsa_data$species[chelsa_data$species == "Pediomelum tenuiflorum"] <- "Psoralidium tenuiflorum"
chelsa_data$species[chelsa_data$species == "Hesperostipa comata"] <- "Stipa comata"
chelsa_data$species[chelsa_data$species == "Phemeranthus parviflorus"] <- "Talinum parviflorum"
chelsa_data$species[chelsa_data$species == "Festuca octoflora"] <- "Vulpia octoflora"
# CHY
chelsa_data$species[chelsa_data$species == "Bouteloua dactyloides"] <- "Buchloe dactyloides"
chelsa_data$species[chelsa_data$species == "Carex duriuscula"] <- "Carex eleocharis"
chelsa_data$species[chelsa_data$species == "Erigeron canadensis"] <- "Conyza canadensis"
chelsa_data$species[chelsa_data$species == "Cymopterus glomeratus"] <- "Cymopterus acaulis"
chelsa_data$species[chelsa_data$species == "Tomostima reptans"] <- "Draba reptans"
chelsa_data$species[chelsa_data$species == "Pelecyphora vivipara"] <- "Escobaria vivipara"
chelsa_data$species[chelsa_data$species == "Oenothera suffrutescens"] <- "Gaura coccinea"
chelsa_data$species[chelsa_data$species == "Physaria montana"] <- "Lesquerella montana"
chelsa_data$species[chelsa_data$species == "Xanthisma spinulosum"] <- "Machaeranthera pinnatifida"
chelsa_data$species[chelsa_data$species == "Elymus smithii"] <- "Pascopyrum smithii"
chelsa_data$species[chelsa_data$species == "Hesperostipa comata"] <- "Stipa comata"
chelsa_data$species[chelsa_data$species == "Festuca octoflora"] <- "Vulpia octoflora"



## Calculating niche based on median value
niche_est <- chelsa_data %>%
  group_by(species) %>%
  mutate(temp_niche = median(mean_annual_temp)) %>%
  mutate(precip_niche = median(mean_annual_precip)) %>%
  dplyr::select(-c(ID,ID.1,CHELSA_bio1_1981.2010_V.2.1,CHELSA_bio12_1981.2010_V.2.1,latitude,longitude,mean_annual_temp,mean_annual_precip)) %>%
  distinct()


# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
write.csv(niche_est,paste(path_out,'chy_niche.csv'),row.names=F)
