# TITLE:          TRY Trait plots
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Yiluan Song, Kai Zhu, Peter Reich
# DATA INPUT:     TRY trait data
# DATA OUTPUT:    Plots of traits
# PROJECT:        EcoAcc
# DATE:           Feb 2025



### Load packages
library(tidyverse)
library(gridExtra)



### Set path to turbo to get data
# Set path to turbo to get TRY data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TRY_data/"
setwd(path_data)
# Load in data
traits <- read.csv(" exp_species_traits.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
# Load in data
niche_est_phace <- read.csv(" phace_niche.csv")
niche_est_phace$site <- "PHACE"
phace <- read.csv(" phace_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
# Load in data
niche_est_tera <- read.csv(" teracon_niche.csv")
niche_est_tera$site <- "TeRaCON"
tera <- read.csv(" teracon_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
# Load in data
niche_est_cfc <- read.csv(" b4warmed_cfc_niche.csv")
niche_est_hwrc <- read.csv(" b4warmed_hwrc_niche.csv")
b4_cfc <- read.csv(" b4warmed_cfc_clean.csv")
b4_hwrc <- read.csv(" b4warmed_hwrc_clean.csv")
niche_est_cfc$site <- "B4WarmED CFC"
niche_est_hwrc$site <- "B4WarmED HWRC"

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
# Load in data
niche_est_ok <- read.csv(" ok_niche.csv")
niche_est_ok$site <- "Oklahoma"
ok <- read.csv(" ok_clean.csv")

### Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
# Load in data
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
niche_est_jrgce$site <- "JRGCE"
jrgce <- read.csv(" jrgce_clean.csv")



### Make a dataframe containing all species and their temp niches
tera_niche_filt <- niche_est_tera %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()
jrgce_niche_filt <- niche_est_jrgce %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()
phace_niche_filt <- niche_est_phace %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()
b4_cfc_niche_filt <- niche_est_cfc %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()
b4_hwrc_niche_filt <- niche_est_hwrc %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()
ok_niche_filt <- niche_est_ok %>%
  dplyr::select(site,species,temp_niche) %>%
  distinct()

# Combine all species and temp niches into one dataframe
all_niche <- rbind(tera_niche_filt,jrgce_niche_filt,phace_niche_filt,b4_cfc_niche_filt,b4_hwrc_niche_filt,ok_niche_filt)

# Renaming species to match the names found in Try
all_niche$species[all_niche$species == "Agropyron repens"] <- "Elymus repens"
all_niche$species[all_niche$species == "Lysimachia arvensis"] <- "Anagallis arvensis"
all_niche$species[all_niche$species == "Stipa pulchra"] <- "Nassella pulchra"
all_niche$species[all_niche$species == "Andropogon gerardi"] <- "Andropogon gerardii"
all_niche$species[all_niche$species == "Koeleria cristata"] <- "Koeleria macrantha"
all_niche$species[all_niche$species == "Festuca perennis"] <- "Lolium perenne"
all_niche$species[all_niche$species == "Taraxia ovata"] <- "Camissonia ovata"
all_niche$species[all_niche$species == "Festuca bromoides"] <- "Vulpia bromoides"
all_niche$species[all_niche$species == "Logfia gallica"] <- "Filago gallica"
all_niche$species[all_niche$species == "Pascopyrum smithii"] <- "Elymus smithii"
all_niche$species[all_niche$species == "Hesperostipa comata"] <- "Stipa comata"   
all_niche$species[all_niche$species == "Picradeniopsis oppositifolia"] <- "Bahia oppositifolia"
all_niche$species[all_niche$species == "Heterotheca villosa"] <- "Chrysopsis villosa"
all_niche$species[all_niche$species == "Cryptantha thyrsiflora"] <- "Oreocarya thyrsiflora"
all_niche$species[all_niche$species == "Cryptantha cinerea"] <- "Oreocarya suffruticosa"
all_niche$species[all_niche$species == "Draba reptans"] <- "Tomostima reptans"
all_niche$species[all_niche$species == "Machaeranthera pinnatifida"] <- "Xanthisma spinulosum"
all_niche$species[all_niche$species == "Lesquerella montana"] <- "Physaria montana"
all_niche$species[all_niche$species == "Machaeranthera canescens"] <- "Dieteria canescens"
all_niche$species[all_niche$species == "Salsola tragus"] <- "Kali tragus"
all_niche$species[all_niche$species == "Populus tremouloides"] <- "Populus tremuloides"
all_niche$species[all_niche$species == "Calylophus serrulatus"] <- "Oenothera serrulata"
all_niche$species[all_niche$species == "Coreopsis grandiflora"] <- "Bidens sweetiana"
all_niche$species[all_niche$species == "Dichanthelium oligosanthes"] <- "Panicum oligosanthes"
all_niche$species[all_niche$species == "Gaura parviflora"] <- "Oenothera curtiflora"
all_niche$species[all_niche$species == "Hedyotis nigricans"] <- "Stenaria nigricans"
all_niche$species[all_niche$species == "Psoralidium tenuiflorum"] <- "Pediomelum tenuiflorum"
all_niche$species[all_niche$species == "Solidago ludiviciana"] <- "Solidago ludoviciana"
all_niche$species[all_niche$species == "Stenosiphon linifolius"] <- "Oenothera glaucifolia"
all_niche$species[all_niche$species == "Symphyotrichum ericoides "] <- "Symphyotrichum ericoides"
all_niche$species[all_niche$species == "Amphiachyris dracunculoides"] <- "Gutierrezia dracunculoides"
all_niche$species[all_niche$species == "Diodia teres"] <- "Hexasepalum teres"
all_niche$species[all_niche$species == "Stipa tenuissima"] <- "Nassella tenuissima"
all_niche$species[all_niche$species == "Rubus calycinoides"] <- "Rubus rolfei"
all_niche$species[all_niche$species == "Rubus pentalobus"] <- "Rubus rolfei"
all_niche$species[all_niche$species == "Ruellia hirta"] <- "Strobilanthes glutinosus"
all_niche$species[all_niche$species == "solidago drummondii"] <- "Solidago drummondii"
all_niche$species[all_niche$species == "Symphyotrichum ontarionsis"] <- "Symphyotrichum ontarionis"
all_niche$species[all_niche$species == "Haplopappus ciliatus"] <- "Grindelia ciliata"
all_niche$species[all_niche$species == "Heterotheca latifolia"] <- "Heterotheca subaxillaris"
all_niche$species[all_niche$species == "Muhlenbergia capillaris "] <- "Muhlenbergia capillaris"
all_niche$species[all_niche$species == "Setaria glauca"] <- "Pennisetum glaucum"
all_niche$species[all_niche$species == "Ambrosia trida"] <- "Ambrosia trifida"
all_niche$species[all_niche$species == "Diodia ocymifolia"] <- "Spermacoce ocymifolia"
all_niche$species[all_niche$species == "Grindelia papposa"] <- "Grindelia ciliata"
all_niche$species[all_niche$species == "Tragia cannabina"] <- "Tragia plukenetii"
all_niche$species[all_niche$species == "Trifolium purpureum "] <- "Trifolium purpureum"
all_niche$species[all_niche$species == "Melilotus alba"] <- "Trigonella alba"
all_niche$species[all_niche$species == "Coreopsis tinctoria"] <- "Bidens tinctoria"

# Combine all species niche data with trait data
# rename AccSpeciesName column in traits dataframe
colnames(traits)[1] <- "species"
traits_niche <- left_join(all_niche,traits, by="species")
traits_niche <- traits_niche %>%
  filter(!is.na(TraitID))

# Fixing long trait names
traits_niche$TraitName[traits_niche$TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)"] <- "LDMC"
traits_niche$TraitName[traits_niche$TraitName == "Fine root length per fine root dry mass (specific fine root length, SRL)"] <- "Specific fine root length"
traits_niche$TraitName[traits_niche$TraitName == "Root length per root dry mass (specific root length, SRL)"] <- "Specific root length"
traits_niche$TraitName[traits_niche$TraitName == "Plant biomass and allometry: Fine root length per plant"] <- "Fine root length"
traits_niche$TraitName[traits_niche$TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded"] <- "SLA"
traits_niche$TraitName[traits_niche$TraitName == "Coarse root length per coarse root dry mass (specific coarse root length, SRL)"] <- "Specific coarse root length"
traits_niche$TraitName[traits_niche$TraitName == "Plant biomass and allometry: Coarse root length per plant"] <- "Coarse root length"

# Removing some high outliers and traits with little data
traits_niche_rem <- traits_niche %>%
  filter(TraitName == "Plant height vegetative") %>%
  filter(mean_trait_val < 400)
traits_niche_rem2 <- traits_niche %>%
  filter(TraitName == "Specific fine root length") %>%
  filter(mean_trait_val < 8000)
rest_of_data <- traits_niche %>%
  filter(!(TraitName == "Plant height vegetative" |
             TraitName == "Specific fine root length"))
combined_data <- bind_rows(traits_niche_rem, traits_niche_rem2, rest_of_data)
combined_data <- combined_data %>%
  filter(!(TraitName == "Fine root length" |
             TraitName == "Coarse root length" |
             TraitName == "Specific coarse root length" |
             TraitName == "Dispersal syndrome")) %>%
  mutate(mean_trait_val_log = log(mean_trait_val))

# Make a function to select a given trait name and plot it against temp niche
trait_niche_plot <- function(data,trait){
  plot_data <- data %>%
    filter(TraitID == trait)
  
  trait_name <- plot_data$TraitName[1]
  
  ggplot(plot_data, aes(x = temp_niche, y = mean_trait_val_log)) +
    geom_point() +
    geom_smooth() +
    labs(title = trait_name, x = "Temperature niche (°C)", y = "Log(trait value)") +
    theme_minimal()
}
#trait_niche_plot(traits_niche_rem,3106)

# Initialize a list to store plots
plots_list <- list()

# Check unique TraitID values
unique_trait_ids <- unique(combined_data$TraitID)

# Generate plots and store them in the list
for (trait in unique_trait_ids) {
  # Ensure `trait` is always a single entity
  if (length(trait) == 1) {
    # Assuming trait_niche_plot returns a plot
    p <- trait_niche_plot(combined_data, trait)
    plots_list[[as.character(trait)]] <- p  # Convert to character to avoid unintended issues with numeric indices
  } else {
    message("Warning: Encountered unexpected multi-length trait identifier.")
  }
}

# Arrange all plots in a grid
grid.arrange(grobs = plots_list)



### Interactive plot
combined_data_plotly <- combined_data %>%
  filter(TraitName == "Leaf thickness")
plot_ly(data=combined_data_plotly, x = ~temp_niche, y = ~mean_trait_val_log,
        type="scatter",mode="markers",color=~species)

# Generate a list of all unique trait names
trait_names <- unique(combined_data$TraitName)

# Create a master plot without specific initial data
p <- plot_ly(type = 'scatter', mode = 'markers')

# Add a trace for each (trait, species) combination
for (i in seq_along(trait_names)) {
  trait_data <- combined_data[combined_data$TraitName == trait_names[i], ]
  species_names <- unique(trait_data$species)
  
  for (species in species_names) {
    species_data <- trait_data[trait_data$species == species, ]
    
    p <- add_trace(p,
                   data = species_data,
                   x = ~temp_niche,
                   y = ~mean_trait_val,
                   name = species,
                   text = ~paste("Species:", species),  # Custom hover text
                   hoverinfo = "text",  # Ensure that only custom text is displayed
                   colors = "Dark2",
                   visible = i == 1  # Show only the first trait initially
    )
  }
}

# Add the dropdown to switch visibility between traces
p <- layout(p,
            updatemenus = list(
              list(
                type = 'dropdown',
                active = 0,  # Initial active trait (0-indexed)
                buttons = lapply(seq_along(trait_names), function(i) {
                  list(
                    label = trait_names[i],
                    method = "update",
                    args = list(
                      list(visible = as.logical(rep(seq_along(trait_names) == i, each = length(species_names)))),
                      list(title = paste("Mean Trait Value for", trait_names[i]))  # Update the plot title based on the selected trait
                    )
                  )
                })
              )
            ),
            xaxis = list(title = "Temperature Niche"),
            yaxis = list(title = "Mean Trait Value")
)

p
