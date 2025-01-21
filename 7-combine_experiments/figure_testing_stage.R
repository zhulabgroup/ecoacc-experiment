# TITLE:          Testing different figures; final figs are in EcoAcc-Experiments
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Niche estimate and ecosystem response data for all experiments 
# DATA OUTPUT:    Combined figures
# PROJECT:        EcoAcc
# DATE:           Dec 2024

# Load packages
library(tidyverse)
library(ggpubr)
library(plotly)
library(maps)
library(gridExtra)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TRY_data/"
setwd(path_data)
# Load in data
growth_life <- read.csv(" exp_species_growth_lifecycle.csv")
traits <- read.csv(" exp_species_traits.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/"
setwd(path_data)
# Load in data
CTI_sens_teracon <- read.csv(" CTI_sens_teracon.csv")
CTI_teracon <- read.csv(" CTI_teracon.csv")
tera <- read.csv(" teracon_clean.csv")
NPP_teracon <- read.csv(" eco_response_teracon.csv")
NPP_overall_teracon <- read.csv(" eco_response_overall_teracon.csv")
CTI_CPI_teracon <- read.csv(" CTI_CPI_teracon.csv")
niche_est_tera <- read.csv(" teracon_niche.csv")
gbif_tera <- read.csv(" GBIF_teracon.csv")
# Set path to turbo to get data; data for no bluestem
path_data = "/Volumes/seas-zhukai/proj-ecoacc/TeRaCON/data_for_testing/"
setwd(path_data)
CTI_sens_teracon_noblue <- read.csv(" CTI_sens_teracon_nobluestem.csv")
CTI_teracon_noblue <- read.csv(" CTI_teracon_nobluestem.csv")
CTI_CPI_teracon_noblue <- read.csv(" CTI_CPI_teracon_nobluestem.csv")
NPP_teracon_noblue <- read.csv(" eco_response_teracon_noblue.csv")
NPP_overall_teracon_noblue <- read.csv(" eco_response_overall_teracon_noblue.csv")
CTI_sens_teracon_noblueplots <- read.csv(" CTI_sens_teracon_nobluestemplots.csv")
CTI_teracon_noblueplots <- read.csv(" CTI_teracon_nobluestemplots.csv")
CTI_CPI_teracon_noblueplots <- read.csv(" CTI_CPI_teracon_nobluestemplots.csv")
NPP_teracon_noblueplots <- read.csv(" eco_response_teracon_noblueplots.csv")
NPP_overall_teracon_noblueplots <- read.csv(" eco_response_overall_teracon_noblueplots.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/JRGCE/"
setwd(path_data)
# Load in data
CTI_sens_jrgce <- read.csv(" CTI_sens_jrgce.csv")
CTI_jrgce <- read.csv(" CTI_jrgce.csv")
jrgce <- read.csv(" jrgce_clean.csv")
jrgce <- jrgce %>%
  mutate(temp_treatment = if_else(str_detect(treatment, "T"), "warmed", "ambient"))
NPP_jrgce <- read.csv(" eco_response_jrgce.csv")
NPP_overall_jrgce <- read.csv(" eco_response_overall_jrgce.csv")
CTI_CPI_jrgce <- read.csv(" CTI_CPI_jrgce.csv")
niche_est_jrgce <- read.csv(" jrgce_niche.csv")
gbif_jrgce <- read.csv(" GBIF_jrgce.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/PHACE/"
setwd(path_data)
# Load in data
CTI_sens_phace <- read.csv(" CTI_sens_phace.csv")
CTI_phace <- read.csv(" CTI_phace.csv")
phace <- read.csv(" phace_clean.csv")
NPP_phace <- read.csv(" eco_response_phace.csv")
NPP_overall_phace <- read.csv(" eco_response_overall_phace.csv")
CTI_CPI_phace <- read.csv(" CTI_CPI_phace.csv")
niche_est_phace <- read.csv(" phace_niche.csv")
gbif_phace <- read.csv(" GBIF_phace.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/B4Warmed/"
setwd(path_data)
# Load in data
CTI_sens_b4 <- read.csv(" CTI_sens_b4warmed.csv")
CTI_b4 <- read.csv(" CTI_b4warmed.csv")
b4 <- read.csv(" b4warmed_clean.csv")
NPP_b4 <- read.csv(" eco_response_b4warmed.csv")
NPP_overall_b4 <- read.csv(" eco_response_overall_b4warmed.csv")
CTI_CPI_b4 <- read.csv(" CTI_CPI_b4warmed.csv")
niche_est_b4 <- read.csv(" b4warmed_niche.csv")

# Set path to data
path_data = "/Volumes/seas-zhukai/proj-ecoacc/OK/"
setwd(path_data)
# Load in data
CTI_sens_ok <- read.csv(" CTI_sens_ok.csv")
CTI_ok <- read.csv(" CTI_ok.csv")
ok <- read.csv(" ok_clean.csv")
NPP_ok <- read.csv(" eco_response_ok.csv")
NPP_overall_ok <- read.csv(" eco_response_overall_ok.csv")
CTI_CPI_ok <- read.csv(" CTI_CPI_ok.csv")
niche_est_ok <- read.csv(" ok_niche.csv")





##### Fig: Checking distribution of occurrences for each species
world <- map_data("world")
distb_occ <- function(data,spp){
  
  spp_data <- data %>%
    filter(species == spp)
  
  ggplot() +
    geom_map(
      data = world, map = world,
      aes(long, lat, map_id = region),
      color = "lightgrey", fill = "darkgrey", size = 0.1
    ) +
    geom_point(
      data = spp_data,
      aes(decimalLongitude, decimalLatitude),
      alpha = 0.7,
      color = "red",
      size=2
    ) +
    theme_classic() +
    labs(x = "Longitude",y = "Latitude") + 
    theme(axis.title.x = element_text(size=15),
          axis.title.y = element_text(size=15),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14))
}
# Plot occurrence maps for abundant species in each experiment
distb_occ(gbif_tera,"Andropogon gerardi")
distb_occ(gbif_jrgce,"Festuca bromoides")
distb_occ(gbif_phace,"Elymus smithii")



##### Fig: traits vs. temp niche
# make a dataframe containing all species and their temp niches
tera_niche_filt <- niche_est_tera %>%
  select(species,temp_niche) %>%
  distinct()
jrgce_niche_filt <- niche_est_jrgce %>%
  select(species,temp_niche) %>%
  distinct()
phace_niche_filt <- niche_est_phace %>%
  select(species,temp_niche) %>%
  distinct()
b4_cfc_niche_filt <- niche_est_b4 %>%
  filter(site == "CFC") %>%
  select(species,temp_niche) %>%
  distinct()
b4_hwrc_niche_filt <- niche_est_b4 %>%
  filter(site == "HWRC") %>%
  select(species,temp_niche) %>%
  distinct()
ok_niche_filt <- niche_est_ok %>%
  select(species,temp_niche) %>%
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
             TraitName == "Dispersal syndrome"))

# Make a function to select a given trait name and plot it against temp niche
trait_niche_plot <- function(data,trait){
  plot_data <- data %>%
    filter(TraitID == trait)
  
  trait_name <- plot_data$TraitName[1]
  
  ggplot(plot_data, aes(x = temp_niche, y = mean_trait_val)) +
    geom_point() +
    geom_smooth() +
    labs(title = trait_name, x = "Temperature niche (°C)", y = "Mean trait value per species") +
    theme_minimal()
}
trait_niche_plot(traits_niche_rem,3106)

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
# You can specify ncol or nrow to arrange the plots
grid.arrange(grobs = plots_list) # Adjust ncol or nrow based on preference

# Interactive plot
plot_ly(data=traits_niche_rem, x = ~temp_niche, y = ~mean_trait_val,
        type="scatter",color=~species)





##### Fig: Mean CTI over time in warmed and ambient #####
# note: could also change y = CTI to a different metric (CTI_sd, CTI_skew, etc.)
CTI_teracon_plot <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth() +
  labs(title="TeRaCON", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red"))

CTI_jrgce <- CTI_jrgce %>%
  filter(!(year == 1998))
CTI_jrgce_plot <- ggplot(CTI_jrgce, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth() +
  labs(title = "JRGCE", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red"))

CTI_phace_plot <- ggplot(CTI_phace, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth() +
  labs(title = "PHACE", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red"))

CTI_ok_plot <- ggplot(CTI_ok, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth() +
  labs(title = "Oklahoma", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("Ambient","Warmed"),
                     values = c("blue","red"))

CTI_b4_cfc <- CTI_b4 %>%
  filter(site == "CFC")
CTI_b4_hwrc <- CTI_b4 %>%
  filter(site == "HWRC")
CTI_b4_cfc_plot <- ggplot(CTI_b4_cfc, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) + 
  geom_smooth() +
  labs(title = "B4Warmed CFC", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("1.7°C Warmed","3.4°C Warmed","Ambient"),
                     values = c("orange","red","blue"))
CTI_b4_hwrc_plot <- ggplot(CTI_b4_hwrc, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) + 
  geom_smooth() +
  labs(title = "B4Warmed HWRC", x = "Year") +
  theme_minimal() +
  scale_color_manual(name = "Treatment",
                     labels = c("1.7°C Warmed","3.4°C Warmed","Ambient"),
                     values = c("orange","red","blue"))






##### Fig: CTI as a function of temperature over time
# Using if...else to determine the value based on temp_treatment
CTI_teracon$ambient_temp <- NA
CTI_teracon <- CTI_teracon %>%
  mutate(ambient_temp = case_when(temp_treatment == "HTelv" ~ mean_C_temp_warmed,
                                  temp_treatment == "HTamb" ~ mean_C_temp_summer))

CTI_teracon_plot <- ggplot(CTI_teracon, aes(x = mean_C_temp_summer, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
              #position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="TeRaCON") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))

CTI_jrgce_plot <- ggplot(CTI_jrgce, aes(x = mean_C_temp_summer, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1) +
              #position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               #width = 0.4,
               #position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="TeRaCON") +
  theme_minimal() +
  scale_color_manual(values = c("ambient" = "blue", "warmed" = "red"))






##### Fig:  interactive plot for species niche estimates
plot_ly(data=niche_est_phace_avg, x = ~temp_niche, y = ~precip_niche,
        type="scatter",color=~species)






##### Fig: Bubble plots for species abundance each year in each treatment
# Merging niche estimate data with abundance data
niche_est_phace_avg <- niche_est_phace[,c(1,6,7)]
niche_est_phace_avg <- niche_est_phace_avg %>%
  distinct()
phace_merge <- merge(phace_test,niche_est_phace_avg, by="species")

niche_est_jrgce_avg <- niche_est_jrgce %>%
  dplyr::select(-c(tmp,ppt)) %>%
  distinct()
jrgce_merge <- merge(jrgce_test,niche_est_jrgce_avg, by="species")

niche_est_tera_avg <- niche_est_tera %>%
  dplyr::select(-c(mean_annual_temp, mean_annual_precip, latitude, longitude)) %>%
  distinct()
tera_merge <- merge(tera_test,niche_est_tera_avg, by="species")

phace_warm <- phace_merge %>%
  filter(temp_treatment == "warmed")
phace_amb <- phace_merge %>%
  filter(temp_treatment == "ambient")
abun_warm <- ggplot(phace_warm, aes(x = temp_niche, y = precip_niche)) + 
  geom_point(aes(size = avg_abun), alpha = 0.75, shape = 21) + 
  facet_wrap(.~year) +
  theme_bw()
abun_amb <- ggplot(phace_amb, aes(x = temp_niche, y = precip_niche)) + 
  geom_point(aes(size = avg_abun), alpha = 0.75, shape = 21) + 
  facet_wrap(.~year) +
  theme_bw()
# Combine figures into one multi-panel plot
ggarrange(abun_warm,abun_amb,
          ncol = 2, nrow=1, common.legend = T, legend = "right")





##### Fig: teracon --> w/ and w/o big bluestem
CTI_blue_tera <- ggplot(CTI_teracon, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/ big bluestem") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_noblue_tera <- ggplot(CTI_teracon_noblue, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/o big bluestem") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
CTI_noblueplots_tera <- ggplot(CTI_teracon_noblueplots, aes(x = year, y = CTI, color = temp_treatment, group=temp_treatment)) +
  geom_jitter(alpha = 0.1,
              position = position_jitterdodge(dodge.width = 0.7)) +  # Add jittered points
  stat_summary(fun = mean,
               fun.min = mean,
               fun.max = mean,
               geom = "line",
               # width = 0.4,
               position = position_dodge(width = 0.7),
               aes(color = temp_treatment, group = temp_treatment)) +
  labs(title="w/o big bluestem plots") +
  theme_minimal() +
  scale_color_manual(values = c("HTamb" = "blue", "HTelv" = "red"))
arrow_blue <- ggplot(CTI_CPI_teracon) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/ big bluestem") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_noblue <- ggplot(CTI_CPI_teracon_noblue) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/o big bluestem") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
arrow_noblueplots <- ggplot(CTI_CPI_teracon_noblueplots) +
  geom_segment(aes(x = CTI_HTamb, y = CPI_HTamb, 
                   xend = CTI_HTelv, yend = CPI_HTelv,
                   color = year),
               arrow = arrow(length = unit(0.1, "inches"))) +
  geom_point(aes(x = CTI_HTamb, y = CPI_HTamb), color = "black") +
  geom_point(aes(x = CTI_HTelv, y = CPI_HTelv), color = "red") +
  labs(x = "CTI", y = "CPI", title = "w/o big bluestem plots") +
  scale_color_viridis_c(option = "magma") +
  theme_minimal()
CTI_blue_smooth <- ggplot(CTI_sens_teracon, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/ big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
CTI_noblue_smooth <- ggplot(CTI_sens_teracon_noblue, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/o big bluestem") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
CTI_noblueplots_smooth <- ggplot(CTI_sens_teracon_noblueplots, aes(x = year, y = sensitivity)) +
  geom_smooth() +
  labs(x = "Year", y = "CTI (Warmed - Ambient)",title = "w/o big bluestem plots") +
  scale_x_continuous(breaks = seq(2012, 2023, by = 2)) +
  ylim(-0.3,0.35) +
  theme_bw()
# Combine figures into one multi-panel plot
ggarrange(CTI_blue_tera, arrow_blue, CTI_blue_smooth,
          CTI_noblue_tera, arrow_noblue, CTI_noblue_smooth,
          CTI_noblueplots_tera, arrow_noblueplots, CTI_noblueplots_smooth,
          ncol = 3, nrow=3)





#### Fig: teracon --> CTI vs. biomass w/ and w/o big bluestem
CTI_yearly_avg_tera <- CTI_teracon %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon <- left_join(CTI_yearly_avg_tera, NPP_overall_teracon, by=c("year","temp_treatment"))
tera_scatter <- ggscatter(NPP_CTI_teracon, x = "mean_ab_bio", y = "mean_CTI", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomass", ylab = "CTI",title="All data")

CTI_yearly_avg_tera_noblue <- CTI_teracon_noblue %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon_noblue <- left_join(CTI_yearly_avg_tera_noblue, NPP_overall_teracon_noblue, by=c("year","temp_treatment"))
tera_scatter_noblue <- ggscatter(NPP_CTI_teracon_noblue, x = "mean_ab_bio", y = "mean_CTI", 
                          add = "reg.line", conf.int = TRUE, 
                          cor.coef = TRUE, cor.method = "pearson",
                          xlab = "Biomass", ylab = "CTI", title="Bluestem removed")

CTI_yearly_avg_tera_noblueplots <- CTI_teracon_noblueplots %>%
  group_by(year,temp_treatment) %>%
  summarize(mean_CTI = mean(CTI))
NPP_CTI_teracon_noblueplots <- left_join(CTI_yearly_avg_tera_noblueplots, NPP_overall_teracon_noblueplots, by=c("year","temp_treatment"))
tera_scatter_noblueplots <- ggscatter(NPP_CTI_teracon_noblueplots, x = "mean_ab_bio", y = "mean_CTI", 
                                 add = "reg.line", conf.int = TRUE, 
                                 cor.coef = TRUE, cor.method = "pearson",
                                 xlab = "Biomass", ylab = "CTI", title="Bluestem plots removed")

# Combine figures into one multi-panel plot
ggarrange(tera_scatter,tera_scatter_noblue,tera_scatter_noblueplots,
          ncol = 3)





##### Fig: species sorted by life cycle
jrgce_sig_spp <- left_join(jrgce_sig_spp, growth_life, by="species")
phace_sig_spp <- left_join(phace_sig_spp, growth_life, by="species")
tera_sig_spp <- left_join(tera_sig_spp, growth_life, by="species")
b4_sig_spp_cfc <- left_join(b4_sig_spp_cfc, growth_life, by="species")
b4_sig_spp_hwrc <- left_join(b4_sig_spp_hwrc, growth_life, by="species")

ggplot(jrgce_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "JRGCE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(phace_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(tera_sig_spp, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(b4_sig_spp_cfc, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")

ggplot(b4_sig_spp_hwrc, aes(x = mean_CTI, y = avg_abun)) +
  geom_smooth(method="lm") +
  geom_point() +
  theme_minimal() +
  labs(title = "PHACE: Mean CTI vs Abundance for Different Species",
       x = "Mean CTI",
       y = "Abundance") +
  #scale_color_manual(values = c("ambient" = "blue", "warmed" = "red")) +
  facet_wrap(~ life_cycle, scales = "free_y")



##### Fig: % perennial species in each experiment
jrgce_lifecycles <- left_join(jrgce, growth_life, by="species")
phace_lifecycles <- left_join(phace, growth_life, by="species")
tera_lifecycles <- left_join(tera, growth_life, by="species")
b4_cfc_lifecycles <- left_join(b4_cfc, growth_life, by="species")
b4_hwrc_lifecycles <- left_join(b4_hwrc, growth_life, by="species")

# Selecting species, life cycle, and growth form column form each data frame
jrgce_lifecycles <- jrgce_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
phace_lifecycles <- phace_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
tera_lifecycles <- tera_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
b4_cfc_lifecycles <- b4_cfc_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()
b4_hwrc_lifecycles <- b4_hwrc_lifecycles %>%
  dplyr::select(species, life_cycle, growth_form) %>%
  distinct()

# For each data frame, calculate the % of species for life cycle
jrgce_lifecycles_perc <- jrgce_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
phace_lifecycles_perc <- phace_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
tera_lifecycles_perc <- tera_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_cfc_lifecycles_perc <- b4_cfc_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_hwrc_lifecycles_perc <- b4_hwrc_lifecycles %>%
  filter(!is.na(life_cycle)) %>%
  group_by(life_cycle) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))

# For each data frame, calculate the % of species for growth form
jrgce_growth_perc <- jrgce_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
phace_growth_perc <- phace_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
tera_growth_perc <- tera_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_cfc_growth_perc <- b4_cfc_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))
b4_hwrc_growth_perc <- b4_hwrc_lifecycles %>%
  filter(!is.na(growth_form)) %>%
  group_by(growth_form) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n))



