# TITLE:          Yu et al. Nature paper data cleaning
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     RData file from paper imported
# DATA OUTPUT:    Cleaned experiment data
# PROJECT:        EcoAcc
# DATE:           March 2025

# Load packages
library(tidyverse)

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
setwd(path_data)

# Read in data
load("all_data.RData")



### Cleaning data
# Wide to long
yu_abun_long <- spe_cover_US %>%
  pivot_longer(cols = -c(site,year,block,subplot,species,country),
               names_to = "treatment", values_to = "percent_cover")

### Calculating relative abundance
yu_abun <- yu_abun_long %>%
  group_by(site,year,block,subplot,treatment) %>%
  mutate(total_cover = sum(percent_cover)) %>%
  mutate(rel_abun = percent_cover/total_cover)

### Fixing species names (adding a space btwn genus and spp)
yu_abun$species[yu_abun$species == "Vulpiaoctoflora"] <- "Vulpia octoflora"
yu_abun$species[yu_abun$species == "Violarafinesquii"] <- "Viola rafinesquii"
yu_abun$species[yu_abun$species == "Viciaamericana"] <- "Vicia americana"
yu_abun$species[yu_abun$species == "Vernoniabaldwinii"] <- "Vernonia baldwinii"
yu_abun$species[yu_abun$species == "Verbenastricta"] <- "Verbena stricta"
yu_abun$species[yu_abun$species == "Ulmusamericana"] <- "Ulmus americana"
yu_abun$species[yu_abun$species == "Triodanissp."] <- "Triodanis"
yu_abun$species[yu_abun$species == "Triodanisperfoliata"] <- "Triodanis perfoliata"
yu_abun$species[yu_abun$species == "Triodanisleptocarpa"] <- "Triodanis leptocarpa"
yu_abun$species[yu_abun$species == "Tragopogondubius"] <- "Tragopogon dubius"
yu_abun$species[yu_abun$species == "Tragiaramosa"] <- "Tragia ramosa"
yu_abun$species[yu_abun$species == "Tradescantiasp."] <- "Tradescantia"
yu_abun$species[yu_abun$species == "Tradescantiaoccidentalis"] <- "Tradescantia occidentalis"
yu_abun$species[yu_abun$species == "Tradescantiabracteata"] <- "Tradescantia bracteata"
yu_abun$species[yu_abun$species == "Townsendiagrandiflora"] <- "Townsendia grandiflora"
yu_abun$species[yu_abun$species == "Thelespermamegapotamicum"] <- "Thelesperma megapotamicum"
yu_abun$species[yu_abun$species == "Thelespermafilifolium"] <- "Thelesperma filifolium"
yu_abun$species[yu_abun$species == "Teucriumcanadense"] <- "Teucrium canadense"
yu_abun$species[yu_abun$species == "Tetraneurisacaulis"] <- "Tetraneuris acaulis"
yu_abun$species[yu_abun$species == "Taraxacumofficinale"] <- "Taraxacum officinale"
yu_abun$species[yu_abun$species == "Talinumparviflorum"] <- "Talinum parviflorum"
yu_abun$species[yu_abun$species == "Stipacomata"] <- "Stipa comata"
yu_abun$species[yu_abun$species == "Stephanomeriaruncinata"] <- "Stephanomeria runcinata"
yu_abun$species[yu_abun$species == "Stenarianigricans"] <- "Stenaria nigricans"
yu_abun$species[yu_abun$species == "Sporobolusheterolepis"] <- "Sporobolus heterolepis"
yu_abun$species[yu_abun$species == "Sporoboluscryptandrus"] <- "Sporobolus cryptandrus"
yu_abun$species[yu_abun$species == "Sporobolusasper"] <- "Sporobolus asper"
yu_abun$species[yu_abun$species == "Sphaeralceacoccinea"] <- "Sphaeralcea coccinea"
yu_abun$species[yu_abun$species == "Sorghastrumnutans"] <- "Sorghastrum nutans"
yu_abun$species[yu_abun$species == "Sonchusasper"] <- "Sonchus asper"
yu_abun$species[yu_abun$species == "Solidagomissouriensis"] <- "Solidago missouriensis"
yu_abun$species[yu_abun$species == "Solidagocanadensis"] <- "Solidago canadensis"
yu_abun$species[yu_abun$species == "Solanumtriflorum"] <- "Solanum triflorum"
yu_abun$species[yu_abun$species == "Sisyrinchiumcampestre"] <- "Sisyrinchium campestre"
yu_abun$species[yu_abun$species == "Sisymbriumaltissimum"] <- "Sisymbrium altissimum"
yu_abun$species[yu_abun$species == "Sileneantirrhina"] <- "Silene antirrhina"
yu_abun$species[yu_abun$species == "Senecioplattensis"] <- "Senecio plattensis"
yu_abun$species[yu_abun$species == "Scorzoneralaciniata"] <- "Scorzonera laciniata"
yu_abun$species[yu_abun$species == "Schrankianuttallii"] <- "Schrankia nuttallii"
yu_abun$species[yu_abun$species == "Schizachyriumscoparium"] <- "Schizachyrium scoparium"
yu_abun$species[yu_abun$species == "Schedonnarduspaniculatus"] <- "Schedonnardus paniculatus"
yu_abun$species[yu_abun$species == "Salviaazurea"] <- "Salvia azurea"
yu_abun$species[yu_abun$species == "Salsolatragus"] <- "Salsola tragus"
yu_abun$species[yu_abun$species == "Ruelliahumilis"] <- "Ruellia humilis"
yu_abun$species[yu_abun$species == "Rhusglabra"] <- "Rhus glabra"
yu_abun$species[yu_abun$species == "Ratibidacolumnifera"] <- "Ratibida columnifera"
yu_abun$species[yu_abun$species == "Psoralidiumtenuiflorum"] <- "Psoralidium tenuiflorum"
yu_abun$species[yu_abun$species == "Psoraleatenuiflora"] <- "Psoralea tenuiflora"
yu_abun$species[yu_abun$species == "Psoraleaesculenta"] <- "Psoralea esculenta"
yu_abun$species[yu_abun$species == "Psoraleacuspidata"] <- "Psoralea cuspidata"
yu_abun$species[yu_abun$species == "Potentillapensylvanica"] <- "Potentilla pensylvanica"
yu_abun$species[yu_abun$species == "Polygonumsp."] <- "Polygonum"
yu_abun$species[yu_abun$species == "Polygalaverticillata"] <- "Polygala verticillata"
yu_abun$species[yu_abun$species == "Polygalasp."] <- "Polygala"
yu_abun$species[yu_abun$species == "Poapratensis"] <- "Poa pratensis"
yu_abun$species[yu_abun$species == "Plantagorhodosperma"] <- "Plantago rhodosperma"
yu_abun$species[yu_abun$species == "Plantagopatagonica"] <- "Plantago patagonica"
yu_abun$species[yu_abun$species == "Picradeniopsisoppositifolia"] <- "Picradeniopsis oppositifolia"
yu_abun$species[yu_abun$species == "Physalispumila"] <- "Physalis pumila"
yu_abun$species[yu_abun$species == "Phloxhoodii"] <- "Phlox hoodii"
yu_abun$species[yu_abun$species == "Penstemonangustifolius"] <- "Penstemon angustifolius"
yu_abun$species[yu_abun$species == "Penstemonalbidus"] <- "Penstemon albidus"
yu_abun$species[yu_abun$species == "Pascopyrumsmithii"] <- "Pascopyrum smithii"
yu_abun$species[yu_abun$species == "Paronychiasessiliflora"] <- "Paronychia sessiliflora"
yu_abun$species[yu_abun$species == "Panicumvirgatum"] <- "Panicum virgatum"
yu_abun$species[yu_abun$species == "Panicumsp."] <- "Panicum"
yu_abun$species[yu_abun$species == "Oxytropissericea"] <- "Oxytropis sericea"
yu_abun$species[yu_abun$species == "Oxytropislambertii"] <- "Oxytropis lambertii"
yu_abun$species[yu_abun$species == "Oxalisviolacea"] <- "Oxalis violacea"
yu_abun$species[yu_abun$species == "Oxalisstricta"] <- "Oxalis stricta"
yu_abun$species[yu_abun$species == "Orobanchesp."] <- "Orobanche"
yu_abun$species[yu_abun$species == "Opuntiapolyacantha"] <- "Opuntia polyacantha"
yu_abun$species[yu_abun$species == "Oenotherasp."] <- "Oenothera"
yu_abun$species[yu_abun$species == "Oenotheracoronopifolia"] <- "Oenothera coronopifolia"
yu_abun$species[yu_abun$species == "Oenotheraalbicaulis"] <- "Oenothera albicaulis"
yu_abun$species[yu_abun$species == "Nothocalaiscuspidata"] <- "Nothocalais cuspidata"
yu_abun$species[yu_abun$species == "Nassellaviridula"] <- "Nassella viridula"
yu_abun$species[yu_abun$species == "Musineondivaricatum"] <- "Musineon divaricatum"
yu_abun$species[yu_abun$species == "Muhlenbergiatorreyi"] <- "Muhlenbergia torreyi"
yu_abun$species[yu_abun$species == "Mirabilislinearis"] <- "Mirabilis linearis"
yu_abun$species[yu_abun$species == "Mertensialanceolata"] <- "Mertensia lanceolata"
yu_abun$species[yu_abun$species == "Melilotusofficinalis"] <- "Melilotus officinalis"
yu_abun$species[yu_abun$species == "Medicagolupulina"] <- "Medicago lupulina"
yu_abun$species[yu_abun$species == "Machaerantheratanacetifolia"] <- "Machaeranthera tanacetifolia"
yu_abun$species[yu_abun$species == "Machaerantherapinnatifida"] <- "Machaeranthera pinnatifida"
yu_abun$species[yu_abun$species == "Lygodesmiajuncea"] <- "Lygodesmia juncea"
yu_abun$species[yu_abun$species == "Lithospermumincisum"] <- "Lithospermum incisum"
yu_abun$species[yu_abun$species == "Linumsulcatum"] <- "Linum sulcatum"
yu_abun$species[yu_abun$species == "Linumrigidum"] <- "Linum rigidum"
yu_abun$species[yu_abun$species == "Linariadalmatica"] <- "Linaria dalmatica"
yu_abun$species[yu_abun$species == "Liatrispunctata"] <- "Liatris punctata"
yu_abun$species[yu_abun$species == "Leucocrinummontanum"] <- "Leucocrinum montanum"
yu_abun$species[yu_abun$species == "Lesquerellamontana"] <- "Lesquerella montana"
yu_abun$species[yu_abun$species == "Lespedezaviolacea"] <- "Lespedeza violacea"
yu_abun$species[yu_abun$species == "Lespedezacapitata"] <- "Lespedeza capitata"
yu_abun$species[yu_abun$species == "Lepidiumdensiflorum"] <- "Lepidium densiflorum"
yu_abun$species[yu_abun$species == "Lappularedowskii"] <- "Lappula redowskii"
yu_abun$species[yu_abun$species == "Lactucaserriola"] <- "Lactuca serriola"
yu_abun$species[yu_abun$species == "Kuhniaeupatorioides"] <- "Kuhnia eupatorioides"
yu_abun$species[yu_abun$species == "Koeleriapyramidata"] <- "Koeleria pyramidata"
yu_abun$species[yu_abun$species == "Kochiascoparia"] <- "Kochia scoparia"
yu_abun$species[yu_abun$species == "Juncussp."] <- "Juncus"
yu_abun$species[yu_abun$species == "Juncusinterior"] <- "Juncus interior"
yu_abun$species[yu_abun$species == "Ipomopsislaxiflora"] <- "Ipomopsis laxiflora"
yu_abun$species[yu_abun$species == "Hordeumpusillum"] <- "Hordeum pusillum"
yu_abun$species[yu_abun$species == "Heterothecavillosa"] <- "Heterotheca villosa"
yu_abun$species[yu_abun$species == "Helianthussp."] <- "Helianthus"
yu_abun$species[yu_abun$species == "Helianthuspumilus"] <- "Helianthus pumilus"
yu_abun$species[yu_abun$species == "Helianthusannuus"] <- "Helianthus annuus"
yu_abun$species[yu_abun$species == "Hedeomahispida"] <- "Hedeoma hispida"
yu_abun$species[yu_abun$species == "Gutierreziasarothrae"] <- "Gutierrezia sarothrae"
yu_abun$species[yu_abun$species == "Grindeliasquarrosa"] <- "Grindelia squarrosa"
yu_abun$species[yu_abun$species == "Gauraparviflora"] <- "Gaura parviflora"
yu_abun$species[yu_abun$species == "Gauracoccinea"] <- "Gaura coccinea"
yu_abun$species[yu_abun$species == "Evolvulusnuttalliana"] <- "Evolvulus nuttalliana"
yu_abun$species[yu_abun$species == "Euphorbiaspathulata"] <- "Euphorbia spathulata"
yu_abun$species[yu_abun$species == "Euphorbiaserpens"] <- "Euphorbia serpens"
yu_abun$species[yu_abun$species == "Euphorbianutans"] <- "Euphorbia nutans"
yu_abun$species[yu_abun$species == "Euphorbiamarginata"] <- "Euphorbia marginata"
yu_abun$species[yu_abun$species == "Euphorbiaglyptosperma"] <- "Euphorbia glyptosperma"
yu_abun$species[yu_abun$species == "Euphorbiadavidii"] <- "Euphorbia davidii"
yu_abun$species[yu_abun$species == "Escobariavivipara"] <- "Escobaria vivipara"
yu_abun$species[yu_abun$species == "Erysimumcapitatum"] <- "Erysimum capitatum"
yu_abun$species[yu_abun$species == "Erysimumasperum"] <- "Erysimum asperum"
yu_abun$species[yu_abun$species == "Eriogonumhelichrysoides"] <- "Eriogonum helichrysoides"
yu_abun$species[yu_abun$species == "Eriogonumeffusum"] <- "Eriogonum effusum"
yu_abun$species[yu_abun$species == "Eriogonumalatum"] <- "Eriogonum alatum"
yu_abun$species[yu_abun$species == "Erigeronstrigosus"] <- "Erigeron strigosus"
yu_abun$species[yu_abun$species == "Erigeronpumilus"] <- "Erigeron pumilus"
yu_abun$species[yu_abun$species == "Ericamerianauseosa"] <- "Ericameria nauseosa"
yu_abun$species[yu_abun$species == "Eragrostisspectabilis"] <- "Eragrostis spectabilis"
yu_abun$species[yu_abun$species == "Elymuselymoides"] <- "Elymus elymoides"
yu_abun$species[yu_abun$species == "Eleocharissp."] <- "Eleocharis"
yu_abun$species[yu_abun$species == "Echinocereusviridiflorus"] <- "Echinocereus viridiflorus"
yu_abun$species[yu_abun$species == "Drabareptans"] <- "Draba reptans"
yu_abun$species[yu_abun$species == "Drabanemorosa"] <- "Draba nemorosa"
yu_abun$species[yu_abun$species == "Dieteriacanescens"] <- "Dieteria canescens"
yu_abun$species[yu_abun$species == "Dichantheliumoligosanthes"] <- "Dichanthelium oligosanthes"
yu_abun$species[yu_abun$species == "Dichantheliumacuminatum"] <- "Dichanthelium acuminatum"
yu_abun$species[yu_abun$species == "Descurainiapinnata"] <- "Descurainia pinnata"
yu_abun$species[yu_abun$species == "Delphiniumgeyeri"] <- "Delphinium geyeri"
yu_abun$species[yu_abun$species == "Daleapurpurea"] <- "Dalea purpurea"
yu_abun$species[yu_abun$species == "Daleacandida"] <- "Dalea candida"
yu_abun$species[yu_abun$species == "Cyperuslupulinus"] <- "Cyperus lupulinus"
yu_abun$species[yu_abun$species == "Cyperusacuminatus"] <- "Cyperus acuminatus"
yu_abun$species[yu_abun$species == "Cymopterusacaulis"] <- "Cymopterus acaulis"
yu_abun$species[yu_abun$species == "Cryptanthaminima"] <- "Cryptantha minima"
yu_abun$species[yu_abun$species == "Crotontexensis"] <- "Croton texensis"
yu_abun$species[yu_abun$species == "Crotonsp."] <- "Croton"
yu_abun$species[yu_abun$species == "Conyzacanadensis"] <- "Conyza canadensis"
yu_abun$species[yu_abun$species == "Convolvulusarvensis"] <- "Convolvulus arvensis"
yu_abun$species[yu_abun$species == "Comandraumbellata"] <- "Comandra umbellata"
yu_abun$species[yu_abun$species == "Collomialinearis"] <- "Collomia linearis"
yu_abun$species[yu_abun$species == "Cirsiumundulatum"] <- "Cirsium undulatum"
yu_abun$species[yu_abun$species == "Cirsiumsp."] <- "Cirsium"
yu_abun$species[yu_abun$species == "Cirsiumochrocentrum"] <- "Cirsium ochrocentrum"
yu_abun$species[yu_abun$species == "Chlorissp."] <- "Chloris"
yu_abun$species[yu_abun$species == "Chenopodiumleptophyllum"] <- "Chenopodium leptophyllum"
yu_abun$species[yu_abun$species == "Chenopodiumincanum"] <- "Chenopodium incanum"
yu_abun$species[yu_abun$species == "Chenopodiumdesiccatum"] <- "Chenopodium desiccatum"
yu_abun$species[yu_abun$species == "Chenopodiumalbum"] <- "Chenopodium album"
yu_abun$species[yu_abun$species == "Chaetopappaericoides"] <- "Chaetopappa ericoides"
yu_abun$species[yu_abun$species == "Castillejapurpurea"] <- "Castille japurpurea"
yu_abun$species[yu_abun$species == "Carexmeadii"] <- "Carex meadii"
yu_abun$species[yu_abun$species == "Carexheliophila"] <- "Carex heliophila"
yu_abun$species[yu_abun$species == "Carexgravida"] <- "Carex gravida"
yu_abun$species[yu_abun$species == "Carexfilifolia"] <- "Carex filifolia"
yu_abun$species[yu_abun$species == "Carexeleocharis"] <- "Carex eleocharis"
yu_abun$species[yu_abun$species == "Carexbrevior"] <- "Carex brevior"
yu_abun$species[yu_abun$species == "Carexblanda"] <- "Carex blanda"
yu_abun$species[yu_abun$species == "Calylophusserrulatus"] <- "Calylophus serrulatus"
yu_abun$species[yu_abun$species == "Buchloedactyloides"] <- "Buchloe dactyloides"
yu_abun$species[yu_abun$species == "Bromustectorum"] <- "Bromus tectorum"
yu_abun$species[yu_abun$species == "Bromusjaponicus"] <- "Bromus japonicus"
yu_abun$species[yu_abun$species == "Boutelouahirsuta"] <- "Bouteloua hirsuta"
yu_abun$species[yu_abun$species == "Boutelouagracilis"] <- "Bouteloua gracilis"
yu_abun$species[yu_abun$species == "Boutelouacurtipendula"] <- "Bouteloua curtipendula"
yu_abun$species[yu_abun$species == "Baptisiabracteata"] <- "Baptisia bracteata"
yu_abun$species[yu_abun$species == "Astragalussp."] <- "Astragalus"
yu_abun$species[yu_abun$species == "Astragalusshortianus"] <- "Astragalus shortianus"
yu_abun$species[yu_abun$species == "Astragalusmollissimus"] <- "Astragalus mollissimus"
yu_abun$species[yu_abun$species == "Astragalusmissouriensis"] <- "Astragalus missouriensis"
yu_abun$species[yu_abun$species == "Astragaluslotiflorus"] <- "Astragalus lotiflorus"
yu_abun$species[yu_abun$species == "Astragaluslaxmannii"] <- "Astragalus laxmannii"
yu_abun$species[yu_abun$species == "Astragalusgracilis"] <- "Astragalus gracilis"
yu_abun$species[yu_abun$species == "Astragalusdrummondii"] <- "Astragalus drummondii"
yu_abun$species[yu_abun$species == "Astragaluscrassicarpus"] <- "Astragalus crassicarpus"
yu_abun$species[yu_abun$species == "Asteroblongifolius"] <- "Aster oblongifolius"
yu_abun$species[yu_abun$species == "Asterericoides"] <- "Aster ericoides"
yu_abun$species[yu_abun$species == "Asclepiasviridis"] <- "Asclepias viridis"
yu_abun$species[yu_abun$species == "Asclepiasviridiflora"] <- "Asclepias viridiflora"
yu_abun$species[yu_abun$species == "Asclepiasverticillata"] <- "Asclepias verticillata"
yu_abun$species[yu_abun$species == "Asclepiastuberosa"] <- "Asclepias tuberosa"
yu_abun$species[yu_abun$species == "Asclepiassullivantii"] <- "Asclepias sullivantii"
yu_abun$species[yu_abun$species == "Asclepiasstenophylla"] <- "Asclepias stenophylla"
yu_abun$species[yu_abun$species == "Asclepiassp."] <- "Asclepias"
yu_abun$species[yu_abun$species == "Asclepiasasperula"] <- "Asclepias asperula"
yu_abun$species[yu_abun$species == "Artemisialudoviciana"] <- "Artemisia ludoviciana"
yu_abun$species[yu_abun$species == "Artemisiafrigida"] <- "Artemisia frigida"
yu_abun$species[yu_abun$species == "Artemisiadracunculus"] <- "Artemisia dracunculus"
yu_abun$species[yu_abun$species == "Aristidapurpurea"] <- "Aristida purpurea"
yu_abun$species[yu_abun$species == "Antennariarosea"] <- "Antennaria rosea"
yu_abun$species[yu_abun$species == "Antennariaparvifolia"] <- "Antennaria parvifolia"
yu_abun$species[yu_abun$species == "Antennarianeglecta"] <- "Antennaria neglecta"
yu_abun$species[yu_abun$species == "Androsaceoccidentalis"] <- "Androsace occidentalis"
yu_abun$species[yu_abun$species == "Andropogonternarius"] <- "Andropogon ternarius"
yu_abun$species[yu_abun$species == "Andropogongerardii"] <- "Andropogon gerardii"
yu_abun$species[yu_abun$species == "Amorphacanescens"] <- "Amorpha canescens"
yu_abun$species[yu_abun$species == "Ambrosiapsilostachya"] <- "Ambrosia psilostachya"
yu_abun$species[yu_abun$species == "Ambrosiaartemisiifolia"] <- "Ambrosia artemisiifolia"
yu_abun$species[yu_abun$species == "Alliumtextile"] <- "Allium textile"
yu_abun$species[yu_abun$species == "Achilleamillefolium"] <- "Achillea millefolium"



### Remove all species codes that are not scientific names
filter_species <- function(species) {
  grepl("\\d", species) || (nchar(gsub("[^A-Z]", "", species)) > 1)
}
# Apply the filter function to remove unwanted rows
filtered_df <- yu_abun %>%
  filter(!sapply(species, filter_species))


### Separate dataframes for each site
knz <- filtered_df %>%
  filter(site == "KNZ")
hys <- filtered_df %>%
  filter(site == "HYS")
sgs <- filtered_df %>%
  filter(site == "SGS")
chy <- filtered_df %>%
  filter(site == "CHY")



# Upload data
path_out = "/Volumes/seas-zhukai/proj-ecoacc-experiment/Yu_2025_Nature/"
write.csv(filtered_df,paste(path_out,'yu_clean.csv'),row.names=F)
write.csv(knz,paste(path_out,'knz_clean.csv'),row.names=F)
write.csv(hys,paste(path_out,'hys_clean.csv'),row.names=F)
write.csv(sgs,paste(path_out,'sgs_clean.csv'),row.names=F)
write.csv(chy,paste(path_out,'chy_clean.csv'),row.names=F)






