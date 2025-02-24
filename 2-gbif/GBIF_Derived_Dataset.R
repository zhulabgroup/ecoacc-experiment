# TITLE:          Derived GBIF dataframes for citation
# AUTHORS:        Kara Dobson
# COLLABORATORS:  Kai Zhu, Peter Reich
# DATA INPUT:     Clean JRGCE data read in
# DATA OUTPUT:    GBIF data for all experiments
# PROJECT:        EcoAcc
# DATE:           Feb 2025



# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/JRGCE/"
setwd(path_data)
jrgce <- read.csv(" GBIF_jrgce.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
cfc <- read.csv(" GBIF_b4warmed_cfc.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/B4Warmed/"
setwd(path_data)
hwrc <- read.csv(" GBIF_b4warmed_hwrc.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/TeRaCON/"
setwd(path_data)
tera <- read.csv(" GBIF_teracon.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/PHACE/"
setwd(path_data)
phace <- read.csv(" GBIF_phace.csv")

# Set path to turbo to get data
path_data = "/Volumes/seas-zhukai/proj-ecoacc-experiment/OK/"
setwd(path_data)
ok <- read.csv(" GBIF_ok.csv")



### Selecting datasetKey and counting # of occurrences
jrgce <- jrgce %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())
cfc <- cfc %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())
hwrc <- hwrc %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())
tera <- tera %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())
phace <- phace %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())
ok <- ok %>%
  select(datasetKey) %>%
  group_by(datasetKey) %>%
  summarise(n = n())



### Merge all together
all <- rbind(jrgce, cfc, hwrc, tera, phace, ok)
all <- all %>%
  group_by(datasetKey) %>%
  summarize(n = sum(n))


### Export as csv
write.csv(all, "GBIF_all.csv", row.names = FALSE)

