# EcoAcc - Warming experiments & thermophilization

This repository contains R scripts to analyze data from six warming experiments (JRGCE, PHACE, Oklahoma, TeRaCON, B4WarmED CFC, and B4WarmED HWRC) in the United States. Analyses primarily include tests of Community Temperature Index (CTI) changes over time for each experiment, as well as tests of species contributions to changes in CTI. The repository is organized by folders in order from 1-7, following the workflow of the scripts.

## Scripts

### 0-testing

This folder contains testing scripts for preliminary analyses conducted throughout the project; the analyses in these folders were subsidary to the main project.

### 1-clean

This folder contains scripts to clean the raw data from each experiment. The 'phylogenies.R' script then takes the clean data and generates phylogenies for the species from each experiment.

### 2-gbif

This folder contains scripts to download and clean GBIF data for all species from each experiment. GBIF occurrence records were downloaded for a 2000 km x 2000 km bounding box around each experiment's location. The 'GBIF_derived_dataset.R' script then subsets the datasetKey information from each experiment for uploading to the GBIF platform, in order to generate a data citation.

### 3-chelsa

This folder contains scripts that merge CHELSA climate data with each species occurrence point data downloaded in the GBIF scripts. The 'MAT_MAP_download.R' script pulls annual MAT and MAP climate files from TerraClim, and the 'MAT.R' script then matches the climate data with each site.

### 4-niche_est

This folder contains scripts to calculate species-level temperature niche estimates for each experiment.

### 5-cti_cpi

This folder contains scripts to calculate community temperature and precipitation indices (CTI and CPI) for each experiment, plot, and treatment.

### 6-ecosystem_response

This folder contains scripts to clean biomass data from each experiment. The biomass data and associated analyses were not included in the main findings of this project.

### 7-combine_experiments

Within this folder, 'cti_figs.R' contains code to visualize CTI multiple ways for each experiment; 'map.R' makes a map of the locations of each of the six experiments, and 'models.R' tests for changes in CTI over time, due to warming, as well as tests for trait-contribution relationships. This folder also contains sub-folders related to separate parts of the analyses:

#### TRY

The 'TRY' folder contains scripts that extract and clean TRY plant trait data for all species found within the experiments.

#### contributions

The 'contributions' folder contains the scripts used for calculating and plotting species contributions to CTI. 'contributions.R' calculates species contributions to overall CTI; 'contributions_shortterm.R' calculates yearly species contributions to CTI; 'contributions_treatment.R' calculates species contributions to the warming treatment effect on CTI; and 'contributions_treatment_shortterm.R' calculates yearly species contributions to the warming treatment effect on CTI.
