# EcoAcc - Warming experiments & thermophilization

This repository contains R scripts to analyze data from six warming experiments (JRGCE, PHACE, Oklahoma, TeRaCON, B4WarmED CFC, and B4WarmED HWRC) in the United States. The repository is organized by folders in order from 1-7, following the workflow of the scripts.

## Scripts

### 0-testing

This folder contains testing scripts for preliminary analyses conducted throughout the project; the analyses in these folders were subsidary to the main project.

### 1-clean

This folder contains scripts to clean the raw data from each experiment. The 'phylogenies.R' script then takes the clean data and generates phylogenies for the species from each experiment.

### 2-gbif

This folder contains scripts to download and clean GBIF data for all species from each experiment. The 'GBIF_derived_dataset.R' script then subsets the datasetKey information from each experiment for uploading to the GBIF platform, in order to generate a data citation.

### 3-chelsa

This folder contains scripts that merge CHELSA climate data with each species occurrence point data downloaded in the GBIF scripts. The 'MAT_MAP_download.R' script pulls annual MAT and MAP climate files from TerraClim, and the 'MAT.R' script then matches the climate data with each site.

### 4-niche_est

### 5-cti_cpi

### 6-ecosystem_response

### 7-combine_experiments
