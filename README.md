# Source code collection "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050"

English title: *"Land Take in Norwegian Municipalities: Historical Development and Future Scenarios, 2000–2050"*

This repository collects the main scripts to reproduce the research communicated in the publication "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050" from Simensen et al., 2025. 
In the **R folder** you find:
* 1_AGR_calculations.R which is the core routine to calculate land take per capita for each Norwegian municipality
* 2_tables and figures need the output of the first script as input and produce the different tables and plots for the publication

  The **data-raw** folder contains input data, used for the analysis. 
  
| File    | Source | Description                                                                 |
|------------------|------------|-----------------------------------------------------------------------------|
| `area_measured_and_estimated_2000_2050.csv`             | GLC_FCS30 (Zhang mfl., 2021 - https://doi.org/10.5194/essd-13-2753-2021)   | Linear regression and extrapolation of future trends of land take for Norwegian municipalities                                       |
| `arealnøytrale_kommuner_2024.csv`      | SABIMA   | Information regarding the Norwegian municipalities aiming for land neutrality.                     |
| `change_data_population_area_2000_2050.csv`   | SSB      | Based on:  https://www.ssb.no/statbank/table/14288                   |
| `fcKomPop.csv`         |     | Curated data frame from area_measured_and_estimated_2000_2050.csv and change_data_population_area_2000_2050.csv  |
| `kommuner_land.gpkg`       | Kartverket     | Administrative units of Norway                    |
| `tabell_ssb_954_2011_2022.csv`       | SSB     | 09594: Arealbruk og arealressurser, etter arealklasse (km²) (K) (B) 2011 - 2024                    |

The **data folder** contains the output table from *1_AGR_calculations.R* and the **reports folder** the preprocessed plots.

## Input data
You can download the following input data used for the analysis from the following repository

[![DOI](https://zenodo.org/badge/913708722.svg)](https://doi.org/10.5281/zenodo.15370391)

