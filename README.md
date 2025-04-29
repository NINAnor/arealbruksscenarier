# Source code collection "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050"

This repository collects the main scripts to reproduce the research communicated in the publication "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050" from Simensen et al., 2025. 
In the **R folder** you find:
* 1_AGR_calculations.R which is the core routine to calculate land take per capita for each Norwegian municipality
* 2_tables and figures need the output of the first script as input and produce the different tables and plots for the publication
The **data folder** contains the output table from *1_AGR_calculations.R* and the **reports folder** the preprocessed plots.

## Input data
You can download the following input data used for the analysis from the following repository


| File    | Source | Description                                                                 |
|------------------|------------|-----------------------------------------------------------------------------|
| `area_measured_and_estimated_2000_2050.csv`             | ??    | Unique identifier for each record.                                         |
| `arealnøytrale_kommuner_2024.csv`      | SABIMA   | Date and time the data was recorded (ISO 8601 format).                     |
| `change_data_population_area_2000_2050.csv`   | Float      | The raw reading from the sensor device.                                    |
| `fcKomGLCkm2_tibble.csv`      | python     | Unique identifier of the device collecting the data.                       |
| `fcKomPop.csv`         | ?     | Operational status of the sensor (`active`, `inactive`, or `error`).      |
| `komm_var`       | ?     | Geographic location or label of the sensor’s position.                    |
| `kommuner_land.gpkg`       | Kartverket     | Administrative units of Norway                    |
| `tabell_ssb_954_2011_2022.csv`       | SSB     | 09594: Arealbruk og arealressurser, etter arealklasse (km²) (K) (B) 2011 - 2024                    |
