# Source code collection "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050"

This repository collects the main scripts to reproduce the research communicated in the publication "Samlet utbyggingsareal i Kommune-Norge: Historisk utvikling og framtidige scenarioer, 2000–2050" from Simensen et al., 2025. 
In the **R folder** you find:
* 1_AGR_calculations.R which is the core routine to calculate land take per capita for each Norwegian municipality
* 2_tables and figures need the output of the first script as input and produce the different tables and plots for the publication
The **data folder** contains the output table from *1_AGR_calculations.R* and the **reports folder** the preprocessed plots.

## Input data
You can download the following input data used for the analysis from the following repository


| File    | Data Type | Description                                                                 |
|------------------|------------|-----------------------------------------------------------------------------|
| `area_measured_and_estimated_2000_2050`             | Integer    | Unique identifier for each record.                                         |
| `timestamp`      | Datetime   | Date and time the data was recorded (ISO 8601 format).                     |
| `sensor_value`   | Float      | The raw reading from the sensor device.                                    |
| `device_id`      | String     | Unique identifier of the device collecting the data.                       |
| `status`         | String     | Operational status of the sensor (`active`, `inactive`, or `error`).      |
| `location`       | String     | Geographic location or label of the sensor’s position.                    |
