####

### 0.setup
# source functions
source("R/utils_functions.R")

#  required packages
required_packages <- c("biscale","reshape2","tidyverse", "sf", "tmap","readxl","cowplot","ggrepel","scales","openxlsx","parallel")

# Install and load 
install_if_needed(required_packages)

# Language and notation settings
Sys.setlocale(locale='no_NB.utf8') 
options(scipen = 999)


### 1. folder paths and data

raw_folder <- "data-raw/" 


# Original area, impervious surfaces in GLC (see python script)
fcKomGLCkm2 <- read.csv(file.path(raw_folder, "fcKomGLCkm2_tibble.csv")) |> as_tibble()

# Population data and estimates from SSB
fcKomPop <- read.csv(file.path(raw_folder, "fcKomPop.csv")) |> as_tibble()

# Global land cover data (GLC) (see python script)
area_measured_and_estimated <- read.csv(file.path(raw_folder, "area_measured_and_estimated_2000_2050.csv")) |> as_tibble()

# ssb 954 to compare with GLC
ssb_954_table <- read_csv(file.path(raw_folder,"tabell_ssb_954_2011_2022.csv")) |> as_tibble()
# # Preprosessed population data (for plotting, etc)
# area_estimated <- read.csv(file.path(data_folder, "area_estimated_2000_2050.csv")) |> as_tibble()
# 
# HOW was this computed?
change_data <- read.csv(file.path(raw_folder, "change_data_population_area_2000_2050.csv")) |> as_tibble()


### 2. Compare GLC with SSB (Methods, page 5 in Simensen et al., 2025)
ssb_954_table <- ssb_954_table %>%
  rename_with(~ paste0("ssb_", .), starts_with("areal"))
ssb_954_table$kommunenummer <- as.integer(ssb_954_table$kommunenummer)

comp_table <- area_measured_and_estimated |> select(kommunenummer, KomNavn, km2_2017:km2_2022)

comp_table <- comp_table %>%
  rename_with(~ paste0("glc_", .), starts_with("km2"))
# join ssb and glc for correlations
comp_table <- left_join(comp_table, ssb_954_table, by = "kommunenummer")
comp_table$glc_change <- comp_table$glc_km2_2022-comp_table$glc_km2_2017



### 3. Change analysis
bpc_data <- change_data %>%
  mutate(
    km2_change_2000_2022 = ifelse(km2_change_2000_2022 < 0, 0, km2_change_2000_2022),
    km2_change_2022_2034 = ifelse(km2_change_2022_2034 < 0, 0, km2_change_2022_2034),
    km2_change_2022_2050 = ifelse(km2_change_2022_2050 < 0, 0, km2_change_2022_2050)
  )



### 3.1 Land take per capita
# From km2 to m2, 
# pop = population in inhabitants
# t = time (e.g., t0 = 2000, t1 = 2022, t2 = 2050)
# bpc = built up area per capita at time 1, 2, etc


bpc_data <- bpc_data |> mutate(area_t0 = km2_2000*1e6,
                               area_t1 = km2_2022*1e6,
                               area_t2 = km2_2050*1e6,
                               pop_t0 = pop2000,
                               pop_t1 = pop2022,
                               pop_t2 = pop2050)



# Diagnostic: Identify rows where area_t1 is smaller than area_t0
bpc_data <- bpc_data %>%
  mutate(area_t1_smaller = area_t1 < area_t0)

# Print diagnostic result
#print(bpc_data %>% filter(area_t1_smaller == TRUE))

# Correct for negative growth in area report in Simensen et al.,2025 p.7
bpc_data <- bpc_data |> mutate(
  area_t1 = if_else(area_t1 < area_t0, area_t0 + 0.001, area_t1))


# Correct for negative growth in area #page 7
bpc_data <- bpc_data |> mutate(
  area_t2 = if_else(area_t2 < area_t1, area_t1 + 0.001, area_t2))

# Diagnostic: Identify rows where area_t1 is smaller than area_t0
bpc_data <- bpc_data %>%
  mutate(area_t1_smaller = area_t1 < area_t0)


bpc_data <- bpc_data |> select(kommunenummer,
                               KomNavn,
                               area_t0,
                               area_t1,
                               area_t2,
                               pop_t0,
                               pop_t1,
                               pop_t2,
                               km2_change_2000_2022,
                               km2_change_2022_2050,
                               pop_change_2000_2022,
                               pop_change_2022_2050)


# Step 1: Calculate BPC and AGR for Period 1 (2000-2022) and Period 2 (2022-2050) #page 8
bpc_data <- bpc_data %>%
  mutate(
    # Built-up area per capita (BPC)
    bpc_t0 = area_t0 / pop_t0,
    bpc_t1 = area_t1 / pop_t1,
    bpc_t2 = area_t2 / pop_t2,
    
    # Annual Growth Rate for Population (AGR_pop) - 
    AGR_pop_1 = ((pop_t1 / pop_t0)^(1 / 22) - 1) * 100,
    AGR_pop_2 = ((pop_t2 / pop_t1)^(1 / 28) - 1) * 100,
    
    # Annual Growth Rate for BPC (AGR_bpc) 
    AGR_bpc_1 = ((bpc_t1 / bpc_t0)^(1 / 22) - 1) * 100,
    AGR_bpc_2 = ((bpc_t2 / bpc_t1)^(1 / 28) - 1) * 100
  )


# Step 2: Conservative Adjustments 
bpc_data <- bpc_data %>%
  mutate(
    # Small adjustments to avoid division/log issues
    area_t1 = if_else(area_t1 == area_t0, area_t1 + 0.0001, area_t1),
    area_t2 = if_else(area_t2 == area_t1, area_t2 + 0.0001, area_t2),
    pop_t1 = if_else(pop_t1 == pop_t0, pop_t1 + 0.0001, pop_t1),
    pop_t2 = if_else(pop_t2 == pop_t1, pop_t2 + 0.0001, pop_t2),
    bpc_t0 = if_else(bpc_t0 == 0, bpc_t0 + 1e-10, bpc_t0),
    bpc_t1 = if_else(bpc_t1 == 0, bpc_t1 + 1e-10, bpc_t1),
    bpc_t2 = if_else(bpc_t2 == 0, bpc_t2 + 1e-10, bpc_t2)
  )

# # Step 3: Calculate APOP and ABPC
# bpc_data <- bpc_data %>%
#   mutate(
#     # Period 1: APOP and ABPC (2000-2022)
#     APOP = (area_t1 - area_t0) * (log(pop_t1 / pop_t0) / log(area_t1 / area_t0)),
#     ABPC = (area_t1 - area_t0) * (log(bpc_t1 / bpc_t0) / log(area_t1 / area_t0)),
#     
#     # Period 2: APOP and ABPC (2022-2050)
#     APOP_t_1_2 = (area_t2 - area_t1) * (log(pop_t2 / pop_t1) / log(area_t2 / area_t1)),
#     ABPC_t_1_2 = (area_t2 - area_t1) * (log(bpc_t2 / bpc_t1) / log(area_t2 / area_t1))
#   )

# Replace NaNs with 0.00001 in AGR_bpc_1 and AGR_bpc_2
bpc_data$AGR_bpc_1[is.nan(bpc_data$AGR_bpc_1)] <- 0.00001
bpc_data$AGR_bpc_2[is.nan(bpc_data$AGR_bpc_2)] <- 0.00001


# Step 4: Define Thresholds and Classify Changes
# Use the same thresholds for both periods for consistency page 8
thresholds_pop <- quantile(bpc_data$AGR_pop_1, probs = c(0.33, 0.67), na.rm = TRUE)
thresholds_bpc <- quantile(bpc_data$AGR_bpc_1, probs = c(0.33, 0.67), na.rm = TRUE)

# Classify changes using thresholds (classes see Simensen et al., 2025 p.9)
bpc_data <- bpc_data %>%
  mutate(
    # Population change classes
    pop_change_class_norsk = case_when(
      AGR_pop_1 < thresholds_pop[1] ~ "Nedgang",
      AGR_pop_1 > thresholds_pop[2] ~ "Vekst",
      TRUE ~ "Stabil"
    ),
    pop_change_class_t_1_2_norsk = case_when(
      AGR_pop_2 < thresholds_pop[1] ~ "Nedgang",
      AGR_pop_2 > thresholds_pop[2] ~ "Vekst",
      TRUE ~ "Stabil"
    ),
    
    # BPC change classes
    bpc_change_class_norsk = case_when(
      AGR_bpc_1 < thresholds_bpc[1] ~ "Liten endring",
      AGR_bpc_1 > thresholds_bpc[2] ~ "Sterk økning",
      TRUE ~ "Moderat økning"
    ),
    bpc_change_class_t_1_2_norsk = case_when(
      AGR_bpc_2 < thresholds_bpc[1] ~ "Liten endring",
      AGR_bpc_2 > thresholds_bpc[2] ~ "Sterk økning",
      TRUE ~ "Moderat økning"
    )
  )

# Table 1: Summary Statistics of BPC for Historical and Future Periods Simensen et al., 2025 p.11
# summary_stats <- bpc_data %>%
#   summarize(
#     # Historical period
#     mean_bpc_t0 = mean(bpc_t0, na.rm = TRUE),
#     mean_bpc_t1 = mean(bpc_t1, na.rm = TRUE),
#     mean_agr_pop_1 = mean(AGR_pop_1, na.rm = TRUE),
#     mean_agr_bpc_1 = mean(AGR_bpc_1, na.rm = TRUE),
#     
#     # Future period
#     mean_bpc_t2 = mean(bpc_t2, na.rm = TRUE),
#     mean_agr_pop_2 = mean(AGR_pop_2, na.rm = TRUE),
#     mean_agr_bpc_2 = mean(AGR_bpc_2, na.rm = TRUE)
#   )



# Create classes for combinations of scenarios for both historical and future periods
# Create a new combined variable by pasting together the population and BPC change classes for each period
bpc_data$combined_change_class_1 <- paste(
  "pop", bpc_data$pop_change_class_norsk,
  "bpc", bpc_data$bpc_change_class_norsk,
  sep = "_"
)

bpc_data$combined_change_class_2 <- paste(
  "pop", bpc_data$pop_change_class_t_1_2_norsk,
  "bpc", bpc_data$bpc_change_class_t_1_2_norsk,
  sep = "_"
)

# Define the reverse mapping for the combined categories
category_labels <- c(
  "a) Overflødig utbygd areal" = "pop_Nedgang_bpc_Liten endring",
  "b) Stillstand i utbygd areal" = "pop_Nedgang_bpc_Moderat økning",
  "c) Nedgang-spredning" = "pop_Nedgang_bpc_Sterk økning",
  "d) Stillstand i utbygd areal" = "pop_Stabil_bpc_Liten endring",
  "e) Moderat arealvekst" = "pop_Stabil_bpc_Moderat økning",
  "f) Ekspansjon i utbygd areal" = "pop_Stabil_bpc_Sterk økning",
  "g) Arealeffektiv vekst" = "pop_Vekst_bpc_Liten endring",
  "i) Rask vekst i utbygd areal" = "pop_Vekst_bpc_Moderat økning",
  "h) Proporsjonal vekst" = "pop_Vekst_bpc_Sterk økning"
)

# Apply the labels to both historical and future periods
bpc_data$category_1 <- factor(
  bpc_data$combined_change_class_1,
  levels = category_labels,
  labels = names(category_labels)
)

bpc_data$category_2 <- factor(
  bpc_data$combined_change_class_2,
  levels = category_labels,
  labels = names(category_labels)
)

# order from "a)" to "i)"
bpc_data$category_1 <- factor(bpc_data$category_1, levels = names(category_labels))
bpc_data$category_2 <- factor(bpc_data$category_2, levels = names(category_labels))


# Calculate BPC ratios 
bpc_data <- bpc_data %>%
  mutate(
    BPC_ratio = bpc_t1 / bpc_t0,         # Ratio for historical data (2000–2022)
    BPC_ratio_t_1_2 = bpc_t2 / bpc_t1    # Ratio for future data (2022–2050)
  )

# check utf before save
write.csv(bpc_data,"data/bpc_data.csv",fileEncoding = "UTF-8")
