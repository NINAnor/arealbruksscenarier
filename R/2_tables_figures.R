# tables and figures
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

data_folder <- "data/"
raw_folder<- "data-raw"

# Co-variates for the municipalities
komm_var <- read.csv(file.path(raw_folder, "komm_var.csv")) |> as_tibble()

arealnoytral <- read.csv(file.path(raw_folder, "arealnøytrale_kommuner_2024.csv"), sep = ";") |> as_tibble()
arealnoytral$arealnoytral[is.na(arealnoytral$arealnoytral)] <- 0
arealnoytral$kommunenummer <- as.integer(arealnoytral$kommunenummer)

#join
komm_var <- left_join(komm_var, arealnoytral, by = "kommunenummer")


bpc_data<-read.csv(paste0(data_folder,"bpc_data.csv"))

# Spatial data for NOR municipalities
kommuner_land <- st_read(file.path(raw_folder, "kommuner_land.gpkg"))

# Population data and estimates from SSB
fcKomPop <- read.csv(file.path(raw_folder, "fcKomPop.csv")) |> as_tibble()

# Global land cover data (GLC) (see python script)
area_measured_and_estimated <- read.csv(file.path(raw_folder, "area_measured_and_estimated_2000_2050.csv")) |> as_tibble()

#### BPC table p. 11
# Step 1: Prepare the Detailed Data for All Municipalities for both periods
detailed_data_all <- bpc_data %>%
  select(
    kommunenummer, KomNavn, 
    # Historical data (2000–2022)
    area_t0, area_t1, km2_change_2000_2022, pop_t0, pop_t1, pop_change_2000_2022, 
    bpc_t0, bpc_t1, AGR_pop_1, AGR_bpc_1, BPC_ratio,
    pop_change_class_norsk, bpc_change_class_norsk, category_1,
    
    # Future data (2022–2050)
    area_t2, km2_change_2022_2050, pop_t2, pop_change_2022_2050, 
    bpc_t2, AGR_pop_2, AGR_bpc_2, BPC_ratio_t_1_2,
    pop_change_class_t_1_2_norsk, bpc_change_class_t_1_2_norsk, category_2
  )

# Step 2: Prepare the Classified Data for All Municipalities for both periods
classified_data_all <- bpc_data %>%
  select(
    kommunenummer, KomNavn, 
    pop_change_class_norsk, bpc_change_class_norsk, category_1,
    pop_change_class_t_1_2_norsk, bpc_change_class_t_1_2_norsk, category_2
  )

# Melt the data to a long format for plotting

plot_data <- melt(detailed_data_all[, c("AGR_bpc_1", "AGR_pop_1")], 
                  variable.name = "Growth_Type", 
                  value.name = "Growth_Rate")


# Create a logical vector to check where BPC growth outpaces population growth
detailed_data_all$bpc_outpaces_pop <- detailed_data_all$AGR_bpc_1 > detailed_data_all$AGR_pop_1

# Count the number of municipalities where this is true report in Simensen et al., 2025 p.15
sum(detailed_data_all$bpc_outpaces_pop, na.rm = TRUE)

# Subset the municipalities where BPC growth outpaces population growth
municipalities_outpaced <- detailed_data_all |> filter(bpc_outpaces_pop == TRUE)

print(municipalities_outpaced$KomNavn)  # Adjust the column name as needed


# Filter only numeric columns (of type dbl and int) to exclude chr and fct columns
numerical_data <- detailed_data_all %>%
  select(where(is.numeric)) |> 
  select(-kommunenummer)


numerical_data <- numerical_data |> mutate(area_t0 = area_t0*0.000001,
                                           area_t1 = area_t1*0.000001,
                                           area_t2 = area_t2*0.000001)


# Step 1: Calculate summary statistics for each numeric variable individually
numerical_summary <- numerical_data %>%
  summarise(across(where(is.numeric), list(
    Min = ~min(.x, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    Mean = ~mean(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE),
    SD = ~sd(.x, na.rm = TRUE)
  )))

# Step 2: Reshape to a long format without issues from multi-part column names
numerical_summary_long <- numerical_summary %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Statistic"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = value)

# View the final summary table
print(numerical_summary_long)


# Step 1: Round the summary statistics to three decimal places
numerical_summary_long <- numerical_summary_long %>%
  mutate(across(c(Min, Median, Mean, Max, SD), round, 3))



# Step 2: Create the description table
description_table <- tibble(
  Variable = c(
    "area_t0", "area_t1", "km2_change_2000_2022", "pop_t0", "pop_t1", 
    "pop_change_2000_2022", "bpc_t0", "bpc_t1", "AGR_pop_1", "AGR_bpc_1", 
    "BPC_ratio", "area_t2", "km2_change_2022_2050", "pop_t2", "pop_change_2022_2050", 
    "bpc_t2", "AGR_pop_2", "AGR_bpc_2", "BPC_ratio_t_1_2"
  ),
  Description_English = c(
    "Area in square meters at initial time (t0)", 
    "Area in square meters at time t1", 
    "Change in area (km^2) between 2000 and 2022", 
    "Population at initial time (t0)", 
    "Population at time t1", 
    "Population change from 2000 to 2022", 
    "Built-up area per capita at initial time (t0)", 
    "Built-up area per capita at time t1", 
    "Annual growth rate of population (t0 to t1)", 
    "Annual growth rate of built-up area per capita (t0 to t1)", 
    "Ratio of built-up area per capita (t1/t0)", 
    "Area in square meters at time t2", 
    "Change in area (km^2) between 2022 and 2050", 
    "Population at time t2", 
    "Population change from 2022 to 2050", 
    "Built-up area per capita at time t2", 
    "Annual growth rate of population (t1 to t2)", 
    "Annual growth rate of built-up area per capita (t1 to t2)", 
    "Ratio of built-up area per capita (t2/t1)"
  ),
  Description_Norwegian = c(
    "Areal i kvadratmeter ved starttidspunkt (t0)", 
    "Areal i kvadratmeter ved tidspunkt t1", 
    "Endring i areal (km^2) mellom 2000 og 2022", 
    "Befolkning ved starttidspunkt (t0)", 
    "Befolkning ved tidspunkt t1", 
    "Befolkningsendring fra 2000 til 2022", 
    "Utbygd areal per innbygger ved starttidspunkt (t0)", 
    "Utbygd areal per innbygger ved tidspunkt t1", 
    "Årlig vekstrate for befolkning (t0 til t1)", 
    "Årlig vekstrate for utbygd areal per innbygger (t0 til t1)", 
    "Forhold mellom utbygd areal per innbygger (t1/t0)", 
    "Areal i kvadratmeter ved tidspunkt t2", 
    "Endring i areal (km^2) mellom 2022 og 2050", 
    "Befolkning ved tidspunkt t2", 
    "Befolkningsendring fra 2022 til 2050", 
    "Utbygd areal per innbygger ved tidspunkt t2", 
    "Årlig vekstrate for befolkning (t1 til t2)", 
    "Årlig vekstrate for utbygd areal per innbygger (t1 til t2)", 
    "Forhold mellom utbygd areal per innbygger (t2/t1)"
  )
)

# Step 3: Left join the description table with the numerical summary table
final_summary_table <- description_table %>%
  left_join(numerical_summary_long, by = "Variable")

# View the final summary table
print(final_summary_table)

final_summary_table_df <- as.data.frame(final_summary_table)

write.csv(final_summary_table_df,"reports/table_1.csv") #page 11 in Simensen et al., 2025



############### figures

komm_var_simple <- komm_var |> select(-iKomNr, -KomNavn)%>%  left_join(komm_var_simple, by = "kommunenummer")

# Merge Data with Spatial Object
kommuner_land$kommunenummer <-as.integer(kommuner_land$kommunenum)

bpc_sf <- kommuner_land %>%
  left_join(bpc_data, by = "kommunenummer")

norway <- st_union(kommuner_land)


bpc_sf$AGR_pop_1_round <- round(bpc_sf$AGR_pop_1, 1)
bpc_sf$AGR_bpc_1_round <- round(bpc_sf$AGR_bpc_1, 1)


#### Fig 4a
map4a <- tm_shape(bpc_sf) +
  tm_fill("AGR_bpc_1_round", 
          fill.legend = tm_legend(title = "Årlig prosentvis \nendring"),
          fill.scale = tm_scale_intervals(n = 8, values = "brewer.rd_bu", style = "fisher")
          #fill.scale = tm_scale_intervals(n = 5, style = "fisher", values = "brewer.YlOrRd", ) # Set 10 intervals with the "pretty" style
  ) +
  tm_borders(col = "grey50", lwd = 0.5) +
  tm_shape(norway)+
  tm_borders(col = "grey60", lwd = 0.5) +
  tm_layout(
    legend.position = c("right", "bottom"),
    legend.bg.color = "white",
    legend.frame = FALSE,
    frame = FALSE,
    legend.outside = FALSE,
    title.position = c("left", "top"), # Position the title in the top-left
    title.bg.color = "white",          # Add background color to the title
    title.frame = FALSE                 # Add a frame around the title
  ) +
  tm_title(
    text = "a)\nUtbygd areal\n per innbygger\n 2000 – 2022", 
    size = 1, fontface = "bold"
  ) 

map4a


### Simensen et al., 2025 Fig 4b
map4b <- tm_shape(bpc_sf) +
  tm_polygons(fill = "AGR_pop_1_round", 
              #fill.scale = tm_scale_intervals(values = "purple_green"), 
              #fill.scale = tm_scale_intervals(n = 8, values = "brewer.rd_bu", midpoint = NA),
              fill.scale = tm_scale_intervals(n = 8, values = "brewer.rd_bu", style = "fisher"),
              fill.legend = tm_legend(title = "Årlig prosentvis \nendring"))+
  tm_borders(col = "grey80", lwd = 0.5) +
  tm_shape(norway)+
  tm_borders(col = "grey0", lwd = 0.5) +
  tm_layout(
    legend.position = c("right", "bottom"),
    legend.bg.color = "white",
    legend.frame = FALSE,
    frame = FALSE,
    legend.outside = FALSE,
    title.position = c("left", "top"), # Position the title in the top-left
    title.bg.color = "white",          # Add background color to the title
    title.frame = FALSE                 # Add a frame around the title
  ) +
  tm_title(
    text = "Befolkningsendringer \n2000–2022", 
    size = 1, fontface = "bold"
  ) 

map4b

##### figure 5
### Simensen et al., 2025 Fig 5
arealnoytral_map <- kommuner_land %>%
  left_join(arealnoytral, by = "kommunenummer") |> 
  filter(arealnoytral == 1)



map5 <- tm_shape(bpc_sf) +
  tm_polygons(
    fill = tm_vars(c("pop_change_class_t_1_2_norsk", "bpc_change_class_t_1_2_norsk"), 
                   multivariate = TRUE), 
    fill.scale = tm_scale_bivariate(
      scale1 = tm_scale_categorical(),
      scale2 = tm_scale_categorical(),
      values = "brewer.qualseq"
    )
  ) +
  tm_shape(bpc_sf) +  # Add an additional layer for white borders
  tm_borders(col = "grey99", lwd = 0.5) +  # Draw only the borders
  tm_shape(norway) +
  tm_borders(col = "grey60", lwd = 0.5) +
  tm_shape(arealnoytral_map) +
  tm_borders(col = "#e31a1c", lwd = 1.5) +
  tm_layout(
    title.size = 1,
    title.position = c("left", "top"),         # Position title in the upper left
    title.bg.color = "white",                  # Add a white background to the title
    title.bg.alpha = 0.8,                      # Make the background slightly transparent if desired
    legend.position = c("right", "bottom"),
    legend.bg.color = "white",                 # Set a white background for the legend
    legend.frame = FALSE,                      # No frame around the legend
    frame = FALSE,
    legend.outside = FALSE,
    legend.show = FALSE                        # Suppresses the entire legend
  )+
  tm_title("\nFramskriving\n2022–2050")


map5





bpc_sf <- bpc_sf |> mutate(km_change_label = round(km2_change_2000_2022, 2))
bpc_sf <- bpc_sf |> mutate(pop_change_label = (pop_t1 - pop_t0))
bpc_sf <- bpc_sf |> mutate(pop_label = (pop_t1))
bpc_sf <- bpc_sf |> mutate(AGR_pop_label = round(AGR_pop, 2))
bpc_sf <- bpc_sf |> mutate(AGR_bpc_label = round(AGR_bpc, 2))


map11 <- tm_shape(bpc_data3) +
  tm_polygons(
    fill = tm_vars(c("Befolkning", "Areal"), 
                   multivariate = TRUE), 
    fill.scale = tm_scale_bivariate(
      scale1 = tm_scale_categorical(),
      scale2 = tm_scale_categorical(),
      values = "brewer.qualseq"
    )) +
  # Add labels with municipality name and km2 change
  tm_text("KomNavn", size = 0.5, col = "black", ymod = 0.7) +  # Add labels for each municipality
  tm_text("km_change_label", size = 0.5, col = "black", ymod = 0) +  # ymod moves the label downwards
  tm_text("pop_change_label", size = 0.5, col = "black", ymod = -0.7) +  # ymod moves the label downwards
  tm_text("AGR_pop_label", size = 0.5, col = "black", ymod = -1.4) +  # ymod moves the label downwards
  tm_text("AGR_bpc_label", size = 0.5, col = "black", ymod = -2.1) +  # ymod moves the label downwards
  tm_shape(norway) +
  tm_borders(col = "grey0", lwd = 0.5) +
  #tm_text(text = paste(bpc_data3$KomNavn, round(bpc_data3$km2_change_2022_2050, 2), "km²"), size = 0.7, just = "left") +
  tm_layout(
    title = "Endring i\nbefolkning og\nutbygd areal\nper innbygger\n2000–2022",
    title.size = 1.5,
    title.position = c("left", "top"),
    title.bg.color = "white",
    title.bg.alpha = 0.8,
    legend.position = c("right", "bottom"),
    legend.bg.color = "white",
    legend.frame = FALSE,
    frame = TRUE,
    legend.outside = FALSE,
    legend.show = TRUE
  ) +
  tm_graticules(lines = FALSE)





# Extract and sort year-based columns
year_columns <- names(fcKomPop) %>%
  grep("^pop\\d{4}$", ., value = TRUE) %>%
  sort()

# Reorder columns with the correct sequence
population <- fcKomPop %>%
  select(
    kommunenummer, iKomNr, KomNavn, iKomNr2023, txtFylke, # Description columns
    all_of(year_columns), # Year-based columns in ascending order
    everything() # Include any remaining columns
  )

pop_2000_2022 <- population[, c(2:3, 6:28)]
pop_2022_2050 <- population[, c(2:3, 28:58)]

pop_2000_2022 <- pop_2000_2022 |> rename(kommunenummer = iKomNr)
pop_2022_2050 <- pop_2022_2050 |> rename(kommunenummer = iKomNr)

# AGR bpc
AGR_bpc <- bpc_data |> select(kommunenummer, KomNavn, bpc_t1, AGR_bpc_1) |> rename(AGR_bpc = AGR_bpc_1, bpc_2022 = bpc_t1)


tot_area_2022_2050_linear_pred <- area_measured_and_estimated [ ,c(1:2, 25:53)]
area_trendline_2000_2022 <- area_measured_and_estimated[,1:25]

# Net neutral scenario ----------------------------------------------------

scenario1_net_neutral <- tot_area_2022_2050_linear_pred %>%
  mutate(
    # Copy 2022 area into all future years
    km2_2023 = km2_2022,
    km2_2024 = km2_2022,
    km2_2025 = km2_2022,
    km2_2026 = km2_2022,
    km2_2027 = km2_2022,
    km2_2028 = km2_2022,
    km2_2029 = km2_2022,
    km2_2030 = km2_2022,
    km2_2031 = km2_2022,
    km2_2032 = km2_2022,
    km2_2033 = km2_2022,
    km2_2034 = km2_2022,
    km2_2035 = km2_2022,
    km2_2036 = km2_2022,
    km2_2037 = km2_2022,
    km2_2038 = km2_2022,
    km2_2039 = km2_2022,
    km2_2040 = km2_2022,
    km2_2041 = km2_2022,
    km2_2042 = km2_2022,
    km2_2043 = km2_2022,
    km2_2044 = km2_2022,
    km2_2045 = km2_2022,
    km2_2046 = km2_2022,
    km2_2047 = km2_2022,
    km2_2048 = km2_2022,
    km2_2049 = km2_2022,
    km2_2050 = km2_2022,
  )


# Scenario 2 Linear projection of total area ------------------------------


scenario2_linear <- tot_area_2022_2050_linear_pred


# Scenario 3 Constant BPC Scenario ----------------------------------------

scenario3_bpc_constant <- pop_2022_2050 %>%
  # Join BPC_2022 (in m^2/capita)
  left_join(AGR_bpc %>%
              select(kommunenummer, bpc_2022),
            by = "kommunenummer") %>%
  rowwise() %>%
  mutate(
    # Convert from m^2 to km^2
    bpc_2022_km2 = bpc_2022 / 1e6,
    
    # Multiply by each future population to get area in km^2
    km2_2022 = bpc_2022_km2 * pop2022,
    km2_2023 = bpc_2022_km2 * pop2023,
    km2_2024 = bpc_2022_km2 * pop2024,
    km2_2025 = bpc_2022_km2 * pop2025,
    km2_2026 = bpc_2022_km2 * pop2026,
    km2_2027 = bpc_2022_km2 * pop2027,
    km2_2028 = bpc_2022_km2 * pop2028,
    km2_2029 = bpc_2022_km2 * pop2029,
    km2_2030 = bpc_2022_km2 * pop2030,
    km2_2031 = bpc_2022_km2 * pop2031,
    km2_2032 = bpc_2022_km2 * pop2032,
    km2_2033 = bpc_2022_km2 * pop2033,
    km2_2034 = bpc_2022_km2 * pop2034,
    km2_2035 = bpc_2022_km2 * pop2035,
    km2_2036 = bpc_2022_km2 * pop2036,
    km2_2037 = bpc_2022_km2 * pop2037,
    km2_2038 = bpc_2022_km2 * pop2038,
    km2_2039 = bpc_2022_km2 * pop2039,
    km2_2040 = bpc_2022_km2 * pop2040,
    km2_2041 = bpc_2022_km2 * pop2041,
    km2_2042 = bpc_2022_km2 * pop2042,
    km2_2043 = bpc_2022_km2 * pop2043,
    km2_2044 = bpc_2022_km2 * pop2044,
    km2_2045 = bpc_2022_km2 * pop2045,
    km2_2046 = bpc_2022_km2 * pop2046,
    km2_2047 = bpc_2022_km2 * pop2047,
    km2_2048 = bpc_2022_km2 * pop2048,
    km2_2049 = bpc_2022_km2 * pop2049,
    km2_2050 = bpc_2022_km2 * pop2050
  ) %>%
  ungroup()



# Scenario 4 Growing (or shrinking) BPC -----------------------------------

scenario4_bpc_growing_full <- pop_2022_2050 %>%
  # Bring in the 2022 total area (km2_2022) plus the annual BPC growth rate (AGR_bpc)
  left_join(
    tot_area_2022_2050_linear_pred %>% select(kommunenummer, km2_2022),
    by = "kommunenummer"
  ) %>%
  left_join(
    AGR_bpc %>% select(kommunenummer, AGR_bpc),
    by = "kommunenummer"
  ) %>%
  rowwise() %>%
  mutate(
    # 1) BPC in 2022:
    bpc_2022 = km2_2022 / pop2022,   # in km² per person
    
    # 2) Grow BPC each year using last year's BPC:
    bpc_2023 = bpc_2022 * (1 + AGR_bpc / 100),
    bpc_2024 = bpc_2023 * (1 + AGR_bpc / 100),
    bpc_2025 = bpc_2024 * (1 + AGR_bpc / 100),
    bpc_2026 = bpc_2025 * (1 + AGR_bpc / 100),
    bpc_2027 = bpc_2026 * (1 + AGR_bpc / 100),
    bpc_2028 = bpc_2027 * (1 + AGR_bpc / 100),
    bpc_2029 = bpc_2028 * (1 + AGR_bpc / 100),
    bpc_2030 = bpc_2029 * (1 + AGR_bpc / 100),
    bpc_2031 = bpc_2030 * (1 + AGR_bpc / 100),
    bpc_2032 = bpc_2031 * (1 + AGR_bpc / 100),
    bpc_2033 = bpc_2032 * (1 + AGR_bpc / 100),
    bpc_2034 = bpc_2033 * (1 + AGR_bpc / 100),
    bpc_2035 = bpc_2034 * (1 + AGR_bpc / 100),
    bpc_2036 = bpc_2035 * (1 + AGR_bpc / 100),
    bpc_2037 = bpc_2036 * (1 + AGR_bpc / 100),
    bpc_2038 = bpc_2037 * (1 + AGR_bpc / 100),
    bpc_2039 = bpc_2038 * (1 + AGR_bpc / 100),
    bpc_2040 = bpc_2039 * (1 + AGR_bpc / 100),
    bpc_2041 = bpc_2040 * (1 + AGR_bpc / 100),
    bpc_2042 = bpc_2041 * (1 + AGR_bpc / 100),
    bpc_2043 = bpc_2042 * (1 + AGR_bpc / 100),
    bpc_2044 = bpc_2043 * (1 + AGR_bpc / 100),
    bpc_2045 = bpc_2044 * (1 + AGR_bpc / 100),
    bpc_2046 = bpc_2045 * (1 + AGR_bpc / 100),
    bpc_2047 = bpc_2046 * (1 + AGR_bpc / 100),
    bpc_2048 = bpc_2047 * (1 + AGR_bpc / 100),
    bpc_2049 = bpc_2048 * (1 + AGR_bpc / 100),
    bpc_2050 = bpc_2049 * (1 + AGR_bpc / 100)
  ) %>%
  # 3) Multiply BPC by population each year
  mutate(
    km2_2022 = bpc_2022 * pop2022,
    km2_2023 = bpc_2023 * pop2023,
    km2_2024 = bpc_2024 * pop2024,
    km2_2025 = bpc_2025 * pop2025,
    km2_2026 = bpc_2026 * pop2026,
    km2_2027 = bpc_2027 * pop2027,
    km2_2028 = bpc_2028 * pop2028,
    km2_2029 = bpc_2029 * pop2029,
    km2_2030 = bpc_2030 * pop2030,
    km2_2031 = bpc_2031 * pop2031,
    km2_2032 = bpc_2032 * pop2032,
    km2_2033 = bpc_2033 * pop2033,
    km2_2034 = bpc_2034 * pop2034,
    km2_2035 = bpc_2035 * pop2035,
    km2_2036 = bpc_2036 * pop2036,
    km2_2037 = bpc_2037 * pop2037,
    km2_2038 = bpc_2038 * pop2038,
    km2_2039 = bpc_2039 * pop2039,
    km2_2040 = bpc_2040 * pop2040,
    km2_2041 = bpc_2041 * pop2041,
    km2_2042 = bpc_2042 * pop2042,
    km2_2043 = bpc_2043 * pop2043,
    km2_2044 = bpc_2044 * pop2044,
    km2_2045 = bpc_2045 * pop2045,
    km2_2046 = bpc_2046 * pop2046,
    km2_2047 = bpc_2047 * pop2047,
    km2_2048 = bpc_2048 * pop2048,
    km2_2049 = bpc_2049 * pop2049,
    km2_2050 = bpc_2050 * pop2050
  ) %>%
  ungroup()


scenario4_bpc_growing <- scenario4_bpc_growing_full [ , c(1:2, 34:92)]

### input target municipality
target_mun<-"Oslo"

# Sum all area columns across municipalities
pop_totals <- fcKomPop %>%
  filter(KomNavn == target_mun) |> 
  summarize(across(starts_with("pop"), sum, na.rm = TRUE))


# Convert the wide data to long format for plotting
pop_total_long <- pop_totals %>%
  pivot_longer(
    cols = starts_with("pop"),
    names_to = "year",
    names_prefix = "pop",
    values_to = "Population"
  ) %>%
  mutate(
    year = as.integer(year),  # Convert year to numeric for plotting
    Type = ifelse(year <= 2022, "Måling", "Framskriving")  # Type for measured vs predicted
  )

# Reorder the Type factor
pop_total_long <- pop_total_long %>%
  mutate(Type = factor(Type, levels = c("Måling", "Framskriving")))  # Specify the desired order

# Create a separate dataset for the segment 2022-2023
segment_2022_2023 <- pop_total_long %>%
  filter(year >= 2022 & year <= 2023)

ylim1 <- max(pop_total_long$Population)

# Filter & pivot the measured data
one_muni_measured <- area_measured_and_estimated %>%
  filter(KomNavn == target_mun) %>%
  select(kommunenummer, KomNavn, starts_with("km2_")) %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  # convert "km2_2000" -> numeric year 2000
  mutate(year = as.numeric(sub("km2_", "", year_col))) %>%
  select(-year_col)

# Then label it as "Measured" in a scenario column
one_muni_measured <- one_muni_measured %>%
  mutate(scenario = "Måling (GLC)")

# Filter & pivot the net neutrality data

one_muni_neutral <- scenario1_net_neutral %>%
  filter(KomNavn == target_mun) %>%
  select(kommunenummer, KomNavn, starts_with("km2_")) %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  mutate(
    year = as.numeric(sub("km2_", "", year_col)),
    scenario = "Arealnøytral"
  ) %>%
  select(-year_col)


# Filter & pivot the historical trendline data
one_muni_trend <- area_trendline_2000_2022 %>%
  filter(KomNavn == target_mun) %>%
  select(kommunenummer, KomNavn, starts_with("km2_")) %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  mutate(
    year = as.numeric(sub("km2_", "", year_col)),
    scenario = "Historisk trend"
  ) %>%
  select(-year_col)

# Filter & pivot the future linear projection data
one_muni_linear <- tot_area_2022_2050_linear_pred %>%
  filter(KomNavn == target_mun) %>%
  pivot_longer(
    cols = starts_with("km2_20"),
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  mutate(
    year = as.numeric(sub("km2_", "", year_col)),
    scenario = "Ekstrapolering, areal"
  ) %>%
  select(-year_col) %>%
  # EXCLUDE 2022 from the final dataset
  filter(year >= 2023)


# Filter & pivot the linear projection scenario

one_muni_scen3 <- scenario3_bpc_constant %>%
  filter(KomNavn == target_mun) %>%
  pivot_longer(
    cols = starts_with("km2_20"), 
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  mutate(
    year = as.numeric(sub("km2_", "", year_col)),
    scenario = "Konstant BPC"
  ) %>%
  select(-year_col)


one_muni_scen4 <- scenario4_bpc_growing %>%
  filter(KomNavn == target_mun) %>%
  pivot_longer(
    cols = starts_with("km2_20"), 
    names_to = "year_col",
    values_to = "km2_area"
  ) %>%
  mutate(
    year = as.numeric(sub("km2_", "", year_col)),
    scenario = "Trend: befolkning og areal"
  ) %>%
  select(-year_col)

# Combine All into One Long Tibble
# Bind_rows all these single‐municipality data frame

one_muni_combined <- bind_rows(
  one_muni_measured,
  one_muni_trend,
  one_muni_neutral,
  one_muni_linear,
  one_muni_scen3,
  one_muni_scen4
)


# Plot
a <- ggplot(pop_total_long, aes(x = year, y = Population)) +
  # Entire line by Type
  geom_line(aes(color = Type), size = 0.6) +
  # Separate yellow line segment for 2022-2023
  geom_line(data = segment_2022_2023, aes(x = year, y = Population), color = "#E69F00", linewidth = 0.5) +
  # Points by Type
  geom_point(aes(color = Type), size = 0.7) +
  # Manual color scale
  scale_color_manual(values = c("Måling" = "#0072B2", "Framskriving" = "#E69F00")) +
  # Labels and theme
  labs(
    title = paste0("a) Befolkning, ",target_mun),
    x = "År",
    y = "Personer",
    color = "Data Type"  # Legend title
  ) +
  ylim(0, ylim1 +1000) +
  theme_minimal() +
  theme(
    legend.position.inside = c(0.78, 0.20),  # Place legend in the lower-right corner
    legend.background = element_rect(fill = "white", color = "grey"),  # Add white background with border
    legend.title = element_blank(),  # Suppress legend title
    plot.title = element_text(size = rel(0.96))  # Reduce title size to 70%
  )

# 1) Specify the desired order of scenarios (top to bottom in the legend).
scenario_order <- c(
  "Måling (GLC)",
  "Historisk trend",
  "Arealnøytral",
  "Ekstrapolering, areal",
  "Konstant BPC",
  "Trend: befolkning og areal"
)

a

# 2) Plot
b <- ggplot() +
  # A) Lines for all scenarios except "Measured":
  geom_line(
    data = subset(one_muni_combined, scenario != "Måling (GLC)"),
    aes(x = year, y = km2_area, color = scenario),
    size = 0.6
  ) +
  # B) Points ONLY for "Measured":
  geom_point(
    data = subset(one_muni_combined, scenario == "Måling (GLC)"),
    aes(x = year, y = km2_area, color = scenario),
    size = 0.7
  ) +
  
  # 3) Color scale: define the same breaks/order & pick colors
  scale_color_manual(
    breaks = scenario_order,
    values = c(
      "Måling (GLC)"            = "#0072B2",   # points only
      "Historisk trend"  = "#0072B2",   # line only
      "Arealnøytral"         = "#4daf4a",
      "Ekstrapolering, areal"              = "#E69F00",
      "Konstant BPC"        = "#d60066",
      "Trend: befolkning og areal"    = "grey16"
    )
  ) +
  # Lay out color legend in 2 columns
  guides(color = guide_legend(ncol = 2)) +
  
  # 4) Labels, axes, and theme
  labs(
    title = paste0("b) Utbygd areal,",target_mun),
    unique(one_muni_combined$KomNavn),
    x = "Year",
    y = "Totalt utbygd areal (km²)"
  ) +
  ylim(0, NA) +
  theme_minimal() +
  theme(
    legend.position    = c(0.6, 0.2),       # adjust as needed
    legend.background  = element_rect(fill = "white", color = "grey"),
    legend.title       = element_blank(),
    plot.title         = element_text(size = rel(1.0))
  )
b


#### Figure 2
categories <- data.frame(
  Arealbruk_per_innbygger = rep(c("Nedgang eller\nliten endring", "Moderat økning", "Sterk økning"), 3),
  Befolkningsendringer = c(rep("Nedgang", 3), rep("Stabil", 3), rep("Vekst", 3)),
  Descriptions = c("g)\n Ledig \nareal",
                   "h)\n Befolknings-\nnedgang og\narealstillstand",
                   "i)\n Befolknings-\nnedgang og\narealvekst",
                   "d)\n Stillstand i \nbefolkning \nog areal",
                   "e)\n Moderat \narealvekst",
                   "f)\n Arealekspansjon",
                   "a)\n Arealeffektiv \nvekst",
                   "b)\n Proporsjonal \nvekst",
                   "c)\n Rask \n arealekspansjon"),
  Colors = c("#cce8d7", "#cedced", "#fbb4d9", "#80c39b", "#85a8d0", "#f668b3", "#008837", "#0a50a1", "#d60066")
)


# Reorder the Type factor
categories <- categories %>%
  mutate(Arealbruk_per_innbygger = factor(Arealbruk_per_innbygger, levels = c("Nedgang eller\nliten endring", "Moderat økning", "Sterk økning")))  # Specify the desired order


# Define the plot
ggplot(categories, aes(x = Arealbruk_per_innbygger, y = Befolkningsendringer)) +
  geom_tile(aes(fill = Colors), color = "white", width = 0.9, height = 0.9) +
  scale_fill_identity() +  # Use exact colors as defined in Colors
  geom_text(aes(label = Descriptions), size = 3.5, color = "black") +  # Reduce text size in each block
  labs(x = "Arealbruk per innbygger", y = "Befolkningsendringer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 6),
        panel.grid = element_blank())  # Remove grid lines for cleaner look


# Define the plot
arealbruksfigur <- ggplot(categories, aes(x = Arealbruk_per_innbygger, y = Befolkningsendringer)) +
  geom_tile(aes(fill = Colors), color = "white", width = 0.9, height = 0.9) +
  scale_fill_identity() +  # Use exact colors as defined in Colors
  geom_text(aes(label = Descriptions), size = 3.5, color = "black") +  # Add descriptions in each block
  # Add thick black outline to the selected boxes (middle-left and bottom-left two)
  # geom_rect(
  #   data = categories[c(4, 7, 8), ],
  #   aes(xmin = as.numeric(factor(Arealbruk_per_innbygger)) - 0.45,
  #       xmax = as.numeric(factor(Arealbruk_per_innbygger)) + 0.45,
  #       ymin = as.numeric(factor(Befolkningsendringer, levels = rev(unique(Befolkningsendringer)))) - 0.45,
  #       ymax = as.numeric(factor(Befolkningsendringer, levels = rev(unique(Befolkningsendringer)))) + 0.45),
  #   color = "black",
  #   fill = NA,
  #   size = 1.5
  # ) +
  labs(x = "Arealbruk per innbygger", y = "Befolkningsendringer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid = element_blank())  # Remove grid lines for cleaner look

arealbruksfigur




# Scenario a) -------------------------------------------------------------


# Define data for the baseline rectangle (100% by 100%)
old_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Define data for the new rectangle (90% by 90%)
new_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 90, 90)   # y coordinates
)

# Create the plot
plot_g <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#cce8d7", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 100, y = 90), size = 3, color = "black") +    # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 80, label = "Utbygd~areal~T[1] == 90~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Ledig", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "utbygd areal", parse = FALSE, size = 6) +
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_g



# Scenario 2 --------------------------------------------------------------


# Define data for the baseline rectangle (100% by 100%)
old_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Define data for the new rectangle (90% by 110%)
new_rect <- data.frame(
  x = c(0, 111, 111, 0),  # x coordinates
  y = c(0, 0, 90, 90)     # y coordinates
)

# Create the plot
plot_h <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#cedced", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 111, y = 90), size = 3, color = "black") +   # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 80, label = "Utbygd~areal~T[1] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Befolknings-", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 45, label = "nedgang og", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = "arealstillstand", parse = FALSE, size = 6) +
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_h



# Scenario c --------------------------------------------------------------

# Define data for the baseline rectangle (100% by 100%)
old_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Define data for the new rectangle (120% by 90%)
new_rect <- data.frame(
  x = c(0, 120, 120, 0),  # x coordinates
  y = c(0, 0, 90, 90)     # y coordinates
)

# Create the plot for Scenario c
plot_i <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#fbb4d9", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 120, y = 90), size = 3, color = "black") +   # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 80, label = "Utbygd~areal~T[1] == 108~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Befolknings-", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 45, label = "nedgang og", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = "arealvekst", parse = FALSE, size = 6) +
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_i


# Scenario d, e and f -----------------------------------------------------


# Define data for the baseline rectangle (100% by 100%)
old_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Define data for the new rectangle (100% by 100%)
new_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Create the plot for Scenario d
plot_d <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#80c39b", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Top-right corner
  # Add text annotations
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[1] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Stillstand", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "i samlet", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = "utbygd areal", parse = FALSE, size = 6) +
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_d


# Scenario e --------------------------------------------------------------

# Define data for the new rectangle (110% by 100%)
new_rect <- data.frame(
  x = c(0, 110, 110, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Create the plot for Scenario e
plot_e <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#85a8d0", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 110, y = 100), size = 3, color = "black") +  # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[1] == 110~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Moderat", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "arealvekst", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = " ", parse = FALSE, size = 6) +
  # Customize axes
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_e


# Scenario f --------------------------------------------------------------

# Define data for the new rectangle (120% by 100%)
new_rect <- data.frame(
  x = c(0, 120, 120, 0),  # x coordinates
  y = c(0, 0, 100, 100)   # y coordinates
)

# Create the plot for Scenario f
plot_f <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#f668b3", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 120, y = 100), size = 3, color = "black") +  # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 105, label = "Utbygd~areal~T[1] == 120~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Areal-", parse = FALSE, size = 6) +
  #annotate("text", x = 50, y = 45, label = "Areal-", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 45, label = "ekspansjon", parse = FALSE, size = 6) +
  # Customize axes
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_f


# Scenario g --------------------------------------------------------------

# Define data for the new rectangle (90% by 110%)
new_rect <- data.frame(
  x = c(0, 100, 100, 0),  # x coordinates
  y = c(0, 0, 110, 110) # y coordinates
)

# Create the plot for Scenario g
plot_a <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#008837", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 100, y = 110), size = 3, color = "black") +   # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 118, label = "Utbygd~areal~T[1] == 110~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Arealeffektiv", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "vekst", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = " ", parse = FALSE, size = 6) +
  # Customize axes
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_a


# Scnario h ---------------------------------------------------------------

# Define data for the new rectangle (110% by 110%)
new_rect <- data.frame(
  x = c(0, 110, 110, 0),  # x coordinates
  y = c(0, 0, 110, 110)   # y coordinates
)

# Create the plot for Scenario h
plot_b <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#0a50a1", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 110, y = 110), size = 3, color = "black") +  # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 118, label = "Utbygd~areal~T[1] == 121~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Proporsjonal", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "vekst", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = " ", parse = FALSE, size = 6) +
  # Customize axes
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_b


# Scenario i --------------------------------------------------------------

# Define data for the new rectangle (120% by 110%)
new_rect <- data.frame(
  x = c(0, 120, 120, 0),  # x coordinates
  y = c(0, 0, 110, 110)   # y coordinates
)

# Create the plot for Scenario i
plot_c <- ggplot() +
  # Plot the new rectangle
  geom_polygon(data = new_rect, aes(x = x, y = y), fill = "#d60066", color = "black", lwd = 1.5) +
  # Plot the baseline rectangle
  geom_polygon(data = old_rect, aes(x = x, y = y), fill = "grey90", color = "black", alpha = 0.5) +
  # Add a point at the top-right corners
  geom_point(aes(x = 100, y = 100), size = 3, color = "black") +  # Baseline top-right
  geom_point(aes(x = 120, y = 110), size = 3, color = "black") +  # New top-right
  # Add text annotations
  annotate("text", x = 50, y = 92, label = "Utbygd~areal~T[0] == 100~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 118, label = "Utbygd~areal~T[1] == 132~'%'", parse = TRUE, size = 5) +
  annotate("text", x = 50, y = 55, label = "Rask", parse = TRUE, size = 6) +
  annotate("text", x = 50, y = 45, label = "areal-", parse = FALSE, size = 6) +
  annotate("text", x = 50, y = 35, label = "ekspansjon", parse = FALSE, size = 6) +
  # Customize axes
  # Customize axes
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 125),
    breaks = seq(0, 120, 20),
    labels = paste0(seq(0, 120, 20), "%")
  ) +
  # Add axis titles
  labs(
    x = "Utbygd landareal per innbygger (BPC)",
    y = "Befolkning"
  ) +
  # Customize theme
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Display the plot
plot_c



# Load necessary library
library(ggpubr)


# Arrange the three plots horizontally
combined_plots <- ggarrange(
  plot_a, plot_b, plot_c,
  plot_d, plot_e, plot_f,
  plot_g, plot_h, plot_i,
  ncol = 3, nrow = 3,  # Arrange in one row with three columns
  labels = NULL        # Disable automatic labeling
)

# Display the combined plot
print(combined_plots)

combined_plots2 <- ggarrange(
  plot_a, plot_b, plot_c,
  plot_d, plot_e, plot_f,
  plot_g, plot_h, plot_i,
  labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)", "h)", "i)"),
  ncol = 3, nrow = 3)

combined_plots2

# Adjust plot margins to create space for supercategories
combined_plots3 <- combined_plots2 + 
  theme(plot.margin = margin(t = 10, r = 10, b = 80, l = 80))  # Expand bottom and left margins

combined_plots3

# Overlay text annotations for the supercategories
combined_plots4 <- combined_plots3 +
  annotate("text", x = -0.05, y = 0.80, label = "Vekst", hjust = 0, size = 5, fontface = "bold.italic", angle = 90) +
  annotate("text", x = -0.05, y = 0.40, label = "  Stabil befolkning", hjust = 0, size = 5, fontface = "bold.italic", angle = 90) +
  annotate("text", x = -0.05, y = 0.10, label = "   Nedgang", hjust = 0, size = 5, fontface = "bold.italic", angle = 90) +
  
  annotate("text", x = 0.15, y = -0.04, label = "  Nedgang eller liten endring", hjust = 0.5, size = 5, fontface = "bold.italic") +
  annotate("text", x = 0.50, y = -0.04, label = "  Moderat økning", hjust = 0.5, size = 5, fontface = "bold.italic") +
  annotate("text", x = 0.85, y = -0.04, label = "  Sterk økning", hjust = 0.5, size = 5, fontface = "bold.italic")



combined_plots4

# Add category labels using `annotate_figure()`
combined_plots_annotated <- annotate_figure(
  combined_plots4,
  # Add the y-axis label (left side)
  
  left = text_grob("     Befolkningsendringer", size = 18, rot = 90, face = "bold"),
  # Add the x-axis label (bottom)
  bottom = text_grob("   Arealbruk per innbygger", size = 18, face = "bold")
)

combined_plots_annotated

