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
