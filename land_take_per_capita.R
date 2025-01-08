# Load necessary libraries
library(tidyverse)
library(sf)
library(tmap)
library(readxl)
library(units)
library(cowplot)

# Set locale and options
#Sys.setlocale("LC_CTYPE", "norwegian")
Sys.setlocale(locale='no_NB.utf8') 
options(scipen = 999)
main_path <- "C:/Users/reto.spielhofer/OneDrive - NINA/Documents/Projects/APPLICATIONS/2025/UPLAND_2/test_site_selection/DATA_TROND"

shp_path <- paste0(main_path,"/2023/kommuner_2023_land.shp")
kommuner_land <- st_read(shp_path) |> st_transform(25833)
#plot(kommuner_land$geometry)
qtm(kommuner_land)


# Define the path to the geodatabase
fgdb_path <- paste0(main_path,"/Data2Trond.gdb")


# List all layers in geodatabase
#st_layers(gdb_path)
st_layers(fgdb_path, options = character(0), do_count = FALSE)

fcKomPop <- st_read(fgdb_path, layer = "fcKomPop") |> st_drop_geometry()
pop <- fcKomPop |> select(kommunenummer, KomNavn, bef2000, bef2022, est2050) |> rename(bef2050 = est2050)
plot(pop)

## global landcover per municipality
fcKomGLCkm2 <- st_read(fgdb_path, layer = "fcKomGLCkm2") |> st_drop_geometry()

areal <- fcKomGLCkm2 |> select(kommunenummer, KomNavn, intercept, slope, ImpAreaKm2_2000, ImpAreaKm2_2022, eImpAreaKm2_2050) |> 
  rename(Km2_2000 = ImpAreaKm2_2000, Km2_2022 = ImpAreaKm2_2022, Km2_2050 = eImpAreaKm2_2050)

plot(areal)

kommunevariabler <- st_read(fgdb_path, layer = "fcKomBeskrivelse") |> st_drop_geometry()
plot(kommunevariabler)
#correlations <- cor(kommunevariabler [,4:13])
#corrplot::corrplot(correlations, method = "number")

head(fcKomPop)
glimpse(fcKomPop)
#plot(fcKomPop$SHAPE)

# kommuner_geometry <- st_read("C:/Privat/spatial_trond/fylker_kommuner/2024/fylker_kommuner_klippet_etter_kyst/kommuner.gpkg") |> st_transform(25833)
# plot(kommuner_geometry$geom)
plot(kommunevariabler$rUrbant~kommunevariabler$SSB_Sentralitet)

fcKomGLCkm2

# library(ggplot2)
# library(ggrepel)
# 
# library(ggplot2)
# library(dplyr)
# library(tidyr)

fcKomGLCkm2_df <- fcKomGLCkm2 |> st_drop_geometry()
glimpse(fcKomGLCkm2_df)

# Fit a linear model for each municipality for the period 2000-2022 and add new columns intercept2 and slope2
fcKomGLCkm2_df <- fcKomGLCkm2_df %>%
  rowwise() %>%
  mutate(
    # Extract the area values and years for the period 2000 to 2022
    lm_data = list(tibble(
      year = 2000:2022,
      area = c_across(ImpAreaKm2_2000:ImpAreaKm2_2022)  # Explicitly specify the columns from 2000 to 2022
    )),
    # Fit the linear model and extract coefficients
    lm_fit = list(lm(area ~ year, data = lm_data)),
    intercept2 = coef(lm_fit)[1],
    slope2 = coef(lm_fit)[2]
  ) %>%
  ungroup() %>%
  select(-lm_data, -lm_fit)  # Remove temporary columns

# View the updated data frame
glimpse(fcKomGLCkm2_df)
# cor(fcKomGLCkm2_df$intercept, fcKomGLCkm2_df$intercept2)
# cor(fcKomGLCkm2_df$slope, fcKomGLCkm2_df$slope2)
# plot(fcKomGLCkm2_df$intercept, fcKomGLCkm2_df$intercept2)
# plot(fcKomGLCkm2_df$slope, fcKomGLCkm2_df$slope)

# Calculate estimated area values for each year from 2000 to 2022 using intercept2 and slope2
fcKomGLCkm2_df <- fcKomGLCkm2_df %>%
  mutate(
    est_ImpAreaKm2_2000 = intercept2 + slope2 * 2000,
    est_ImpAreaKm2_2001 = intercept2 + slope2 * 2001,
    est_ImpAreaKm2_2002 = intercept2 + slope2 * 2002,
    est_ImpAreaKm2_2003 = intercept2 + slope2 * 2003,
    est_ImpAreaKm2_2004 = intercept2 + slope2 * 2004,
    est_ImpAreaKm2_2005 = intercept2 + slope2 * 2005,
    est_ImpAreaKm2_2006 = intercept2 + slope2 * 2006,
    est_ImpAreaKm2_2007 = intercept2 + slope2 * 2007,
    est_ImpAreaKm2_2008 = intercept2 + slope2 * 2008,
    est_ImpAreaKm2_2009 = intercept2 + slope2 * 2009,
    est_ImpAreaKm2_2010 = intercept2 + slope2 * 2010,
    est_ImpAreaKm2_2011 = intercept2 + slope2 * 2011,
    est_ImpAreaKm2_2012 = intercept2 + slope2 * 2012,
    est_ImpAreaKm2_2013 = intercept2 + slope2 * 2013,
    est_ImpAreaKm2_2014 = intercept2 + slope2 * 2014,
    est_ImpAreaKm2_2015 = intercept2 + slope2 * 2015,
    est_ImpAreaKm2_2016 = intercept2 + slope2 * 2016,
    est_ImpAreaKm2_2017 = intercept2 + slope2 * 2017,
    est_ImpAreaKm2_2018 = intercept2 + slope2 * 2018,
    est_ImpAreaKm2_2019 = intercept2 + slope2 * 2019,
    est_ImpAreaKm2_2020 = intercept2 + slope2 * 2020,
    est_ImpAreaKm2_2021 = intercept2 + slope2 * 2021,
    est_ImpAreaKm2_2022 = intercept2 + slope2 * 2022
  )

# View the updated data frame with new columns
glimpse(fcKomGLCkm2_df)


# -------------------------------------------------------------------------

# Reshape the regression estimates to long format for plotting
fcKomGLCkm2_long <- fcKomGLCkm2_df %>%
  select(kommunenummer, KomNavn, starts_with("ImpAreaKm2_")) %>%
  pivot_longer(
    cols = starts_with("ImpAreaKm2_"),
    names_to = "year",
    names_prefix = "ImpAreaKm2_",
    values_to = "km2_GLC"
  ) %>%
  mutate(year = as.integer(year))

#glimpse(fcKomGLCkm2_df)

# Plotting
ggplot(fcKomGLCkm2_long, aes(x = year, y = km2_GLC, group = KomNavn, color = KomNavn)) +
  geom_line() +  # Actual development
  geom_line(data = fcKomGLCkm2_regression, aes(y = km2_estimated), linetype = "dashed") +  # Regression estimates
  labs(
    title = "Utvikling over tid per kommune",
    x = "År",
    y = "Areal (km²)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +  # Hide legend for an uncluttered plot
  geom_text(
    data = fcKomGLCkm2_long %>% group_by(KomNavn) %>% slice_max(year),
    aes(label = KomNavn),
    hjust = 0,
    vjust = 0.5
  )  # Add labels at the end of lines

# Reshape the regression estimates to long format for plotting
fcKomGLCkm2_regression <- fcKomGLCkm2_df %>%
  select(kommunenummer, KomNavn, starts_with("est_ImpAreaKm2_")) %>%
  pivot_longer(
    cols = starts_with("est_ImpAreaKm2_"),
    names_to = "year",
    names_prefix = "est_ImpAreaKm2_",
    values_to = "km2_estimated"
  ) %>%
  mutate(year = as.integer(year))

ggplot(fcKomGLCkm2_long, aes(x = year, y = km2, group = KomNavn, color = KomNavn)) +
  #geom_line() +  # Actual development
  geom_line(data = fcKomGLCkm2_regression, aes(y = km2_estimated), linetype = "dashed") +  # Regression estimates
  labs(
    title = "Utvikling over tid per kommune",
    x = "År",
    y = "Areal (km²)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +  # Hide legend for an uncluttered plot
  geom_text(
    data = fcKomGLCkm2_long %>% group_by(KomNavn) %>% slice_max(year),
    aes(label = KomNavn),
    hjust = 0,
    vjust = 0.5
  )  # Add labels at the end of lines



# Entire period 2000 to 2050

library(tidyverse)

# Rename columns to `km2_year` format
fcKomGLCkm2_df <- fcKomGLCkm2_df %>%
  rename_with(~ str_replace_all(., "^eImpAreaKm2_", "km2_"), starts_with("eImpAreaKm2_")) %>%
  rename_with(~ str_replace_all(., "^est_ImpAreaKm2_", "km2_"), starts_with("est_ImpAreaKm2_"))

# Pivot the data to long format for plotting
fcKomGLCkm2_long <- fcKomGLCkm2_df %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year",
    names_prefix = "km2_",
    names_transform = list(year = as.integer),
    values_to = "km2"
  )

# Plot the data
ggplot(fcKomGLCkm2_long, aes(x = year, y = km2, group = KomNavn, color = KomNavn)) +
  geom_line() +
  labs(
    title = "Utvikling over tid per kommune",
    x = "År",
    y = "Areal (km²)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(
    data = fcKomGLCkm2_long %>% group_by(KomNavn) %>% slice_max(year),
    aes(label = KomNavn),
    hjust = 0,
    vjust = 0.5
  )  # Add labels at the end of lines




# -------------------------------------------------------------------------




# 
# fcKomGLCkm2_long
# 
# # Filter data for the years 2000 and 2022
# fcKomGLCkm2_filtered <- fcKomGLCkm2_long %>%
#   filter(year %in% c(2000, 2022))
# 
# # Plotting
# ggplot(fcKomGLCkm2_filtered, aes(x = year, y = km2, group = KomNavn, color = KomNavn)) +
#   geom_line() +
#   labs(
#     title = "Arealutvikling for 2000 og 2022 per kommune",
#     x = "År",
#     y = "Areal (km²)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +  # Hide legend to reduce clutter
#   geom_text(
#     data = fcKomGLCkm2_filtered %>% group_by(KomNavn) %>% filter(year == 2022), 
#     aes(label = KomNavn), 
#     hjust = 0, 
#     vjust = 0.5
#   )  # Add labels at the end of each line


# Define keywords for municipalities of interest
municipalities_keywords <- "Oslo|Bergen|Trondheim|Bodø|Kristiansand|Tromsø|Ringsaker"
municipalities_keywords <- "Tolga|Røros|Alvdal|Øyer|Lom|Ringsaker|Vågå|Vinje"


# Filter data for the years 2000 and 2022, and municipalities containing specified keywords
fcKomGLCkm2_selected <- fcKomGLCkm2_long %>%
  filter(
    year %in% c(2000, 2022),
    str_detect(KomNavn, municipalities_keywords)
  )

# Plotting
ggplot(fcKomGLCkm2_selected, aes(x = year, y = km2, group = KomNavn, color = KomNavn)) +
  geom_line() +
  labs(
    title = "Arealutvikling for 2000 og 2022 i utvalgte kommuner",
    x = "År",
    y = "Areal (km²)"
  ) +
  theme_minimal() +
  geom_text(
    data = fcKomGLCkm2_selected %>% filter(year == 2022), 
    aes(label = KomNavn), 
    hjust = 0, 
    vjust = 0.5
  )  # Add labels at the end of each line


kommuner_land <- as_tibble(kommuner_land)
fcKomGLCkm2 <- as_tibble(fcKomGLCkm2)
fcKomPop <- as_tibble(fcKomPop)
komm_var <- as_tibble(kommunevariabler)

kommuner_land
fcKomGLCkm2 
fcKomPop 
komm_var 

# Rename columns starting with "est20" to "bef20"
names(fcKomPop) <- sub("^est20", "bef20", names(fcKomPop))
names(fcKomPop) <- sub("^bef", "pop", names(fcKomPop))
fcKomPop

# Rename columns starting with "est20" to "bef20"
pop <- fcKomPop |> select(kommunenummer, KomNavn, pop2000, pop2022, pop2034, pop2050) 
pop <- pop |> mutate(pop_change_2000_2022 = pop2022 - pop2000)
pop <- pop |> mutate(pop_change_2022_2034 = pop2034 - pop2022)
pop <- pop |> mutate(pop_change_2022_2050 = pop2050 - pop2022)
pop

plot(sort(pop$pop_change_2000_2022))

fcKomGLCkm2

area <- fcKomGLCkm2 |> select(kommunenummer, KomNavn, estImpAreaKm2_2000, estImpAreaKm2_2022, 
                              eImpAreaKm2_2034, eImpAreaKm2_2050) 

names(area) <- sub("^ImpAreaKm2", "km2", names(area))
names(area) <- sub("^eImpAreaKm2", "km2", names(area))
area

area <- area |> mutate(km2_change_2000_2022 = km2_2022 - km2_2000)
area <- area |> mutate(km2_change_2022_2034 = km2_2034 - km2_2022)
area <- area |> mutate(km2_change_2022_2050 = km2_2050 - km2_2022)

plot(sort(area$km2_change_2000_2022))

# Assuming 'area' is your tibble
area_long <- area %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year",
    names_prefix = "km2_",
    values_to = "km2"
  ) %>%
  mutate(year = as.integer(year))  # Convert year to numeric for plotting

# Plot
ggplot(area_long, aes(x = year, y = km2, group = kommunenummer, color = KomNavn)) +
  geom_line() +
  geom_point() +
  labs(title = "Change in Area Over Time for Each Municipality", x = "Year", y = "Area (km²)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for readability, as there are many municipalities


# Calculate estimates for 2000 and 2022
est_area <- area %>%
  mutate(
    lm_km2_2000 = (intercept + slope * 2000)*0.001,
    lm_km2_2022 = (intercept + slope * 2022)*0.001
  )

# View the updated table
glimpse(est_area)


# Assuming 'area' is your tibble
est_area_long <- est_area %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year",
    names_prefix = "km2_",
    values_to = "km2"
  ) %>%
  mutate(year = as.integer(year))  # Convert year to numeric for plotting

# Plot
ggplot(est_area_long, aes(x = year, y = km2, group = kommunenummer, color = KomNavn)) +
  geom_line() +
  geom_point() +
  labs(title = "Change in Area Over Time for Each Municipality", x = "Year", y = "Area (km²)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for readability, as there are many municipalities

library(tidyverse)
library(ggrepel)

# Assuming 'est_area' is already created and reshaped into 'est_area_long' format
est_area_long <- est_area %>%
  pivot_longer(
    cols = starts_with("km2_"),
    names_to = "year",
    names_prefix = "km2_",
    values_to = "km2"
  ) %>%
  mutate(year = as.integer(year))  # Convert year to numeric for plotting

# Identify the latest year for labeling
latest_year <- max(est_area_long$year)

# Plot with labels
ggplot(est_area_long, aes(x = year, y = km2, group = kommunenummer, color = KomNavn)) +
  geom_line() +
  geom_point() +
  geom_text_repel(
    data = est_area_long %>% filter(year == latest_year),
    aes(label = KomNavn),
    nudge_x = 5,  # Shift labels slightly to the right
    direction = "y",
    hjust = 0,
    segment.color = "grey50",
    segment.size = 0.2
  ) +
  labs(title = "Change in Area Over Time for Each Municipality", x = "Year", y = "Area (km²)") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for readability, as there are many municipalities




