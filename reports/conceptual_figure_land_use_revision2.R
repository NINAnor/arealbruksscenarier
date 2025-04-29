# Preparations ------------------------------------------------------------

# Load necessary libraries 
library(tidyverse)
# library(sf)
# library(tmap)
# library(readxl)
# library(units)
# library(cowplot)
# library(ggrepel)
# library(scales)
# library(openxlsx)

# Language and notation settings
# Set locale and options
#Sys.setlocale("LC_CTYPE", "norwegian")
Sys.setlocale(locale='no_NB.utf8') 
options(scipen = 999)

# Use all available cores
library(parallel)
options(mc.cores = detectCores())

# Define the data for the plot
# Define the data for the plot
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

# Define output path
output_path <- file.path("P:/15220700_gis_samordning_2022_(marea_spare_ecogaps)/Trond/arealbruksscenarier/git/built_up_area_population/plots_and_tables", "arealbruksfigur_revised.png")

# Save the plot as high-resolution PNG with white background
ggsave(output_path, plot = arealbruksfigur, width = 16, height = 16, units = "cm", dpi = 600, bg = "white")


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

# # Define data for the new rectangle (90% by 110%)
# new_rect <- data.frame(
#   x = c(0, 90, 90, 0),  # x coordinates
#   y = c(0, 0, 110, 110) # y coordinates
# )


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


# Combine scenarios -------------------------------------------------------




# Load necessary library
library(ggpubr)

# Add annotations to each plot
# plot_a <- plot_a +
#   annotate("text", x = 5, y = 120, label = "a)", hjust = 0, size = 6)
# 
# plot_b <- plot_b +
#   annotate("text", x = 5, y = 120, label = "b)", hjust = 0, size = 6)
# 
# plot_c <- plot_c +
#   annotate("text", x = 5, y = 120, label = "c)", hjust = 0, size = 6)

# # Add annotations to each plot
# plot_d <- plot_d +
#   annotate("text", x = 5, y = 120, label = "d)", hjust = 0, size = 6)
# 
# plot_e <- plot_e +
#   annotate("text", x = 5, y = 120, label = "e)", hjust = 0, size = 6)
# 
# plot_f <- plot_f +
#   annotate("text", x = 5, y = 120, label = "f)", hjust = 0, size = 6)
# 
# # Add annotations to each plot
# plot_g <- plot_g +
#   annotate("text", x = 5, y = 120, label = "g)", hjust = 0, size = 6)
# 
# plot_h <- plot_h +
#   annotate("text", x = 5, y = 120, label = "h)", hjust = 0, size = 6)
# 
# plot_i <- plot_i +
#   annotate("text", x = 5, y = 120, label = "i)", hjust = 0, size = 6)

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

# Define output path
output_path <- file.path("P:/15220700_gis_samordning_2022_(marea_spare_ecogaps)/Trond/arealbruksscenarier/git/built_up_area_population/plots_and_tables", "arealbruksfigur_revised.png")
#output_path <- file.path("C:/Privat/data_analysis/land_use_simulations_norway/plots_and_tables", "arealbruksfigur7.png")


# Save the plot as high-resolution PNG with white background
ggsave(output_path, plot = combined_plots2, width = 33, height = 33, units = "cm", dpi = 600, bg = "white")


# adding "supercategories" ------------------------------------------------


# Load necessary packages
library(ggplot2)
library(ggpubr)

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


# Define output path
output_path <- file.path("P:/15220700_gis_samordning_2022_(marea_spare_ecogaps)/Trond/arealbruksscenarier/git/built_up_area_population/plots_and_tables", "arealbruksfigur_revised2.png")
#output_path <- file.path("C:/Privat/data_analysis/land_use_simulations_norway/plots_and_tables", "arealbruksfigur7.png")


# Save the plot as high-resolution PNG with white background
ggsave(output_path, plot = combined_plots_annotated, width = 33, height = 33, units = "cm", dpi = 600, bg = "white")

