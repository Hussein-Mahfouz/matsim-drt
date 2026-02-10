library(tidyverse)
library(sf)
library(tmap)
library(glue)

# Source helper functions
source("R/code/transit_opt/load_spatial_layers.R")

####################
#  Configuration
####################

ITERATIONS <- c("iteration_01", "iteration_02")
BASE_PATH <- "R/output"

# Output
PLOT_DIR <- "R/plots/transit_opt_paper/comparisons"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

# Objectives to plot
OBJECTIVES_OF_INTEREST <- c(
  "wt_int_tot",
  "wt_int_atk",
  "wt_avg_tot",
  "wt_avg_atk"
)

# Visual Thresholds
MIN_TRIP_DIFF <- 15

# Load Spatial Layers
layers <- load_all_spatial_layers()
study_area <- layers$study_area
drt_zones_sf <- layers$drt

####################
#  Load Data
####################

# 1. Load Iteration Differences
load_best_diff <- function(iter_id) {
  path <- file.path(BASE_PATH, iter_id, "gtfs_headway_comparisons_overline.rds")
  if (!file.exists(path)) {
    return(NULL)
  }

  readRDS(path) |>
    filter(
      objective %in% OBJECTIVES_OF_INTEREST,
      str_detect(solution, "_00_gtfs$"),
      interval_label == "8-12"
    ) |>
    mutate(
      iteration = iter_id,
      iter_num = str_extract(iteration, "\\d+")
    )
}

diff_data <- map_dfr(ITERATIONS, load_best_diff) |> st_as_sf()

# 2. Load DRT Fleet Info
drt_fleet <- map_dfr(ITERATIONS, function(iter) {
  path <- file.path(BASE_PATH, iter, "drt_fleet_deployments.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) |> mutate(iteration = iter)
})

fleet_join <- drt_fleet |>
  filter(
    objective %in% OBJECTIVES_OF_INTEREST,
    str_detect(solution, "_00$"),
    interval_label == "8-12"
  ) |>
  select(objective, iteration, scenario, fleet_size)


####################
#  Data Preparation
####################

# --- A. Vehicle Trips (Lines) ---
plot_diff_sf <- diff_data |>
  filter(abs(num_trips_diff) >= MIN_TRIP_DIFF) |>
  mutate(
    col_label = paste0("Iteration ", iter_num, " (Diff)")
  )

# --- B. DRT Fleet (Polygons) ---
# Fixed Levels and Palette (Grey for 0, Yellow->Red for active)
fleet_levels_num <- c(0, 10, 25, 50, 100, 150)
fleet_pal <- c("#999999", "#fed976", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026") # Grey -> Yellow -> Red

drt_diff_layer <- drt_zones_sf |>
  cross_join(distinct(
    fleet_join,
    objective,
    iteration,
    scenario,
    fleet_size
  )) |>
  filter(scenario.x == scenario.y) |>
  rename(scenario = scenario.x) |>
  select(-scenario.y) |>
  mutate(
    fleet_label = factor(fleet_size, levels = fleet_levels_num),
    iter_num = str_extract(iteration, "\\d+"),
    col_label = paste0("Iteration ", iter_num, " (Diff)")
  )

# --- C. Align Factors for Faceting ---
# Crucial Step: Ensure facet columns are factors with same levels across datasets
# otherwise tmap might fail to filter the secondary layer correctly.
all_objs <- unique(c(plot_diff_sf$objective, drt_diff_layer$objective))
all_cols <- unique(c(plot_diff_sf$col_label, drt_diff_layer$col_label))

plot_diff_sf <- plot_diff_sf |>
  mutate(
    objective = factor(objective, levels = all_objs),
    col_label = factor(col_label, levels = all_cols)
  )

drt_diff_layer <- drt_diff_layer |>
  mutate(
    objective = factor(objective, levels = all_objs),
    col_label = factor(col_label, levels = all_cols)
  )


####################
#  Plotting
####################

# Dynamic width
n_cols <- length(levels(plot_diff_sf$col_label))
plot_width <- 5 + (n_cols * 8)

tm_diff <- tm_shape(study_area) +
  tm_polygons(fill = "gray20", col_alpha = 0) +

  # 1. Bus Frequency Change (Lines)
  tm_shape(plot_diff_sf) +
  tm_lines(
    col = "num_trips_diff",
    lwd = "num_trips",
    lwd.scale = tm_scale_continuous(values.scale = 4),
    col.scale = tm_scale_continuous(values = "brewer.rd_yl_bu", midpoint = 0),
    col.legend = tm_legend(title = "Trip Change\n(vs Base)"),
    lwd.legend = tm_legend_hide()
  ) +

  # # 2. DRT Zones (Borders Only)
  # # Using tm_borders ensures we map color to the line, not fill
  # tm_shape(drt_diff_layer) +
  # tm_borders(
  #   col = "fleet_label", # Map variable to border color
  #   lwd = 3, # Thicker lines
  #   col.scale = tm_scale_categorical(values = fleet_pal),
  #   col.legend = tm_legend(title = "DRT Fleet Size")
  # ) +
  # # 2. DRT Zones (Filled Polygons)
  # tm_shape(drt_diff_layer) +
  # tm_polygons(
  #   fill = "fleet_label", # Map variable to fill color
  #   col = "white", # Border color (constant, thin)
  #   lwd = 1,
  #   fill.scale = tm_scale_categorical(values = fleet_pal),
  #   fill.legend = tm_legend(title = "DRT Fleet Size"),
  #   fill_alpha = 0.4 # Transparency to see background/context
  # ) +

  # 3. Facets & Layout
  tm_facets_grid(rows = "objective", columns = "col_label") +
  tm_title("Optimization Changes vs Baseline") +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "left",
    panel.label.fontface = "bold"
  )

tm_diff
# --- Save ---
tmap_save(
  tm_diff,
  file.path(PLOT_DIR, "map_iteration_comparison_diffs_pt_only.png"),
  width = plot_width,
  height = 16,
  units = "cm",
  dpi = 300
)

message("✓ Comparison map saved.")

# ------------------------------------------------------------------
# ANALYSIS: SERVICE ADDITIONS PLOT (Positive Changes)
# ------------------------------------------------------------------

# 1. Prepare Data
# A. Context Layer: Baseline network (all links) for background reference
# We reuse diff_data but ensure it has the facet columns
network_context_sf <- diff_data |>
  mutate(
    col_label = paste0("Iteration ", iter_num, " (Diff)"),
    # Match factors to ensure it appears in the correct grid panel
    objective = factor(objective, levels = levels(plot_diff_sf$objective)),
    col_label = factor(col_label, levels = levels(plot_diff_sf$col_label))
  )

# B. Highlights Layer: Only Service Additions (> 0)
plot_additions_sf <- network_context_sf |>
  filter(num_trips_diff > 0)

# 2. Create Plot
tm_additions <- tm_shape(study_area) +
  tm_polygons(fill = "gray20", col_alpha = 0) +

  # Layer 1: Context Network (Faint Grey Lines)
  tm_shape(network_context_sf) +
  tm_lines(
    col = "white",
    lwd = 0.15,
    col_alpha = 0.1 # Fixed: Changed from alpha to col_alpha per previous warning
  ) +

  # Layer 4: Service Additions (Bright Lines on Top)
  tm_shape(plot_additions_sf) +
  tm_lines(
    col = "num_trips_diff",
    lwd = "num_trips",
    lwd.scale = tm_scale_continuous(values.scale = 4),

    # FIX: Changed "mpl.plasma" to "plasma"
    col.scale = tm_scale_continuous(values = "YlGnBu"),
    col.legend = tm_legend(title = "Service Added\n(Trips vs Base)"),
    lwd.legend = tm_legend_hide()
  ) +

  # # Layer 3: DRT Zones (Filled Polygons)
  # tm_shape(drt_diff_layer) +
  # tm_polygons(
  #   fill = "fleet_label",
  #   col = "white",
  #   lwd = 1,
  #   fill.scale = tm_scale_categorical(values = fleet_pal),
  #   fill.legend = tm_legend(title = "DRT Fleet Size"),
  #   fill_alpha = 0.3
  # ) +

  # Facets & Layout
  tm_facets_grid(rows = "objective", columns = "col_label") +
  tm_title("Service Additions vs Baseline") +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "left",
    panel.label.fontface = "bold"
  )

tm_additions

# 3. Save
tmap_save(
  tm_additions,
  file.path(PLOT_DIR, "map_iteration_comparison_additions_pt_only.png"),
  width = plot_width,
  height = 16,
  units = "cm",
  dpi = 300
)

message("✓ Service Additions map saved.")
