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
OBJECTIVES_OF_INTEREST <- c("wt_int_tot", "wt_int_atk")

# Visual Thresholds
MIN_TRIP_DIFF <- 15 # Filter out changes smaller than +/- X trips

# Load Spatial Layers
layers <- load_all_spatial_layers()
study_area <- layers$study_area
drt_zones_sf <- layers$drt

####################
#  Load Data
####################

# 1. Load Iteration Differences (It1-Base, It2-Base)
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
      type = paste(iter_id, "(Diff vs Base)")
    )
}

diff_data <- map_dfr(ITERATIONS, load_best_diff) |> st_as_sf()

# 2. Load Baseline Absolute (From Baseline GTFS)
# We need to construct the baseline "Overline" if it doesn't exist in the same format.
# Usually process_gtfs_headways saves "gtfs_sf_headways_overline.rds" in output folder.
# We can grab it from iteration_01's folder since base processing happens there too.
base_path <- file.path(
  BASE_PATH,
  "iteration_01",
  "gtfs_sf_headways_overline.rds"
)

if (file.exists(base_path)) {
  base_data <- readRDS(base_path) |>
    filter(interval_label == "8-12") |>
    mutate(
      objective = "Base", # Dummy for joining
      iteration = "Baseline",
      type = "Baseline (Absolute)",
      num_trips_diff = num_trips, # Hack for plotting logic? No, keep separate.
      solution = "Base"
    )

  # duplicate base data for each objective so it appears in the grid rows
  base_data_expanded <- map_dfr(OBJECTIVES_OF_INTEREST, function(obj) {
    base_data |> mutate(objective = obj)
  }) |>
    st_as_sf()
} else {
  stop("Baseline overline file not found.")
}

# 3. Load DRT Fleet Info
drt_fleet <- map_dfr(ITERATIONS, function(iter) {
  path <- file.path(BASE_PATH, iter, "drt_fleet_deployments.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) |> mutate(iteration = iter)
})

# Prepare Fleet Data for coloring borders
fleet_join <- drt_fleet |>
  filter(
    objective %in% OBJECTIVES_OF_INTEREST,
    str_detect(solution, "_00$"),
    interval_label == "8-12"
  ) |>
  select(objective, iteration, scenario, fleet_size)


####################
#  Plotting Strategy
####################

# Strategy:
# 2. Plot Iteration Columns (Difference Scale)

# --- B. Comparison Maps (Iter 1 & 2) ---

# Filter small changes
plot_diff_sf <- diff_data |>
  filter(abs(num_trips_diff) >= MIN_TRIP_DIFF)

# Join DRT Fleet for proper filling
# Note: Join fleet_join (obj, iter, scenario) with drt_zones (scenario)
# BUT we need to replicate zones for every objective/iteration in fleet_join first.

drt_diff_layer <- drt_zones_sf |>
  cross_join(
    distinct(fleet_join, objective, iteration, scenario, fleet_size)
  ) |>
  # FIX: cross_join creates scenario.x (from zones) and scenario.y (from fleet)
  filter(scenario.x == scenario.y) |>
  rename(scenario = scenario.x) |>
  select(-scenario.y)

# Add column labels for faceting
plot_diff_sf <- plot_diff_sf |>
  mutate(
    col_label = if_else(
      iteration == "iteration_01",
      "Iteration 1 (Diff)",
      "Iteration 2 (Diff)"
    )
  )

drt_diff_sf <- drt_diff_layer |>
  mutate(
    col_label = if_else(
      iteration == "iteration_01",
      "Iteration 1 (Diff)",
      "Iteration 2 (Diff)"
    )
  )

tm_diff <- tm_shape(study_area) +
  tm_borders(alpha = 0.3) +
  tm_shape(study_area) +
  tm_polygons(fill = "gray20") +

  # 1. DRT Zones: Use tm_polygons to map border color (col), set fill to NA
  tm_shape(drt_diff_sf) +
  tm_polygons(
    col = "fleet_size", # Maps variable to border color
    fill = "fleet_size", # Transparent fill
    alpha = 0.5,
    lwd = 2,
    col.legend = tm_legend(title = "DRT Fleet Size")
  ) +

  # 2. Trip Differences
  tm_shape(plot_diff_sf) +
  tm_lines(
    col = "num_trips_diff",
    lwd = "num_trips",
    lwd.scale = tm_scale_continuous(values.scale = 10), # v4: Scale handled here
    col.scale = tm_scale_continuous(
      values = "brewer.rd_yl_bu",
      midpoint = 0
    ),
    col.legend = tm_legend(title = "Aggregate Headway \nChange vs Base"),
    lwd.legend = tm_legend_hide() # v4: Explicit function to hide legends
  ) +

  # 3. Facets & Layout
  tm_facets_grid(rows = "objective", columns = "col_label") +
  tm_title("Optimization Changes")


tm_diff
# --- C. Save ---

tmap_save(
  tm_diff,
  file.path(PLOT_DIR, "map_iteration_comparison_diffs.png"),
  width = 20,
  height = 16,
  units = "cm",
  dpi = 300
)


message("✓ Maps saved: comparison diffs and baseline reference.")
