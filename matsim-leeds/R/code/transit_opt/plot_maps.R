library(tidyverse)
library(sf)
library(tidytransit)
library(tmap)

# Source helper functions
source("R/code/transit_opt/load_spatial_layers.R")
source("R/code/transit_opt/gtfs_headway_analysis.R")

####################
#  Configuration
####################

ITERATION_ID <- "iteration_00" # Or "iteration_02"

# Paths
# Input directory: where the R/output/iteration_XX files are
input_dir <- file.path("R/output", ITERATION_ID)

# Output directory: where plots will be saved
output_dir <- file.path("R/plots/transit_opt_paper", ITERATION_ID)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

INCLUDE_DRT_FLEET <- TRUE

####################
#  Load Spatial Layers
####################

message("Loading spatial layers...")
layers <- load_all_spatial_layers()
study_area <- layers$study_area
basemap_urban_rural <- layers$basemap_urban_rural
drt <- layers$drt


####################
#  Load Pre-processed Data
####################

message("Loading pre-processed GTFS headway data...")

gtfs_sf_headways_overline <- readRDS(
  file.path(input_dir, "gtfs_sf_headways_overline.rds")
)
all_solutions_overline <- readRDS(
  file.path(input_dir, "gtfs_headway_comparisons_overline.rds")
)
all_drt_deployments <- read_csv(
  file.path(input_dir, "drt_fleet_deployments.csv"),
  show_col_types = FALSE
)


# Replace 0 fleet size with 25
all_drt_deployments <- all_drt_deployments |>
  mutate(fleet_size = if_else(fleet_size == 0, 25, fleet_size))

####################
#  Configuration - Filter Objectives
####################

# ===== USER CONFIGURATION =====
# Objective filtering (Same as evaluate_objectives.R)
OBJECTIVES_TO_INCLUDE <- NULL
OBJECTIVES_TO_EXCLUDE <- "^sc_|_var$|_sum_"

if (!is.null(OBJECTIVES_TO_INCLUDE)) {
  message(glue::glue(
    "Filtering to include only: {paste(OBJECTIVES_TO_INCLUDE, collapse = ', ')}"
  ))
  all_solutions_overline <- all_solutions_overline |>
    filter(objective %in% OBJECTIVES_TO_INCLUDE)
  all_drt_deployments <- all_drt_deployments |>
    filter(objective %in% OBJECTIVES_TO_INCLUDE)
} else if (!is.null(OBJECTIVES_TO_EXCLUDE)) {
  message(glue::glue("Filtering to exclude: {OBJECTIVES_TO_EXCLUDE}"))
  all_solutions_overline <- all_solutions_overline |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
  all_drt_deployments <- all_drt_deployments |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
}

message(glue::glue(
  "Objectives in map analysis: {paste(unique(all_solutions_overline$objective), collapse = ', ')}"
))

####################
#  Helper: Create Base Map Layers
####################

#' Create common base map layers (study area + basemap)
#'
#' @return tmap layers for study area and basemap
#'
create_base_layers <- function() {
  tm_shape(study_area) +
    tm_borders(lwd = 1.5, col = "black") +
    tm_shape(basemap_urban_rural) +
    tm_fill(
      fill = "RUC11",
      fill.scale = tm_scale_categorical(values = "brewer.greys"),
      fill.legend = tm_legend(title = "Urbanisation"),
      fill_alpha = 0.35
    )
}

#' Create DRT zone layer with optional fleet size coloring
#'
#' @param drt_zones sf object with DRT zones
#' @param drt_deployments DRT deployment data (optional)
#' @param objective Filter to objective (optional)
#' @param solution Filter to solution (optional)
#' @param interval_label Filter to interval (optional)
#' @param show_fleet_size If TRUE, color by fleet size
#'
#' @return tmap layer for DRT zones
#'
create_drt_layer <- function(
  drt_zones,
  drt_deployments = NULL,
  objective = NULL,
  solution = NULL,
  interval_label = NULL,
  show_fleet_size = FALSE
) {
  if (show_fleet_size && !is.null(drt_deployments)) {
    # Join fleet data to zones
    drt_with_fleet <- join_drt_fleet_to_zones(
      drt_zones = drt_zones,
      drt_deployments = drt_deployments,
      objective = objective,
      solution = solution,
      interval_label = interval_label
    )

    # Get unique fleet sizes for discrete scale
    fleet_levels <- sort(unique(drt_deployments$fleet_size))

    # Use categorical scale for discrete fleet sizes
    tm_shape(drt_with_fleet) +
      tm_polygons(
        fill = "fleet_size",
        fill.scale = tm_scale_categorical(
          values = "brewer.oranges",
          levels = fleet_levels,
          labels = paste(fleet_levels, "vehicles") # Better labels
        ),
        fill.legend = tm_legend(title = "DRT Fleet Size"),
        fill_alpha = 0.6,
        col = "#d95f02",
        lwd = 2
      )
  } else {
    # Simple border only
    tm_shape(drt_zones) +
      tm_borders(col = "#d95f02", lwd = 2, lty = "solid") +
      tm_add_legend(
        type = "lines",
        labels = "DRT Zones",
        col = "#d95f02",
        lwd = 2,
        title = ""
      )
  }
}

#' Create PT network layer
#'
#' @param pt_data sf object with PT network
#' @param col_var Variable for line color (NULL for fixed color)
#' @param lwd_var Variable for line width (NULL for fixed width)
#' @param fixed_col Fixed color if col_var is NULL
#' @param show_diff If TRUE, use diverging color scale centered at 0
#'
#' @return tmap layer for PT network
#'
create_pt_layer <- function(
  pt_data,
  col_var = NULL,
  lwd_var = "num_trips",
  fixed_col = "#1b9e77",
  show_diff = FALSE,
  lwd_scale = 5
) {
  if (!is.null(col_var) && show_diff) {
    # Diverging color scale for differences
    tm_shape(pt_data) +
      tm_lines(
        col = col_var,
        col.scale = tm_scale_continuous(
          values = "brewer.rd_yl_gn",
          midpoint = 0
        ),
        col.legend = tm_legend(title = "Trip Difference\n(vs Base)"),
        lwd = lwd_var,
        lwd.scale = tm_scale_continuous(values.scale = lwd_scale),
        lwd.legend = tm_legend(show = FALSE)
      )
  } else if (!is.null(col_var)) {
    # Sequential color scale
    tm_shape(pt_data) +
      tm_lines(
        col = col_var,
        col.scale = tm_scale_continuous(values = "brewer.greens"),
        col.legend = tm_legend(title = col_var),
        lwd = lwd_var,
        lwd.scale = tm_scale_continuous(values.scale = lwd_scale),
        lwd.legend = tm_legend(show = FALSE)
      )
  } else {
    # Fixed color
    tm_shape(pt_data) +
      tm_lines(
        col = fixed_col,
        lwd = lwd_var,
        lwd.scale = tm_scale_continuous(values.scale = lwd_scale),
        lwd.legend = tm_legend(title = "Bus Trips")
      )
  }
}

####################
#  Plot 1: Study Area Overview (Base Network + DRT Zones)
####################

message("\nCreating Plot 1: Study area overview...")

pt_data_base <- gtfs_sf_headways_overline |> filter(interval_label == "8-12")

plot1 <- create_base_layers() +
  create_drt_layer(drt, show_fleet_size = FALSE) +
  create_pt_layer(pt_data_base, lwd_var = "num_trips", lwd_scale = 10) +
  tm_title("Study Area: Existing Bus Network and Proposed DRT Zones") +
  tm_layout(frame = FALSE)
plot1
tmap_save(
  plot1,
  file.path(output_dir, "01_study_area_overview.png"),
  width = 16,
  height = 11,
  units = "cm",
  dpi = 600
)

####################
#  Plot 2: Best Solutions Comparison (Faceted by Objective)
####################

message("Creating Plot 2: Best solutions comparison...")

best_pt_data <- all_solutions_overline |>
  filter(
    str_detect(solution, "_00_gtfs$"),
    interval_label == "8-12"
  )

plot2 <- tm_shape(study_area) +
  tm_borders(lwd = 0.5, col = "gray50") +
  tm_shape(study_area) +
  tm_polygons(fill = "gray20") +
  tm_shape(best_pt_data |> filter(num_trips_diff > 20 | num_trips_diff < -20)) +
  tm_lines(
    col = "num_trips_diff",
    col.scale = tm_scale_continuous(
      values = "brewer.rd_yl_gn",
      midpoint = 0
    ),
    col.legend = tm_legend(title = "Trip Diff"),
    lwd = 2,
  ) +
  tm_facets_wrap(by = "objective", ncol = 3) +
  tm_title("Bus Trip Changes (8am-12pm) - Best Solution per Objective") +
  tm_layout(frame = FALSE)

plot2


tmap_save(
  plot2,
  file.path(output_dir, "02_best_solutions_by_objective.png"),
  width = 24,
  height = 16,
  units = "cm",
  dpi = 300
)

####################
#  Plot 3: Single Objective Detailed View (with optional DRT fleet)
####################

message("Creating Plot 3: Single objective detailed view...")

selected_objective <- "wt_avg_atk"
selected_solution <- "combined_solution_00"
selected_interval <- "8-12"

single_pt_data <- all_solutions_overline |>
  filter(
    objective == selected_objective,
    solution == paste0(selected_solution, "_gtfs"),
    interval_label == selected_interval
  )

# Without DRT fleet
plot3a <- create_base_layers() +
  create_drt_layer(drt, show_fleet_size = FALSE) +
  create_pt_layer(
    single_pt_data,
    col_var = "num_trips_diff",
    show_diff = TRUE
  ) +
  tm_title(glue::glue(
    "Bus Trip Changes ({selected_interval}h) - {selected_objective} Best Solution"
  )) +
  tm_layout(frame = FALSE)

plot3a

tmap_save(
  plot3a,
  file.path(output_dir, "03a_single_objective_no_fleet.png"),
  width = 16,
  height = 11,
  units = "cm",
  dpi = 300
)


# With DRT fleet (if enabled)
if (INCLUDE_DRT_FLEET) {
  plot3b <- create_base_layers() +
    create_drt_layer(
      drt,
      drt_deployments = all_drt_deployments,
      objective = selected_objective,
      solution = selected_solution,
      interval_label = selected_interval,
      show_fleet_size = TRUE
    ) +
    create_pt_layer(
      single_pt_data,
      col_var = "num_trips_diff",
      show_diff = TRUE
    ) +
    tm_title(glue::glue(
      "PT + DRT Changes ({selected_interval}h) - {selected_objective} Best Solution"
    )) +
    tm_layout(frame = FALSE)

  # Display
  plot3b

  # Save
  tmap_save(
    plot3b,
    file.path(output_dir, "03b_single_objective_with_fleet.png"),
    width = 16,
    height = 11,
    units = "cm",
    dpi = 300
  )
}

####################
#  Plot 4: Compare Multiple Solutions for Single Objective
####################

message("Creating Plot 4: Multiple solutions comparison...")

multi_solution_data <- all_solutions_overline |>
  filter(
    objective == selected_objective,
    interval_label == selected_interval
  )

plot4 <- tm_shape(study_area) +
  tm_borders(lwd = 0.5, col = "gray50") +
  tm_shape(multi_solution_data) +
  tm_lines(
    col = "num_trips_diff",
    col.scale = tm_scale_continuous(
      values = "brewer.rd_yl_gn",
      midpoint = 0
    ),
    col.legend = tm_legend(title = "Trip Diff"),
    lwd = 0.5
  ) +
  tm_facets_wrap(by = "solution", ncol = 3) +
  tm_title(glue::glue(
    "Bus Trip Changes by Solution Rank - {selected_objective}"
  )) +
  tm_layout(frame = FALSE)

plot4

tmap_save(
  plot4,
  file.path(output_dir, "04_solutions_by_rank.png"),
  width = 24,
  height = 16,
  units = "cm",
  dpi = 300
)
####################
#  Plot 5: DRT Fleet Deployment Summary (if enabled)
####################

if (INCLUDE_DRT_FLEET) {
  message("Creating Plot 5: DRT fleet deployment summary...")

  # Get best solution DRT deployments for all objectives at 8-12h
  best_drt_data <- all_drt_deployments |>
    filter(
      str_detect(solution, "_00$"),
      interval_label == "8-12"
    )

  # Create sf object for each objective
  drt_fleet_by_objective <- drt |>
    cross_join(
      best_drt_data |>
        select(objective, solution, interval_label, scenario, fleet_size)
    ) |>
    filter(scenario.x == scenario.y) |>
    select(-scenario.y) |>
    rename(scenario = scenario.x) |>
    st_as_sf()

  # Get unique fleet sizes for discrete scale
  fleet_levels <- sort(unique(all_drt_deployments$fleet_size))

  plot5 <- tm_shape(study_area) +
    tm_borders(lwd = 0.5, col = "gray50") +
    tm_shape(drt_fleet_by_objective) +
    tm_polygons(
      fill = "fleet_size",
      fill.scale = tm_scale_categorical(
        # Changed from continuous
        values = "brewer.purples", # Match plot3b
        levels = fleet_levels,
        labels = paste(fleet_levels, "vehicles")
      ),
      fill.legend = tm_legend(title = "DRT Fleet Size"), # More descriptive
      fill_alpha = 0.7,
      col = "#d95f02",
      lwd = 1
    ) +
    tm_facets_wrap(by = "objective", ncol = 3) +
    tm_title("DRT Fleet Deployment (8am-12pm) - Best Solutions by Objective") +
    tm_layout(frame = FALSE)

  plot5

  tmap_save(
    plot5,
    file.path(output_dir, "05_drt_fleet_by_objective.png"),
    width = 24,
    height = 16,
    units = "cm",
    dpi = 300
  )
}

####################
#  Plot 6: Combined PT + DRT for All Objectives (if enabled)
####################

if (
  INCLUDE_DRT_FLEET &&
    exists("drt_fleet_by_objective") &&
    !is.null(drt_fleet_by_objective) &&
    nrow(drt_fleet_by_objective) > 0
) {
  message("Creating Plot 6: Combined PT + DRT for all objectives...")

  # Use the same best PT data from Plot 2
  best_pt_data_full <- all_solutions_overline |>
    filter(
      str_detect(solution, "_00_gtfs$"),
      interval_label == "8-12"
    )

  # Ensure levels are defined
  if (!exists("fleet_levels")) {
    fleet_levels <- sort(unique(all_drt_deployments$fleet_size))
  }

  plot6 <- tm_shape(study_area) +
    # Use tm_polygons for background (borders doesn't support fill)
    tm_polygons(lwd = 0.5, border.col = "gray50", fill = "gray35") +

    # Layer 2: DRT Zones
    # Use numeric "fleet_size" matching Plot 5 logic
    tm_shape(drt_fleet_by_objective) +
    tm_polygons(
      fill = "fleet_size",
      fill.scale = tm_scale_categorical(
        values = "brewer.purples",
        levels = fleet_levels,
        labels = paste(fleet_levels, "vehicles")
      ),
      fill.legend = tm_legend(title = "DRT Fleet Size"),
      fill_alpha = 1.0, # Solid fill for visibility
      col = "#d95f02",
      lwd = 1.5
    ) +

    # Layer 3: PT Network
    # Showing increases in bus frequency (Blue/Green lines on top of Purple DRT)
    tm_shape(best_pt_data_full |> filter(num_trips_diff > 0)) +
    tm_lines(
      col = "num_trips_diff",
      col.scale = tm_scale_continuous(
        values = "brewer.rd_yl_bu",
        midpoint = 0
      ),
      col.legend = tm_legend(title = "Bus Trip Diff\n(vs Base)"),
      lwd = "num_trips",
      lwd.scale = tm_scale_continuous(values.scale = 3),
      lwd.legend = tm_legend(show = FALSE)
    ) +

    # Faceting
    tm_facets_wrap(by = "objective", ncol = 3) +
    tm_title("PT + DRT Changes (8am-12pm) - Best Solutions (Rank 0)") +
    tm_layout(frame = FALSE)

  plot6

  tmap_save(
    plot6,
    file.path(output_dir, "06_combined_pt_drt_by_objective.png"),
    width = 24,
    height = 16,
    units = "cm",
    dpi = 300
  )
}


message("\n✓ All plots saved to: ", output_dir)
