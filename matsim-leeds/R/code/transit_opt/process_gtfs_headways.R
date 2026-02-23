#' =============================================================================
#' GTFS Headway Processing for Transit Optimization Analysis
#' =============================================================================
#'
#' Purpose:
#'   Process GTFS feeds from all transit optimization solutions and calculate
#'   headway (service frequency) by route and time interval. This script is
#'   designed to run on the cluster where GTFS zip files are stored.
#'
#' Inputs:
#'   - Base GTFS feed (baseline scenario)
#'   - Solution GTFS feeds (from PSO optimization results)
#'   - DRT deployment JSON files
#'
#' Outputs:
#'   - gtfs_sf_headways_overline.rds: Baseline network headways
#'   - gtfs_headway_comparisons_overline.rds: All solution comparisons
#'   - drt_fleet_deployments.csv: DRT fleet sizes by zone and interval
#'
#' Usage:
#'   On cluster: bash bash/transit_opt/4_process_gtfs_headways.sh --cluster
#'   Locally:    Rscript R/code/transit_opt/process_gtfs_headways.R
#'
#' Dependencies:
#'   - R/code/transit_opt/gtfs_headway_analysis.R (helper functions)
#'   - Conda environment: r-transit-opt (see R/r_transit_opt_env.yaml)

library(tidyverse)
library(sf)
library(tidytransit)
library(jsonlite)


# Source helper functions
source("R/code/transit_opt/gtfs_headway_analysis.R")

####################
#  Configuration
####################

message("\n==========================================")
message("GTFS HEADWAY PROCESSING")
message("==========================================\n")

####################
#  Dynamic Configuration
####################

# Default
ITERATION_FOLDER <- "iteration_02"

# Check for command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  ITERATION_FOLDER <- args[1]
  message(glue::glue("Using iteration from command line: {ITERATION_FOLDER}"))
}

####################
#  Configuration
####################

# Paths
BASE_GTFS_PATH <- "data/supply/transit_opt_paper/basic/combined_solution_00/gtfs_feed.zip"
SOLUTIONS_PARENT_DIR <- "../../transit_opt/output"

# Update output directory to include iteration
OUTPUT_DIR <- file.path("R/output", ITERATION_FOLDER)

# Objectives to process (NULL = auto-detect)
OBJECTIVES_TO_PROCESS <- NULL

# --- AUTO-DETECT OBJECTIVES FOR THIS ITERATION ---
# Verify which objectives actually have results for the target iteration
if (is.null(OBJECTIVES_TO_PROCESS)) {
  all_obj_dirs <- list.dirs(
    SOLUTIONS_PARENT_DIR,
    full.names = FALSE,
    recursive = FALSE
  )

  # Filter to those that interpret as objectives
  valid_obj_regex <- "sc_avg_var|sc_int_var|sc_peak_var|sc_sum_var|wt_avg_tot|wt_avg_var|wt_avg_atk|wt_int_tot|wt_int_var|wt_int_atk|wt_peak_tot|wt_peak_var|wt_peak_atk|wt_sum_tot|wt_sum_var|wt_sum_atk"
  candidates <- all_obj_dirs[grepl(valid_obj_regex, all_obj_dirs)]

  # Check if they have the specific iteration folder with data
  OBJECTIVES_TO_PROCESS <- candidates[sapply(candidates, function(obj) {
    target_dir <- file.path(
      SOLUTIONS_PARENT_DIR,
      obj,
      ITERATION_FOLDER,
      "pso_results"
    )
    if (!dir.exists(target_dir)) {
      return(FALSE)
    }
    # Check for at least one zip file
    length(list.files(target_dir, pattern = "\\.zip$")) > 0
  })]

  message(glue::glue(
    "Auto-detected {length(OBJECTIVES_TO_PROCESS)} objectives with data for {ITERATION_FOLDER}:"
  ))
  message(paste(OBJECTIVES_TO_PROCESS, collapse = ", "))

  if (length(OBJECTIVES_TO_PROCESS) == 0) {
    stop(glue::glue(
      "No objectives found with data for {ITERATION_FOLDER} in {SOLUTIONS_PARENT_DIR}"
    ))
  }
}

# Interval hours for headway calculation
INTERVAL_HOURS <- 4

# Number of cores for parallel processing
NCORES <- 4

# Log configuration
message("Configuration:")
message("  Base GTFS path: ", BASE_GTFS_PATH)
message("  Solutions directory: ", SOLUTIONS_PARENT_DIR)
message("  Output directory: ", OUTPUT_DIR)
message("  Iteration folder: ", ITERATION_FOLDER)
message("  Interval hours: ", INTERVAL_HOURS)
message("  Number of cores: ", NCORES)
message("")


# Create output directory
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

####################
#  Process Base GTFS
####################

message("\n========================================")
message("Processing BASE GTFS...")
message("========================================\n")

tryCatch(
  {
    gtfs <- tidytransit::read_gtfs(BASE_GTFS_PATH)
    message("✓ Base GTFS loaded successfully")

    gtfs_sf_headways <- calculate_route_headway(
      gtfs,
      interval_hours = INTERVAL_HOURS
    )
    message("✓ Base headways calculated")

    gtfs_sf_headways_overline <- aggregate_route_headways(
      gtfs_sf_headways,
      ncores = NCORES
    )
    message("✓ Base headways aggregated")

    # Save base headways
    saveRDS(
      gtfs_sf_headways_overline,
      file.path(OUTPUT_DIR, "gtfs_sf_headways_overline.rds")
    )
    message(
      "✓ Base GTFS headways saved to: ",
      file.path(OUTPUT_DIR, "gtfs_sf_headways_overline.rds")
    )
  },
  error = function(e) {
    message("✗ ERROR processing base GTFS: ", e$message)
    message("Call stack:")
    print(sys.calls())
    stop(e)
  }
)

message("")

####################
#  Process All Solutions
####################

message("\n========================================")
message("Processing all solution GTFS feeds...")
message("========================================\n")

tryCatch(
  {
    results <- process_all_gtfs_solutions(
      base_gtfs_path = BASE_GTFS_PATH,
      solutions_parent_dir = SOLUTIONS_PARENT_DIR,
      objective_names = OBJECTIVES_TO_PROCESS,
      interval_hours = INTERVAL_HOURS,
      iteration_folder = ITERATION_FOLDER
    )
    message("✓ All solution GTFS feeds processed")

    # Aggregate solution comparisons
    all_solutions_overline <- aggregate_solution_comparisons(
      results$all_solutions,
      attrib = c("num_trips", "num_trips_diff"),
      ncores = NCORES
    )
    message("✓ Solution comparisons aggregated")

    # Save solution comparisons
    saveRDS(
      all_solutions_overline,
      file.path(OUTPUT_DIR, "gtfs_headway_comparisons_overline.rds")
    )
    message(
      "✓ Solution GTFS headways saved to: ",
      file.path(OUTPUT_DIR, "gtfs_headway_comparisons_overline.rds")
    )

    # Save Bus Fleet Sizes
    if (!is.null(results$all_solutions_fleet)) {
      # Remove _gtfs suffix from solution column for consistency
      results$all_solutions_fleet <- results$all_solutions_fleet |>
        mutate(solution = str_remove(as.character(solution), "_gtfs$"))

      write_csv(
        results$all_solutions_fleet,
        file.path(OUTPUT_DIR, "bus_fleet_sizes.csv")
      )
      message(
        "✓ Bus fleet sizes saved to: ",
        file.path(OUTPUT_DIR, "bus_fleet_sizes.csv")
      )
    }
  },
  error = function(e) {
    message("✗ ERROR processing solution GTFS feeds: ", e$message)
    message("Call stack:")
    print(sys.calls())
    stop(e)
  }
)

message("")

####################
#  Load DRT Deployments
####################

tryCatch(
  {
    all_drt_deployments <- load_all_drt_deployments(
      solutions_parent_dir = SOLUTIONS_PARENT_DIR,
      objective_names = OBJECTIVES_TO_PROCESS,
      iteration_folder = ITERATION_FOLDER
    )
    message("✓ DRT deployments loaded")

    # Save DRT deployments
    write_csv(
      all_drt_deployments,
      file.path(OUTPUT_DIR, "drt_fleet_deployments.csv")
    )
    message(
      "✓ DRT fleet deployments saved to: ",
      file.path(OUTPUT_DIR, "drt_fleet_deployments.csv")
    )

    # ---------------------------------------------------------
    # Generate Combined Fleet Statistcs (Bus + DRT)
    # ---------------------------------------------------------

    bus_fleet_path <- file.path(OUTPUT_DIR, "bus_fleet_sizes.csv")

    if (file.exists(bus_fleet_path)) {
      message("\nGenerating combined fleet analysis...")

      bus_fleet <- read_csv(bus_fleet_path, show_col_types = FALSE)

      # Aggregate DRT fleet by solution and interval
      drt_fleet_agg <- all_drt_deployments |>
        group_by(objective, solution, interval_label) |>
        summarise(drt_fleet_solution = sum(fleet_size), .groups = "drop")
      # Join and calculate totals
      combined_fleet <- bus_fleet |>
        rename(bus_fleet_solution = max_bus_fleet) |>
        mutate(
          objective = as.character(objective),
          solution = str_remove(as.character(solution), "_gtfs$"),
          interval_label = as.character(interval_label)
        ) |>
        left_join(
          drt_fleet_agg |>
            mutate(
              objective = as.character(objective),
              solution = as.character(solution),
              interval_label = as.character(interval_label)
            ),
          by = c("objective", "solution", "interval_label")
        ) |>
        mutate(
          drt_fleet_solution = replace_na(drt_fleet_solution, 0),

          # Bus differences
          # Note: bus_fleet_sizes.csv has columns: max_bus_fleet(solution), max_bus_fleet_base, max_bus_fleet_diff
          bus_fleet_diff = bus_fleet_solution - max_bus_fleet_base,
          bus_fleet_pct_change = if_else(
            max_bus_fleet_base > 0,
            round((bus_fleet_diff / max_bus_fleet_base) * 100, 2),
            0
          ),

          # Total totals
          total_fleet_base = max_bus_fleet_base, # DRT used to be 0
          total_fleet_solution = bus_fleet_solution + drt_fleet_solution,
          total_fleet_diff = total_fleet_solution - total_fleet_base,
          total_fleet_pct_change = if_else(
            total_fleet_base > 0,
            round((total_fleet_diff / total_fleet_base) * 100, 2),
            0
          )
        ) |>
        rename(bus_fleet_base = max_bus_fleet_base) |>
        select(
          objective,
          solution,
          interval_label,
          bus_fleet_base,
          bus_fleet_solution,
          drt_fleet_solution,
          bus_fleet_diff,
          bus_fleet_pct_change,
          total_fleet_diff,
          total_fleet_pct_change
        ) |>
        # Sort
        mutate(start_hour = as.numeric(str_extract(interval_label, "^\\d+"))) |>
        arrange(objective, solution, start_hour) |>
        select(-start_hour)

      write_csv(
        combined_fleet,
        file.path(OUTPUT_DIR, "combined_fleet_sizes.csv")
      )
      message(
        "✓ Combined fleet analysis saved to: ",
        file.path(OUTPUT_DIR, "combined_fleet_sizes.csv")
      )
    }
  },
  error = function(e) {
    message("✗ ERROR loading DRT deployments: ", e$message)
    message("Call stack:")
    print(sys.calls())
    stop(e)
  }
)

message("")

####################
#  Summary
####################

message("==========================================")
message("PROCESSING COMPLETE")
message("==========================================\n")

message("End time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("")
message("Output files saved to: ", OUTPUT_DIR)
message("  - gtfs_sf_headways_overline.rds")
message("  - gtfs_headway_comparisons_overline.rds")
message("  - drt_fleet_deployments.csv")
message("  - bus_fleet_sizes.csv")
message("  - combined_fleet_sizes.csv")
