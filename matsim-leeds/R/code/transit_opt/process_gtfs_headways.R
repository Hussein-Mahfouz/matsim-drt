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
  all_obj_dirs <- list.dirs(SOLUTIONS_PARENT_DIR, full.names = FALSE, recursive = FALSE)
  
  # Filter to those that interpret as objectives
  valid_obj_regex <- "sc_avg_var|sc_int_var|sc_peak_var|sc_sum_var|wt_avg_tot|wt_avg_var|wt_avg_atk|wt_int_tot|wt_int_var|wt_int_atk|wt_peak_tot|wt_peak_var|wt_peak_atk|wt_sum_tot|wt_sum_var|wt_sum_atk"
  candidates <- all_obj_dirs[grepl(valid_obj_regex, all_obj_dirs)]
  
  # Check if they have the specific iteration folder with data
  OBJECTIVES_TO_PROCESS <- candidates[sapply(candidates, function(obj) {
    target_dir <- file.path(SOLUTIONS_PARENT_DIR, obj, ITERATION_FOLDER, "pso_results")
    if (!dir.exists(target_dir)) return(FALSE)
    # Check for at least one zip file
    length(list.files(target_dir, pattern = "\\.zip$")) > 0
  })]
  
  message(glue::glue("Auto-detected {length(OBJECTIVES_TO_PROCESS)} objectives with data for {ITERATION_FOLDER}:"))
  message(paste(OBJECTIVES_TO_PROCESS, collapse = ", "))
  
  if (length(OBJECTIVES_TO_PROCESS) == 0) {
    stop(glue::glue("No objectives found with data for {ITERATION_FOLDER} in {SOLUTIONS_PARENT_DIR}"))
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
