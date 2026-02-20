library(tidyverse)
library(sf)
library(glue)

# Source function-only modules (they must contain only function definitions)
source("R/code/transit_opt/mode_share_catchment.R")
source("R/code/transit_opt/vkm_catchment.R")

# -------------------------
# Dynamic Configuration
# -------------------------

# Default iteration
ITERATION_ID <- "iteration_01"

# Check for command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  ITERATION_ID <- args[1]
  message(glue::glue("Using iteration from command line: {ITERATION_ID}"))
} else {
  message(glue::glue("No iteration specified, using default: {ITERATION_ID}"))
}


# -------------------------
# Parameters
# -------------------------

# Output directory: R/output/iteration_01/
# (We use "output" relative path so it works with the existing fetch script mapping)
output_dir <- file.path("R/output", ITERATION_ID)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ... (base_trips_file, base_solution_dir remain unchanged)

# Parent directory containing multiple objective subfolders
parent_dir <- file.path("data/supply/transit_opt_paper", ITERATION_ID)

# Path to eqasim trips CSV for the baseline scenario (required)
# - type: string (file path)
# - expected: eqasim_trips.csv produced by your scenario pipeline
base_trips_file <- "data/supply/transit_opt_paper/basic/combined_solution_00/output/eqasim_trips.csv"
# base_trips_file <- "scenarios/basic/sample_1.00/eqasim_trips.csv"

# Directory containing GTFS zip files for the baseline PT network (required)
# - type: string (directory path)
# - expected: contains zip file with stops.txt, stop_times.txt, etc.
base_solution_dir <- "data/supply/transit_opt_paper/basic/combined_solution_00"

# List objective folders (immediate children only - but not all of them)
objective_dirs <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)
# Filter to only directories matching specific patterns
# objective_dirs <- objective_dirs[grepl(
#   "sc_avg_var|sc_int_var|sc_peak_var|sc_sum_var|wt_avg_tot|wt_avg_var|wt_int_tot|wt_int_var|wt_sum_var",
#   basename(objective_dirs)
# )]
objective_dirs <- objective_dirs[grepl(
  "wt_avg_tot|wt_avg_atk|wt_int_tot|wt_int_atk|wt_peak_tot|wt_peak_atk|wt_sum_tot|wt_sum_atk",
  basename(objective_dirs)
)]

# Distance (meters) used to buffer PT stops when computing catchments
# - type: numeric (meters)
catchment_radius <- 300

# Analysis levels:
# - "trip": evaluate individual trips
# - "person": include person only if all their trips meet the access criterion
# - Note: "all" is handled separately by include_all
levels_vec <- c("trip", "person")

# Access definitions:
# - "origin": origin must be inside catchment
# - "origin+destination": both origin and destination must be inside catchment
accesses_vec <- c("origin", "origin+destination")

# Zone options for filtering:
# - "pt": only PT stop buffers used
# - "pt+drt": PT stop buffers PLUS DRT operating polygons included
zones_vec <- c("pt", "pt+drt")

# If TRUE, add an additional single "all" scenario (no spatial filtering) to outputs
include_all <- TRUE

# Output file paths
mode_share_out <- "R/output/mode_share_by_solution.csv" # Note: These vars don't seem used later, but good to update
vkm_out <- "R/output/vkm_by_solution.csv"
# dir.create("output", showWarnings = FALSE, recursive = TRUE) # No longer needed

# -------------------------
# Spatial data (single source)
# -------------------------

# PT stops
stops <- read_csv(
  unz(
    "data/supply/transit_opt_paper/basic/combined_solution_00/gtfs_feed.zip",
    "stops.txt"
  ),
  show_col_types = FALSE
) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(3857)

# study area boundary (to filter out stray pt stops from GTFS)
boundary_sf <- st_read("data/external/study_area_boundary.geojson") |>
  st_transform(3857) |>
  st_union()

stops <- stops |> st_filter(boundary_sf, .predicate = st_within)

# DRT zone boundaries
drt_zone_ne <- st_read(
  "data/supply/drt/ne_cluster_08_00_11_00.shp",
  quiet = TRUE
) |>
  st_transform(3857)
drt_zone_nw <- st_read(
  "data/supply/drt/nw_cluster_08_00_11_00.shp",
  quiet = TRUE
) |>
  st_transform(3857)

drt_zones = bind_rows(drt_zone_ne, drt_zone_nw)

# -------------------------
# Mode-share analysis
# -------------------------

message("\n")
message("##########################################")
message("## MODE-SHARE ANALYSIS")
message("##########################################")
message("\n")

# Load baseline trips DF once for imputation use downstream
message("Loading baseline trips needed for consistency checks...")
base_trips_df <- arrow::read_delim_arrow(base_trips_file, delim = ";")

# -------------------------
# OPTIMIZATION: Pre-compute spatial flags
# -------------------------
message("Pre-computing spatial lookup for all trips (OPTIMIZATION)...")
base_trips_enriched <- enrich_trips_spatially(
  trips = base_trips_df,
  stops = stops,
  drt_zones = drt_zones,
  catchment_radius = catchment_radius
)

# Create lightweight lookup table
spatial_lookup <- base_trips_enriched |>
  select(
    person_id,
    person_trip_id,
    pt_origin_ok,
    pt_dest_ok,
    drt_origin_ok,
    drt_dest_ok
  )
message("✓ Spatial lookup created")

# prepare base results once
message("Processing BASELINE for mode-share analysis...")
base_modes <- mode_share_all_combinations(
  trips_file = base_trips_enriched, # Use enriched data directly
  stops = stops,
  catchment_radius = catchment_radius,
  levels = levels_vec,
  accesses = accesses_vec,
  include_all = include_all,
  zones = zones_vec,
  drt_zones = drt_zones
) |>
  mutate(solution = "base", .before = everything())

message("✓ Baseline mode-share complete\n")

# iterate objectives and collect results
all_mode_list <- map(objective_dirs, function(obj_dir) {
  objective_name <- basename(obj_dir)
  message("\n")
  message("##########################################")
  message("## OBJECTIVE: ", objective_name)
  message("##########################################")
  message("\n")

  # read all solutions under this objective folder
  # mode_share_by_solution expects the folder that directly contains solution_* folders
  obj_modes <- mode_share_by_solution(
    solutions_dir = obj_dir,
    stops = stops,
    catchment_radius = catchment_radius,
    levels = levels_vec,
    accesses = accesses_vec,
    include_all = include_all,
    zones = zones_vec,
    drt_zones = drt_zones,
    base_trips_df = base_trips_df,
    spatial_lookup = spatial_lookup
  ) |>
    mutate(objective = objective_name, .before = everything())

  # attach base (replicated with same objective id)
  base_modes_obj <- base_modes |>
    mutate(objective = objective_name, .before = everything())

  bind_rows(base_modes_obj, obj_modes)
})
message("\n✓ All objectives completed for mode-share\n")

all_mode_results <- bind_rows(all_mode_list)

# add numeric solution id (solution_01_gtfs -> 1, solution_10_gtfs -> 10, base -> 0)
all_mode_results <- all_mode_results %>%
  mutate(
    solution_id = as.integer(str_extract(solution, "\\d+")),
    solution_id = if_else(solution == "base", 0L, solution_id)
  )

# now compute pct changes using base grouped by objective/level/access/zones/mode
base_ref <- all_mode_results |>
  filter(solution == "base") |>
  select(objective, level, access, zones, mode, n_base = n, share_base = share)

mode_results <- all_mode_results |>
  filter(solution != "base") |>
  left_join(
    base_ref,
    by = c("objective", "level", "access", "zones", "mode")
  ) |>
  replace_na(list(n_base = 0, share_base = 0)) |>
  mutate(
    n_solution = n,
    share_solution = share,
    n_pct_change = if_else(
      n_base == 0,
      NA_real_,
      (n_solution - n_base) / n_base * 100
    ),
    share_pct_change = if_else(
      share_base == 0,
      NA_real_,
      (share_solution - share_base) / share_base * 100
    )
  ) |>
  select(
    objective,
    solution,
    solution_id,
    level,
    access,
    zones,
    mode,
    n_solution,
    share_solution,
    n_base,
    share_base,
    n_pct_change,
    share_pct_change
  )

# optional: write combined csv
write_csv(mode_results, file.path(output_dir, "mode_share_by_objective.csv"))

# -------------------------
# VKM analysis
# -------------------------

message("\n")
message("##########################################")
message("## VKM ANALYSIS")
message("##########################################")
message("\n")

# prepare base results once
message("Processing BASELINE for VKM analysis...")
base_vkm <- vkm_all_combinations(
  trips_file = base_trips_enriched, # Use enriched data
  solution_dir = base_solution_dir,
  stops = stops,
  boundary_sf = boundary_sf,
  catchment_radius = catchment_radius,
  levels = levels_vec,
  accesses = accesses_vec,
  modes = c("^car$", "taxi", "pt", "drt"), # Regex for car to avoid car_passenger)
  include_all = include_all,
  zones = zones_vec,
  drt_zones = drt_zones
) |>
  mutate(solution = "base", .before = everything())

message("✓ Baseline VKM complete\n")

all_vkm_list <- map(objective_dirs, function(obj_dir) {
  objective_name <- basename(obj_dir)
  message("\n")
  message("##########################################")
  message("## OBJECTIVE (VKM): ", objective_name)
  message("##########################################")
  message("\n")

  obj_vkm <- vkm_by_solution(
    solutions_dir = obj_dir,
    stops = stops,
    boundary_sf = boundary_sf,
    catchment_radius = catchment_radius,
    levels = levels_vec,
    accesses = accesses_vec,
    modes = c("^car$", "taxi", "pt", "drt"), # Regex for car to avoid car_passenger
    include_all = include_all,
    zones = zones_vec,
    drt_zones = drt_zones,
    base_trips_df = base_trips_df,
    spatial_lookup = spatial_lookup
  ) |>
    mutate(objective = objective_name, .before = everything())

  base_vkm_obj <- base_vkm |> mutate(objective = objective_name)

  bind_rows(base_vkm_obj, obj_vkm)
})
message("\n✓ All objectives completed for VKM\n")

all_vkm_results <- bind_rows(all_vkm_list)

# Add solution id from name
all_vkm_results <- all_vkm_results %>%
  mutate(
    solution_id = as.integer(str_extract(solution, "\\d+")),
    solution_id = if_else(solution == "base", 0L, solution_id)
  )

base_vkm_ref <- all_vkm_results |>
  filter(solution == "base") |>
  select(
    objective,
    level,
    access,
    zones,
    mode,
    total_distance_km_base = total_distance_km,
    share_base = share
  )

vkm_results <- all_vkm_results |>
  filter(solution != "base") |>
  left_join(
    base_vkm_ref,
    by = c("objective", "level", "access", "zones", "mode")
  ) |>
  replace_na(list(total_distance_km_base = 0, share_base = 0)) |>
  mutate(
    total_distance_km_solution = total_distance_km,
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = if_else(
      total_distance_km_base == 0,
      NA_real_,
      delta_km / total_distance_km_base * 100
    ),
    share_solution = share,
    share_pct_change = share_solution - share_base
  ) |>
  select(
    objective,
    solution,
    solution_id,
    level,
    access,
    zones,
    mode,
    total_distance_km_solution,
    share_solution,
    total_distance_km_base,
    share_base,
    delta_km,
    delta_km_pct,
    share_pct_change
  )

write_csv(vkm_results, file.path(output_dir, "vkm_by_objective.csv"))

message("\n")
message("##########################################")
message("## COMBINING SOLUTION OBJECTIVE VALUES")
message("##########################################")
message("\n")

# List of objective names (e.g. "sc_avg_var", ...)
objective_names <- basename(objective_dirs)

# Build the correct paths
summary_paths <- file.path(
  "../../transit_opt/output",
  objective_names,
  ITERATION_ID,
  "pso_results",
  "combined_solution_summary.csv"
)


# Function to read each summary file
read_solution_summary <- function(path, objective_name) {
  if (!file.exists(path)) {
    message(glue::glue("No combined_solution_summary.csv found in {path}"))
    return(NULL)
  }
  df <- read_csv(path, show_col_types = FALSE)
  df$objective_name <- objective_name
  df
}

# Read and combine all
all_solution_objectives_df <- purrr::map2_dfr(
  summary_paths,
  objective_names,
  read_solution_summary
) %>%
  select(objective_name, everything())

# Save to CSV
write_csv(
  all_solution_objectives_df,
  file.path(output_dir, "pso_objective_values.csv")
)

message(
  "✓ Combined solution objective values written to R/output/pso_objective_values.csv\n"
)
