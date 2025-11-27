library(tidyverse)
library(sf)

# ============================================================================
# Data Setup
# ============================================================================

stops <- read_csv(unz(
  "../data/external/study_area_gtfs_bus.zip",
  "stops.txt"
)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(3857)

boundary_sf <- st_read("../data/external/study_area_boundary.geojson") |>
  st_transform(3857) |>
  st_union()

stops <- stops |>
  st_filter(boundary_sf, .predicate = st_within)

# ============================================================================
# Functions
# ============================================================================

#' Calculate VKM for trips within transit stop catchment
#'
#' @param trips Dataframe with origin/destination coordinates and routed_distance
#' @param stops sf object with transit stop locations
#' @param catchment_radius Distance in meters to buffer stops
#' @param level "trip" or "person" (persons with all trips in buffer)
#' @param access "origin" or "origin+destination"
#' @param modes Vector of modes to include. If NULL, includes all modes
#'
#' @return Tibble with mode, total_distance_km, and share columns
#'
vkm_catchment <- function(
  trips,
  stops,
  catchment_radius = 500,
  level = c("trip", "person", "all"),
  access = c("origin", "origin+destination", "all"),
  modes = NULL
) {
  level <- match.arg(level)
  access <- match.arg(access)

  message("Creating unique trip id...")
  trips <- trips |>
    mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

  # If level = "all" and access = "all", skip filtering
  if (level == "all" && access == "all") {
    message("Using all trips (no filtering)...")
    trips_access <- trips |> filter(mode != "car_passenger")
  } else {
    message("Buffering stops...")
    buffer <- stops |> st_buffer(catchment_radius)

    message("Checking origin within buffer...")
    origin_sf <- st_as_sf(
      trips,
      coords = c("origin_x", "origin_y"),
      crs = st_crs(stops),
      remove = FALSE
    )
    origin_in <- st_filter(origin_sf, buffer, .predicate = st_intersects) |>
      pull(unique_trip_id)

    if (access == "origin+destination") {
      message("Checking destination within buffer...")
      dest_sf <- st_as_sf(
        trips,
        coords = c("destination_x", "destination_y"),
        crs = st_crs(stops),
        remove = FALSE
      )
      dest_in <- st_filter(dest_sf, buffer, .predicate = st_intersects) |>
        pull(unique_trip_id)
      both_ids <- intersect(origin_in, dest_in)
      trips_access <- trips |> filter(unique_trip_id %in% both_ids)
    } else {
      trips_access <- trips |> filter(unique_trip_id %in% origin_in)
    }

    if (level == "person") {
      message("Filtering by person...")
      total_trips_per_person <- trips |>
        count(person_id, name = "total_trips")
      filtered_trips_per_person <- trips_access |>
        count(person_id, name = "filtered_trips")
      persons_all_in <- filtered_trips_per_person |>
        inner_join(total_trips_per_person, by = "person_id") |>
        filter(filtered_trips == total_trips) |>
        pull(person_id)
      trips_access <- trips_access |> filter(person_id %in% persons_all_in)
    }

    trips_access <- trips_access |> filter(mode != "car_passenger")
  }

  message("Calculating VKM by mode...")
  result <- trips_access

  # Filter by modes if specified
  if (!is.null(modes)) {
    result <- result |> filter(mode %in% modes)
  }

  result |>
    group_by(mode) |>
    summarize(
      total_distance_km = sum(routed_distance, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    mutate(share = total_distance_km / sum(total_distance_km) * 100)
}

#' Calculate PT VKM from GTFS stop_times
#'
#' @param solution_dir Path to solution directory containing GTFS files
#' @param boundary_sf sf object for filtering stops within boundary
#'
#' @return Tibble with mode="pt", total_distance_km, share=100
#'
pt_vkm_from_gtfs <- function(solution_dir, boundary_sf) {
  message("Reading GTFS files...")
  stop_times <- read_csv(
    file.path(solution_dir, "stop_times.txt"),
    show_col_types = FALSE
  )

  gtfs_stops <- read_csv(
    file.path(solution_dir, "stops.txt"),
    show_col_types = FALSE
  ) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    st_transform(3857) |>
    st_filter(boundary_sf, .predicate = st_within)

  # Extract coordinates as numeric columns
  coords <- st_coordinates(gtfs_stops)
  gtfs_stops_coords <- gtfs_stops |>
    st_drop_geometry() |>
    mutate(
      x = coords[, 1],
      y = coords[, 2]
    ) |>
    select(stop_id, x, y)

  message("Calculating PT distances...")
  stop_times |>
    left_join(gtfs_stops_coords, by = "stop_id") |>
    arrange(trip_id, stop_sequence) |>
    group_by(trip_id) |>
    mutate(
      next_x = lead(x),
      next_y = lead(y),
      distance_m = sqrt((next_x - x)^2 + (next_y - y)^2)
    ) |>
    filter(!is.na(distance_m)) |>
    summarize(
      total_distance_m = sum(distance_m, na.rm = TRUE),
      .groups = "drop"
    ) |>
    summarize(total_distance_km = sum(total_distance_m) / 1000) |>
    mutate(
      mode = "pt",
      share = 100,
      .before = everything()
    )
}
#' Calculate VKM for multiple parameter combinations
#'
#' @param trips_file Path to trips CSV file
#' @param solution_dir Path to solution directory (for GTFS PT data)
#' @param stops sf object with transit stop locations
#' @param boundary_sf sf object for filtering stops within boundary
#' @param catchment_radius Distance in meters
#' @param levels Vector of level values
#' @param accesses Vector of access values
#' @param modes Vector of modes to include. If NULL, includes all modes
#' @param include_all if TRUE, add an extra scenario that calculates global vkm (without any filtering)
#'
#' @return Tibble with results for all combinations
#'
vkm_all_combinations <- function(
  trips_file,
  solution_dir,
  stops,
  boundary_sf,
  catchment_radius = 500,
  levels = c("trip", "person"),
  accesses = c("origin", "origin+destination"),
  modes = NULL,
  include_all = TRUE
) {
  trips <- read_delim(trips_file, delim = ";", show_col_types = FALSE)

  # Car/taxi VKM from eqasim_trips
  car_taxi_results <- expand_grid(level = levels, access = accesses) |>
    pmap_df(function(level, access) {
      message(glue::glue("Computing level={level}, access={access}"))
      vkm_catchment(
        trips = trips,
        stops = stops,
        catchment_radius = catchment_radius,
        level = level,
        access = access,
        modes = if (is.null(modes)) NULL else setdiff(modes, "pt")
      ) |>
        mutate(level = level, access = access, .before = everything())
    })

  # PT VKM from GTFS (same for all level/access combinations)
  pt_result <- pt_vkm_from_gtfs(solution_dir, boundary_sf) |>
    expand_grid(level = levels, access = accesses) |>
    relocate(level, access, .before = mode)

  # Combine and recalculate shares based on total VKM
  all_vkm <- bind_rows(car_taxi_results, pt_result) |>
    group_by(level, access) |>
    mutate(
      total_vkm = sum(total_distance_km),
      share = (total_distance_km / total_vkm) * 100
    ) |>
    ungroup() |>
    select(-total_vkm)

  # Add "all" scenario if requested
  if (include_all) {
    all_scenario <- vkm_catchment(
      trips = trips,
      stops = stops,
      catchment_radius = catchment_radius,
      level = "all",
      access = "all",
      modes = if (is.null(modes)) NULL else setdiff(modes, "pt")
    ) |>
      mutate(level = "all", access = "all", .before = everything())

    pt_all <- pt_vkm_from_gtfs(solution_dir, boundary_sf) |>
      mutate(level = "all", access = "all") |>
      relocate(level, access, .before = mode)

    all_scenario_combined <- bind_rows(all_scenario, pt_all) |>
      mutate(
        total_vkm = sum(total_distance_km),
        share = (total_distance_km / total_vkm) * 100
      ) |>
      select(-total_vkm)

    all_vkm <- bind_rows(all_vkm, all_scenario_combined)
  }

  all_vkm
}

#' Calculate VKM for all solutions in a directory
#'
#' @param solutions_dir Directory containing solution subdirectories
#' @param stops sf object with transit stop locations
#' @param boundary_sf sf object for filtering stops within boundary
#' @param catchment_radius Distance in meters
#' @param levels Vector of level values
#' @param accesses Vector of access values
#' @param modes Vector of modes to include. If NULL, includes all modes
#'
#' @return Tibble with solution, level, access, mode, total_distance_km, share columns
#'
vkm_by_solution <- function(
  solutions_dir,
  stops,
  boundary_sf,
  catchment_radius = 500,
  levels = c("trip", "person"),
  accesses = c("origin", "origin+destination"),
  modes = NULL,
  include_all = TRUE
) {
  solution_dirs <- list.dirs(
    solutions_dir,
    full.names = TRUE,
    recursive = FALSE
  )

  map_df(solution_dirs, function(sol_dir) {
    trips_file <- list.files(
      sol_dir,
      pattern = "eqasim_trips\\.csv$",
      full.names = TRUE,
      recursive = TRUE
    )[1]

    if (is.na(trips_file)) {
      message(glue::glue("No eqasim_trips.csv found in {sol_dir}"))
      return(NULL)
    }

    message(glue::glue("Processing {basename(sol_dir)}..."))

    vkm_all_combinations(
      trips_file = trips_file,
      solution_dir = sol_dir,
      stops = stops,
      boundary_sf = boundary_sf,
      catchment_radius = catchment_radius,
      levels = levels,
      accesses = accesses,
      modes = modes,
      include_all = include_all
    ) |>
      mutate(solution = basename(sol_dir), .before = everything())
  })
}

# ============================================================================
# Analysis
# ============================================================================

# Define parameter combinations to analyze
PARAMS <- list(
  levels = c("trip", "person"),
  accesses = c("origin", "origin+destination")
)

CATCHMENT_RADIUS <- 100
MODES <- c("car", "taxi", "pt")

# Analyze solutions
all_solutions <- vkm_by_solution(
  solutions_dir = "../data/external/gtfs_optimisation/min_variance_stops",
  stops = stops,
  boundary_sf = boundary_sf,
  catchment_radius = CATCHMENT_RADIUS,
  levels = PARAMS$levels,
  accesses = PARAMS$accesses,
  modes = MODES,
  include_all = TRUE
)

# Analyze base scenario
base_trips_file <- "../scenarios/basic/sample_1.00/eqasim_trips.csv"
base_solution_dir <- "../data/external/gtfs_optimisation/max_min_theoretical/worst_service_gtfs"

base_results <- vkm_all_combinations(
  trips_file = base_trips_file,
  solution_dir = base_solution_dir,
  stops = stops,
  boundary_sf = boundary_sf,
  catchment_radius = CATCHMENT_RADIUS,
  levels = PARAMS$levels,
  accesses = PARAMS$accesses,
  modes = MODES,
  include_all = TRUE
) |>
  mutate(solution = "base", .before = everything())

# Combine and calculate percent change
all_results <- bind_rows(base_results, all_solutions)

results_with_change <- all_results |>
  left_join(
    base_results |> select(level, access, mode, total_distance_km, share),
    by = c("level", "access", "mode"),
    suffix = c("_solution", "_base")
  ) |>
  replace_na(list(total_distance_km_base = 0, share_base = 0)) |>
  mutate(
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = if_else(
      total_distance_km_base == 0,
      NA_real_,
      (delta_km / total_distance_km_base) * 100
    ),
    share_pct_change = share_solution - share_base,
    .after = share_base
  ) |>
  filter(solution != "base") |>
  arrange(solution, level, access, mode)

results_with_change
