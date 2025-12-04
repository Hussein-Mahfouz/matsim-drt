library(sf)
library(dplyr)
library(purrr)
library(readr)
library(glue)
library(arrow)

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
#' @param pre_enriched Logical - if TRUE, assumes trips already has spatial flags
#'
#' @return Tibble with mode, total_distance_km, and share columns
#'
vkm_catchment <- function(
  trips,
  stops,
  catchment_radius = 500,
  level = c("trip", "person", "all"),
  access = c("origin", "origin+destination", "all"),
  zones = c("pt", "pt+drt"),
  drt_zones = NULL,
  modes = NULL,
  pre_enriched = FALSE
) {
  level <- match.arg(level)
  access <- match.arg(access)
  zones <- match.arg(zones)

  # Early exit for "all" scenario
  if (level == "all" && access == "all") {
    message("Using all trips (no filtering)...")
    trips_access <- trips |> filter(mode != "car_passenger")
  } else if (!pre_enriched) {
    # Original logic for non-enriched trips
    message("Creating unique trip id...")
    trips <- trips |>
      mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

    message("Buffering stops...")
    buffer <- stops |> st_buffer(catchment_radius, nQuadSegs = 3)

    if (zones == "pt+drt") {
      if (is.null(drt_zones)) {
        stop("zones = 'pt+drt' requires drt_zones (sf).")
      }
      message("Combining PT stop buffers and DRT zones...")
      drt_zones <- st_transform(drt_zones, st_crs(stops))
      catchment <- bind_rows(buffer, drt_zones)
    } else {
      catchment <- buffer
    }

    message("Checking origin within catchment...")
    origin_sf <- st_as_sf(
      trips,
      coords = c("origin_x", "origin_y"),
      crs = st_crs(stops),
      remove = FALSE
    )
    origin_in <- st_filter(origin_sf, catchment, .predicate = st_intersects) |>
      pull(unique_trip_id)

    if (access == "origin+destination") {
      message("Checking destination within catchment...")
      dest_sf <- st_as_sf(
        trips,
        coords = c("destination_x", "destination_y"),
        crs = st_crs(stops),
        remove = FALSE
      )
      dest_in <- st_filter(dest_sf, catchment, .predicate = st_intersects) |>
        pull(unique_trip_id)
      both_ids <- intersect(origin_in, dest_in)
      trips_access <- trips |> filter(unique_trip_id %in% both_ids)
    } else {
      trips_access <- trips |> filter(unique_trip_id %in% origin_in)
    }

    if (level == "person") {
      message("Filtering by person...")
      total_trips_per_person <- trips |> count(person_id, name = "total_trips")
      filtered_trips_per_person <- trips_access |>
        count(person_id, name = "filtered_trips")
      persons_all_in <- filtered_trips_per_person |>
        inner_join(total_trips_per_person, by = "person_id") |>
        filter(filtered_trips == total_trips) |>
        pull(person_id)
      trips_access <- trips_access |> filter(person_id %in% persons_all_in)
    }

    trips_access <- trips_access |> filter(mode != "car_passenger")
  } else {
    # Use pre-enriched spatial flags
    if (zones == "pt") {
      o_flag <- trips$pt_origin_ok
      d_flag <- trips$pt_dest_ok
    } else {
      # pt+drt
      o_flag <- trips$pt_origin_ok | trips$drt_origin_ok
      d_flag <- trips$pt_dest_ok | trips$drt_dest_ok
    }

    if (access == "origin") {
      trips_access <- trips |> filter(o_flag)
    } else {
      # origin+destination
      trips_access <- trips |> filter(o_flag & d_flag)
    }

    if (level == "person") {
      message("Filtering by person...")
      total_trips_per_person <- trips |> count(person_id, name = "total_trips")
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

  # Filter by modes if specified
  if (!is.null(modes)) {
    mode_pattern <- paste(modes, collapse = "|")
    trips_access <- trips_access |> filter(grepl(mode_pattern, mode))
  }

  message("Calculating VKM by mode...")
  trips_access |>
    group_by(mode) |>
    summarize(
      total_distance_km = sum(routed_distance, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) |>
    mutate(share = total_distance_km / sum(total_distance_km) * 100)
}

#' Calculate PT VKM from GTFS stop_times (zip file only)
#'
#' @param solution_dir Path to solution directory containing gtfs_feed.zip
#' @param boundary_sf sf object for filtering stops within boundary
#'
#' @return Tibble with mode="pt", total_distance_km, share=100
#'
pt_vkm_from_gtfs <- function(solution_dir, boundary_sf) {
  message("Reading GTFS from zip file...")

  gtfs_zip <- file.path(solution_dir, "gtfs_feed.zip")

  if (!file.exists(gtfs_zip)) {
    stop("gtfs_feed.zip not found in ", solution_dir)
  }

  message("Unzipping GTFS feed to temporary directory...")
  temp_gtfs_dir <- tempdir()
  unzip(gtfs_zip, exdir = temp_gtfs_dir, overwrite = TRUE)

  stop_times <- read_csv(
    file.path(temp_gtfs_dir, "stop_times.txt"),
    show_col_types = FALSE
  )

  gtfs_stops <- read_csv(
    file.path(temp_gtfs_dir, "stops.txt"),
    show_col_types = FALSE
  ) |>
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
    st_transform(3857) |>
    st_filter(boundary_sf, .predicate = st_within)

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
  include_all = TRUE,
  zones = c("pt"),
  drt_zones = NULL
) {
  message("Reading trips file with arrow...")
  trips <- arrow::read_delim_arrow(trips_file, delim = ";")

  # Pre-compute spatial flags ONCE
  message("Pre-computing spatial flags...")
  trips_enriched <- enrich_trips_spatially(
    trips,
    stops,
    drt_zones,
    catchment_radius
  )

  # Car/taxi VKM from eqasim_trips for each (level, access, zone)
  car_taxi_results <- expand_grid(
    level = levels,
    access = accesses,
    zones = zones
  ) |>
    pmap_df(function(level, access, zones) {
      message("\n")
      message("#####")
      message(glue::glue(
        "Computing level={level}, access={access}, zones={zones}"
      ))
      message("#####")
      message("\n")

      vkm_catchment(
        trips = trips_enriched,
        stops = stops,
        catchment_radius = catchment_radius,
        level = level,
        access = access,
        zones = zones,
        drt_zones = drt_zones,
        modes = if (is.null(modes)) NULL else setdiff(modes, "pt"),
        pre_enriched = TRUE
      ) |>
        mutate(
          level = level,
          access = access,
          zones = zones,
          .before = everything()
        )
    })

  # PT VKM from GTFS (same for all level/access/zone combinations)
  pt_result <- pt_vkm_from_gtfs(solution_dir, boundary_sf) |>
    expand_grid(level = levels, access = accesses, zones = zones) |>
    relocate(level, access, zones, .before = mode)

  # Combine and recalc shares grouped by level/access/zone
  all_vkm <- bind_rows(car_taxi_results, pt_result) |>
    group_by(level, access, zones) |>
    mutate(
      total_vkm = sum(total_distance_km),
      share = (total_distance_km / total_vkm) * 100
    ) |>
    ungroup() |>
    select(-total_vkm)

  # Add "all" (no filtering) scenario if requested
  if (include_all) {
    message("\n")
    message("#####")
    message(glue::glue("Computing level=all, access=all, zones=all"))
    message("#####")
    message("\n")

    all_scenario <- vkm_catchment(
      trips = trips, # Use original, not enriched
      stops = stops,
      catchment_radius = catchment_radius,
      level = "all",
      access = "all",
      zones = "pt",
      drt_zones = drt_zones,
      modes = if (is.null(modes)) NULL else setdiff(modes, "pt"),
      pre_enriched = FALSE
    ) |>
      mutate(
        level = "all",
        access = "all",
        zones = "all",
        .before = everything()
      )

    pt_all <- pt_vkm_from_gtfs(solution_dir, boundary_sf) |>
      mutate(level = "all", access = "all", zones = "all") |>
      relocate(level, access, zones, .before = mode)

    all_scenario_combined <- bind_rows(all_scenario, pt_all) |>
      group_by(level, access, zones) |>
      mutate(
        total_vkm = sum(total_distance_km),
        share = (total_distance_km / total_vkm) * 100
      ) |>
      ungroup() |>
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
  include_all = TRUE,
  zones = c("pt"),
  drt_zones = NULL
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
    message("\n")
    message("###############")
    message(glue::glue("Processing {basename(sol_dir)}..."))
    message("###############")
    message("\n")

    vkm_all_combinations(
      trips_file = trips_file,
      solution_dir = sol_dir,
      stops = stops,
      boundary_sf = boundary_sf,
      catchment_radius = catchment_radius,
      levels = levels,
      accesses = accesses,
      modes = modes,
      include_all = include_all,
      zones = zones,
      drt_zones = drt_zones
    ) |>
      mutate(solution = basename(sol_dir), .before = everything())
  })
}

#' Pre-calculate spatial flags for a trips dataframe (reused from mode_share_catchment.R)
#'
#' @param trips Dataframe with origin/destination coordinates
#' @param stops sf object with transit stop locations
#' @param drt_zones sf object with DRT zone polygons (optional)
#' @param catchment_radius Distance in meters
#'
#' @return trips dataframe with added boolean columns for spatial checks
#'
enrich_trips_spatially <- function(trips, stops, drt_zones, catchment_radius) {
  trips <- trips |>
    mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

  # Create buffer ONCE
  message("  Buffering PT stops...")
  pt_buffer <- stops |> st_buffer(catchment_radius, nQuadSegs = 3)

  # Prepare catchments
  if (!is.null(drt_zones)) {
    drt_zones <- st_transform(drt_zones, st_crs(stops))
    catchment_pt_drt <- bind_rows(pt_buffer, drt_zones)
  } else {
    catchment_pt_drt <- NULL
  }

  # Origins - PT
  message("  Checking PT accessibility (Origin)...")
  origins_sf <- st_as_sf(
    trips,
    coords = c("origin_x", "origin_y"),
    crs = st_crs(stops),
    remove = FALSE
  )

  origin_in_pt <- st_filter(
    origins_sf,
    pt_buffer,
    .predicate = st_intersects
  ) |>
    pull(unique_trip_id)
  trips$pt_origin_ok <- trips$unique_trip_id %in% origin_in_pt

  # Origins - DRT
  if (!is.null(drt_zones)) {
    message("  Checking DRT accessibility (Origin)...")
    origin_in_drt <- st_filter(
      origins_sf,
      drt_zones,
      .predicate = st_intersects
    ) |>
      pull(unique_trip_id)
    trips$drt_origin_ok <- trips$unique_trip_id %in% origin_in_drt
  } else {
    trips$drt_origin_ok <- FALSE
  }

  # Destinations - PT
  message("  Checking PT accessibility (Destination)...")
  dests_sf <- st_as_sf(
    trips,
    coords = c("destination_x", "destination_y"),
    crs = st_crs(stops),
    remove = FALSE
  )

  dest_in_pt <- st_filter(dests_sf, pt_buffer, .predicate = st_intersects) |>
    pull(unique_trip_id)
  trips$pt_dest_ok <- trips$unique_trip_id %in% dest_in_pt

  # Destinations - DRT
  if (!is.null(drt_zones)) {
    message("  Checking DRT accessibility (Destination)...")
    dest_in_drt <- st_filter(dests_sf, drt_zones, .predicate = st_intersects) |>
      pull(unique_trip_id)
    trips$drt_dest_ok <- trips$unique_trip_id %in% dest_in_drt
  } else {
    trips$drt_dest_ok <- FALSE
  }

  return(trips)
}
