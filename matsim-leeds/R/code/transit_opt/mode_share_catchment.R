library(sf)
library(dplyr)
library(purrr)
library(readr)
library(glue)
library(arrow)

# ============================================================================
# Functions
# ============================================================================

#' Calculate mode share for trips within transit stop catchment
#'
#' @param trips Dataframe with origin/destination coordinates (can be pre-enriched)
#' @param stops sf object with transit stop locations
#' @param catchment_radius Distance in meters to buffer stops
#' @param level "trip", "person", or "all" (all trips, no filtering)
#' @param access "origin", "origin+destination", or "all"
#' @param zones "pt", "pt+drt" whether to filter by pt stops only or pt stops and drt zones
#' @param drt_zones the sf polygon layer of the drt service area
#' @param pre_enriched Logical - if TRUE, assumes trips already has spatial flags
#'
#' @return Tibble with mode, n, and share columns
#'
mode_share_catchment <- function(
  trips,
  stops,
  catchment_radius = 500,
  level = c("trip", "person", "all"),
  access = c("origin", "origin+destination", "all"),
  zones = c("pt", "pt+drt"),
  drt_zones = NULL,
  pre_enriched = FALSE
) {
  level <- match.arg(level)
  access <- match.arg(access)
  zones <- match.arg(zones)

  # Early exit for "all" scenario
  if (level == "all" && access == "all") {
    message("Using all trips (no filtering)...")
    return(
      trips |>
        count(mode) |>
        mutate(share = n / sum(n) * 100)
    )
  }

  # Use pre-enriched spatial flags if available
  if (!pre_enriched) {
    message("Creating unique trip id...")
    trips <- trips |>
      mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

    message("Buffering stops...")
    buffer <- stops |> st_buffer(catchment_radius, nQuadSegs = 3)

    if (zones == "pt+drt") {
      if (is.null(drt_zones)) {
        stop("zones = 'pt+drt' requires drt_zones (sf) to be provided.")
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
  } else {
    # Use pre-computed spatial flags
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
  }

  if (level == "person") {
    message("Filtering by person (all trips inside)...")
    total_trips_per_person <- trips |> count(person_id, name = "total_trips")
    filtered_trips_per_person <- trips_access |>
      count(person_id, name = "filtered_trips")
    persons_all_in <- filtered_trips_per_person |>
      inner_join(total_trips_per_person, by = "person_id") |>
      filter(filtered_trips == total_trips) |>
      pull(person_id)
    trips_access <- trips_access |> filter(person_id %in% persons_all_in)
  }

  message("Counting mode share...")
  trips_access |>
    count(mode) |>
    mutate(share = n / sum(n) * 100)
}

#' Pre-calculate spatial flags for a trips dataframe
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

#' Calculate mode share for multiple parameter combinations
#'
#' @param trips_file Path to trips CSV file
#' @param stops sf object with transit stop locations
#' @param catchment_radius Distance in meters
#' @param levels Vector of level values ("trip", "person", or both)
#' @param accesses Vector of access values ("origin", "origin+destination", or both)
#' @param include_all Whether to include "all" scenario with no filtering
#' @param zones  Whether to filter by pt stops only, or by pt and drt zones
#' @param drt_zones the drt zone sf polygon
#'
#' @return Tibble with results for all combinations
#'
mode_share_all_combinations <- function(
  trips_file,
  stops,
  catchment_radius = 500,
  levels = c("trip", "person"),
  accesses = c("origin", "origin+destination"),
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

  # Iterate over parameter combinations using pre-enriched data
  results <- expand_grid(level = levels, access = accesses, zone = zones) |>
    pmap_df(function(level, access, zone) {
      message("\n")
      message("#####")
      message(glue::glue(
        "Computing level={level}, access={access}, zones={zone}"
      ))
      message("#####")
      message("\n")

      mode_share_catchment(
        trips = trips_enriched,
        stops = stops,
        catchment_radius = catchment_radius,
        level = level,
        access = access,
        zones = zone,
        drt_zones = drt_zones,
        pre_enriched = TRUE
      ) |>
        mutate(
          level = level,
          access = access,
          zones = zone,
          .before = everything()
        )
    })

  if (include_all) {
    message("\n")
    message("#####")
    message(glue::glue("Computing level=all, access=all, zones=all"))
    message("#####")
    message("\n")
    all_result <- mode_share_catchment(
      trips = trips, # Use original trips, not enriched (doesn't need flags)
      stops = stops,
      catchment_radius = catchment_radius,
      level = "all",
      access = "all",
      zones = "pt",
      drt_zones = drt_zones,
      pre_enriched = FALSE
    ) |>
      mutate(
        level = "all",
        access = "all",
        zones = "all",
        .before = everything()
      )

    results <- bind_rows(results, all_result)
  }

  results
}


#' Calculate mode share for all solutions in a directory
#'
#' @param solutions_dir Directory containing solution subdirectories
#' @param stops sf object with transit stop locations
#' @param catchment_radius Distance in meters
#' @param levels Vector of level values
#' @param accesses Vector of access values
#' @param include_all Whether to include "all" scenario with no filtering
#'
#' @return Tibble with solution, level, access, mode, n, and share columns
#'
mode_share_by_solution <- function(
  solutions_dir,
  stops,
  catchment_radius = 500,
  levels = c("trip", "person"),
  accesses = c("origin", "origin+destination"),
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

    # Wrap in tryCatch to skip corrupted files
    tryCatch({
      mode_share_all_combinations(
        trips_file = trips_file,
        stops = stops,
        catchment_radius = catchment_radius,
        levels = levels,
        accesses = accesses,
        include_all = include_all,
        zones = zones,
        drt_zones = drt_zones
      ) |>
        mutate(solution = basename(sol_dir), .before = everything())
    }, error = function(e) {
      message(glue::glue("⚠️  SKIPPING {basename(sol_dir)}: {e$message}"))
      return(NULL)
    })
  })
}
