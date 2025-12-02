# ============================================================================
# Functions
# ============================================================================

#' Calculate mode share for trips within transit stop catchment
#'
#' @param trips Dataframe with origin/destination coordinates
#' @param stops sf object with transit stop locations
#' @param catchment_radius Distance in meters to buffer stops
#' @param level "trip", "person", or "all" (all trips, no filtering)
#' @param access "origin", "origin+destination", or "all"
#' @param zones "pt", "pt+drt" whether to filter by pt stops only or pt stops and drt zones
#' @param drt_zones the sf polygon layyer of the drt service area
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
  drt_zones = NULL
) {
  level <- match.arg(level)
  access <- match.arg(access)
  zones <- match.arg(zones)

  message("Creating unique trip id...")
  trips <- trips |>
    mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

  # If level = "all" and access = "all", skip filtering entirely
  if (level == "all" && access == "all") {
    message("Using all trips (no filtering)...")
    trips_access <- trips
  } else {
    message("Buffering stops...")
    buffer <- stops |> st_buffer(catchment_radius)

    if (zones == "pt+drt") {
      if (is.null(drt_zones)) {
        stop("zones = 'pt+drt' requires drt_zones (sf) to be provided.")
      }
      message("Combining PT stop buffers and DRT zones...")
      drt_zones <- st_transform(drt_zones, st_crs(stops))
      # bind_rows keeps geometries separate so st_filter checks against all polygons/points
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
  }

  message("Counting mode share...")
  trips_access |>
    count(mode) |>
    mutate(share = n / sum(n) * 100)
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
  trips <- read_delim(trips_file, delim = ";", show_col_types = FALSE)

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
        trips = trips,
        stops = stops,
        catchment_radius = catchment_radius,
        level = level,
        access = access,
        zones = zone,
        drt_zones = drt_zones
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
      trips = trips,
      stops = stops,
      catchment_radius = catchment_radius,
      level = "all",
      access = "all",
      zones = "pt", # zones irrelevant for "all", keep default
      drt_zones = drt_zones
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
  })
}
