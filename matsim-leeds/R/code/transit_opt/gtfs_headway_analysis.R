library(tidyverse)
library(sf)
library(tidytransit)
library(glue)

#' Calculate headway by route and time interval from GTFS
#'
#' @param gtfs A tidygtfs object (from tidytransit::read_gtfs)
#' @param interval_hours Integer. Number of hours per interval. Must divide evenly into 24.
#'
#' @return An sf object with route geometries and headway_minutes for each interval
#'
calculate_route_headway <- function(gtfs, interval_hours = 1) {
  message("Validating interval_hours parameter...")

  # Validate interval_hours
  if (
    !is.numeric(interval_hours) ||
      interval_hours != as.integer(interval_hours) ||
      interval_hours <= 0
  ) {
    stop("interval_hours must be a positive integer")
  }
  if (24 %% interval_hours != 0) {
    stop(
      "interval_hours must divide evenly into 24 (valid: 1, 2, 3, 4, 6, 8, 12, 24)"
    )
  }

  message("Converting GTFS to sf object...")
  gtfs_sf <- tidytransit::gtfs_as_sf(gtfs, crs = 3857)

  message("Extracting route geometries...")
  gtfs_routes <- gtfs_sf$trips |>
    select(route_id, shape_id) |>
    distinct(route_id, .keep_all = TRUE) |>
    left_join(gtfs_sf$shapes, by = "shape_id") |>
    st_as_sf()

  message("Extracting trips with route IDs...")
  trips_with_routes <- gtfs$trips |>
    select(trip_id, route_id)

  message(glue::glue(
    "Calculating headway for {interval_hours}-hour intervals..."
  ))

  # Calculate seconds per interval
  seconds_per_interval <- interval_hours * 3600

  # Calculate minutes per interval
  minutes_per_interval <- interval_hours * 60

  # Calculate headway by route and interval
  headway_by_route_interval <- gtfs$stop_times |>
    filter(stop_sequence == 0) |>
    left_join(trips_with_routes, by = "trip_id") |>
    mutate(
      departure_seconds_num = as.numeric(departure_seconds),
      interval_num = floor(departure_seconds_num / seconds_per_interval),
      interval_label = paste0(
        interval_num * interval_hours,
        "-",
        (interval_num + 1) * interval_hours
      )
    ) |>
    group_by(route_id, interval_num, interval_label) |>
    summarise(
      num_trips = n(),
      .groups = "drop"
    ) |>
    mutate(
      headway_minutes = minutes_per_interval / num_trips
    ) |>
    ungroup()

  message("Joining headway data with route geometries...")

  # Join back to route geometries
  gtfs_routes_with_headway <- gtfs_routes |>
    left_join(headway_by_route_interval, by = "route_id")

  message("✓ Headway calculation complete")

  return(gtfs_routes_with_headway)
}


#' Aggregate route headways using overline
#'
#' @param gtfs_headways An sf object from calculate_route_headway()
#' @param attrib Character vector of attributes to aggregate (default: "num_trips")
#' @param fun Aggregation function(s) (default: sum)
#' @param ncores Number of cores for parallel processing (default: 3)
#'
#' @return An sf object with aggregated route network
#'
aggregate_route_headways <- function(
  gtfs_headways,
  attrib = "num_trips",
  fun = sum,
  ncores = 3
) {
  message("Aggregating route headways using overline...")

  gtfs_headways_overline <- gtfs_headways |>
    group_by(interval_label) |>
    nest() |>
    mutate(
      rnet = map(
        data,
        ~ stplanr::overline(
          sl = .x,
          attrib = attrib,
          ncores = ncores,
          fun = fun
        )
      )
    ) |>
    select(-data) |>
    unnest(rnet) |>
    ungroup() |>
    st_as_sf()

  message("✓ Aggregation complete")

  return(gtfs_headways_overline)
}


#' Calculate headway for a GTFS zip file
#'
#' @param gtfs_path Path to GTFS zip file
#' @param interval_hours Integer. Number of hours per interval.
#'
#' @return An sf object with route geometries and headway_minutes for each interval
#'
calculate_headway_from_zip <- function(gtfs_path, interval_hours = 4) {
  message(glue::glue("Reading GTFS from: {gtfs_path}"))
  gtfs <- tidytransit::read_gtfs(gtfs_path)
  calculate_route_headway(gtfs, interval_hours = interval_hours)
}


#' Compare headways between a solution and base GTFS
#'
#' @param solution_headways sf object with headways for the solution
#' @param base_headways sf object with headways for the base scenario
#'
#' @return sf object with diff columns added
#'
compare_headways <- function(solution_headways, base_headways) {
  # Drop geometry from base for joining
  base_data <- base_headways |>
    st_drop_geometry() |>
    select(
      route_id,
      interval_label,
      num_trips_base = num_trips,
      headway_minutes_base = headway_minutes
    )

  # Join and calculate differences
  solution_headways |>
    left_join(base_data, by = c("route_id", "interval_label")) |>
    mutate(
      # Handle missing base values (new routes in solution)
      num_trips_base = replace_na(num_trips_base, 0),
      headway_minutes_base = replace_na(headway_minutes_base, Inf),
      # Calculate differences
      num_trips_diff = num_trips - num_trips_base,
      headway_minutes_diff = headway_minutes - headway_minutes_base
    )
}


#' Calculate max concurrent trips (proxy for fleet size) by interval
#'
#' Estimates the Peak Vehicle Requirement (PVR) for the bus network.
#' Calculates PVR per route (assuming no interlining between routes) and sums them up.
#'
#' UPDATED: Now uses the same formula-based logic as the Python optimization code
#' to ensure consistent fleet sizing: ceil((RTT * 1.15) / headway).
#' The legacy empirical method (counting overlapping trip boxes) is preserved in comments.
#'
#' @param gtfs A tidygtfs object
#' @param interval_hours Integer
#'
#' @return Tibble with interval_label and max_bus_fleet (sum of per-route PVRs)
#'
calculate_max_concurrent_trips <- function(gtfs, interval_hours = 1) {
  # --- NEW IMPLEMENTATION: MATCHING PYTHON OPTIMIZATION LOGIC ---

  # 1. Calculate Average Round Trip Time (RTT) per Route
  # We estimate this from the GTFS content
  route_stats <- gtfs$stop_times |>
    # Get departure of first stop and arrival of last stop per trip
    group_by(trip_id) |>
    summarise(
      start_seconds = min(departure_seconds, na.rm = TRUE),
      end_seconds = max(arrival_time, na.rm = TRUE),
      duration_sec = end_seconds - start_seconds,
      .groups = "drop"
    ) |>
    left_join(select(gtfs$trips, trip_id, route_id), by = "trip_id") |>
    # Average duration per route (RTT)
    group_by(route_id) |>
    summarise(
      avg_rtt_sec = mean(duration_sec, na.rm = TRUE),
      .groups = "drop"
    )

  # 2. Calculate Frequency per Interval
  seconds_per_interval <- interval_hours * 3600
  num_intervals <- ceiling((24 * 3600) / seconds_per_interval)

  # Determine time intervals
  intervals <- tibble(
    interval_num = 0:(num_intervals - 1)
  ) |>
    mutate(
      start_sec = interval_num * seconds_per_interval,
      end_sec = (interval_num + 1) * seconds_per_interval,
      interval_label = paste0(
        interval_num * interval_hours,
        "-",
        (interval_num + 1) * interval_hours
      )
    )

  # Assign trips to intervals based on start time
  trips_with_routes <- gtfs$trips |> select(trip_id, route_id)

  trip_counts <- gtfs$stop_times |>
    filter(stop_sequence == 0) |> # Start of trip
    left_join(trips_with_routes, by = "trip_id") |>
    mutate(
      dep_sec = as.numeric(departure_seconds),
      interval_num = floor(dep_sec / seconds_per_interval)
    ) |>
    group_by(route_id, interval_num) |>
    summarise(num_trips = n(), .groups = "drop")

  # 3. Apply Fleet Formula: Ceiling( (RTT * 1.15) / Headway )
  # Headway = (Interval Duration) / Num Trips
  # Therefore: Fleet = Ceiling( (RTT * 1.15 * Num_Trips) / Interval_Duration )

  OPERATIONAL_BUFFER <- 1.15

  fleet_per_route_interval <- expand_grid(
    route_id = unique(trips_with_routes$route_id),
    interval_cols = intervals
  ) |>
    select(route_id, interval_num, interval_label) |>
    left_join(trip_counts, by = c("route_id", "interval_num")) |>
    replace_na(list(num_trips = 0)) |>
    left_join(route_stats, by = "route_id") |>
    mutate(
      # Standard Python Formula:
      # vehicles = ceil( (RTT * buffer) / headway )
      # vehicles = ceil( (RTT * buffer * n_trips) / interval_duration )
      raw_vehicles = (avg_rtt_sec * OPERATIONAL_BUFFER * num_trips) /
        seconds_per_interval,
      vehicles_needed = ceiling(raw_vehicles),

      # Python rule: max(1, int(vehicles)) if service is active (>0 trips)
      vehicles_needed = if_else(num_trips > 0, pmax(1, vehicles_needed), 0)
    )

  # 4. Aggregate
  total_fleet <- fleet_per_route_interval |>
    group_by(interval_label) |>
    summarise(max_bus_fleet = sum(vehicles_needed, na.rm = TRUE)) |>
    mutate(
      start_hour = as.numeric(str_extract(interval_label, "^\\d+"))
    ) |>
    arrange(start_hour) |>
    select(-start_hour)

  return(total_fleet)

  # --- LEGACY IMPLEMENTATION: EMPIRICAL PVR CALCULATION ---
  # Preserved for reference/reversion if needed.
  # This method calculates PVR by creating time-windows for every trip
  # (Duration + Buffer) and counting overlaps.

  # # 1. Get trip durations and route info
  # trip_times <- gtfs$stop_times |>
  #   group_by(trip_id) |>
  #   summarise(
  #     start_seconds = as.numeric(min(departure_seconds, na.rm = TRUE)),
  #     raw_end_seconds = as.numeric(max(arrival_time, na.rm = TRUE))
  #   ) |>
  #   mutate(
  #     duration = raw_end_seconds - start_seconds,
  #     # Dynamic buffer: max(10 mins, 15% of duration)
  #     # This accounts for recovery, layover, or charging/refueling proportional to distance/time
  #     buffer = pmax(600, duration * 0.15),
  #     end_seconds = raw_end_seconds + buffer
  #   ) |>
  #   ungroup() |>
  #   inner_join(select(gtfs$trips, trip_id, route_id), by = "trip_id") |>
  #   filter(!is.na(start_seconds), !is.na(end_seconds))
  #
  # # 2. Define intervals helper (Daily 24h cycle)
  # seconds_per_interval <- interval_hours * 3600
  # num_intervals <- ceiling((24 * 3600) / seconds_per_interval)
  #
  # intervals <- tibble(
  #   interval_num = 0:(num_intervals - 1)
  # ) |>
  #   mutate(
  #     start_time = interval_num * seconds_per_interval,
  #     end_time = (interval_num + 1) * seconds_per_interval,
  #     interval_label = paste0(interval_num * interval_hours, "-", (interval_num + 1) * interval_hours)
  #   )
  #
  # # 3. Helper to calculate PVR for a single route's trips
  # calc_route_pvr <- function(rt_trips) {
  #   if(nrow(rt_trips) == 0) return(tibble(interval_label = intervals$interval_label, pvr = 0))
  #
  #   events <- bind_rows(
  #     rt_trips |> select(t = start_seconds) |> mutate(change = 1),
  #     rt_trips |> select(t = end_seconds) |> mutate(change = -1)
  #   ) |>
  #     arrange(t) |>
  #     mutate(current_fleet = cumsum(change))
  #
  #   map_dfr(1:nrow(intervals), function(i) {
  #     s <- intervals$start_time[i]
  #     e <- intervals$end_time[i]
  #     lbl <- intervals$interval_label[i]
  #
  #     # State at start of interval
  #     start_val_df <- events |> filter(t < s) |> slice_tail(n = 1)
  #     start_val <- if(nrow(start_val_df) > 0) start_val_df$current_fleet else 0
  #
  #     # Events within interval
  #     events_in <- events |> filter(t >= s, t < e)
  #
  #     max_in_interval <- if(nrow(events_in) > 0) {
  #       max(c(start_val, events_in$current_fleet))
  #     } else {
  #       start_val
  #     }
  #
  #     tibble(interval_label = lbl, pvr = max_in_interval)
  #   })
  # }
  #
  # # 4. Apply to each route and aggregate
  # message("  Calculating PVR per route...")
  #
  # # Group trips by route
  # # We use split/map for safety over group_modify in some dplyr versions with simple dataframes
  # trips_by_route <- split(trip_times, trip_times$route_id)
  #
  # all_route_pvrs <- map_dfr(trips_by_route, calc_route_pvr)
  #
  # # Sum PVRs across all routes for each interval
  # total_fleet <- all_route_pvrs |>
  #   group_by(interval_label) |>
  #   summarise(max_bus_fleet = sum(pvr)) |>
  #   ungroup() |>
  #   # Ensure correct ordering by interval
  #   mutate(start_hour = as.numeric(str_extract(interval_label, "^\\d+"))) |>
  #   arrange(start_hour) |>
  #   select(-start_hour)
  #
  # return(total_fleet)
}


#' Compare fleet sizes
#' @param sol_fleet Tibble with solution fleet
#' @param base_fleet Tibble with base fleet
compare_fleet <- function(sol_fleet, base_fleet) {
  base_clean <- base_fleet |>
    rename(max_bus_fleet_base = max_bus_fleet)

  sol_fleet |>
    left_join(base_clean, by = "interval_label") |>
    mutate(
      max_bus_fleet_base = replace_na(max_bus_fleet_base, 0),
      max_bus_fleet_diff = max_bus_fleet - max_bus_fleet_base
    )
}


#' Process all GTFS solutions in a directory and compare to base
#'
#' @param base_gtfs_path Path to base GTFS zip file
#' @param solutions_parent_dir Parent directory containing objective folders
#' @param objective_names Vector of objective folder names to process
#' @param interval_hours Integer. Number of hours per interval.
#' @param iteration_folder Name of the iteration folder (default: "iteration_01")
#'
#' @return A list with base_headways, all_solutions (headways), base_fleet, and all_solutions_fleet
#'
process_all_gtfs_solutions <- function(
  base_gtfs_path,
  solutions_parent_dir = "../../transit_opt/output",
  objective_names = NULL,
  interval_hours = 4,
  iteration_folder = "iteration_01"
) {
  # Calculate base headways
  message("========================================")
  message("Processing BASE GTFS...")
  message("========================================")

  message(glue::glue("Reading Base GTFS from: {base_gtfs_path}"))
  base_gtfs <- tidytransit::read_gtfs(base_gtfs_path)

  base_headways <- calculate_route_headway(base_gtfs, interval_hours)
  base_fleet <- calculate_max_concurrent_trips(base_gtfs, interval_hours)
  message("✓ Base fleet size calculated")

  # Find all objective directories if not specified
  if (is.null(objective_names)) {
    objective_names <- list.dirs(
      solutions_parent_dir,
      full.names = FALSE,
      recursive = FALSE
    )
    objective_names <- objective_names[grepl(
      "sc_avg_var|sc_int_var|sc_peak_var|sc_sum_var|wt_avg_tot|wt_avg_var|wt_avg_atk|wt_int_tot|wt_int_var|wt_int_atk|wt_peak_tot|wt_peak_var|wt_peak_atk|wt_sum_tot|wt_sum_var|wt_sum_atk",
      objective_names
    )]
  }

  # Process each objective and solution
  # collecting lists first to avoid repeated map_dfr merging issues
  results_nested <- map(objective_names, function(obj_name) {
    message("\n========================================")
    message(glue::glue("Processing objective: {obj_name}"))
    message("========================================\n")

    pso_results_dir <- file.path(
      solutions_parent_dir,
      obj_name,
      iteration_folder,
      "pso_results"
    )

    # Find all combined_solution*.zip files
    solution_files <- list.files(
      pso_results_dir,
      pattern = "^combined_solution.*\\.zip$",
      full.names = TRUE
    )

    if (length(solution_files) == 0) {
      message(glue::glue("No solution files found in {pso_results_dir}"))
      return(NULL)
    }

    # Process each solution
    map(solution_files, function(sol_path) {
      solution_name <- tools::file_path_sans_ext(basename(sol_path))
      message(glue::glue("  Processing: {solution_name}"))

      tryCatch(
        {
          # Read GTFS once
          gtfs <- tidytransit::read_gtfs(sol_path)

          # 1. Headways
          sol_headways <- calculate_route_headway(gtfs, interval_hours)
          sol_compared <- compare_headways(sol_headways, base_headways)

          # Add identifiers
          sol_compared <- sol_compared |>
            mutate(
              objective = obj_name,
              solution = solution_name,
              .before = everything()
            )

          # 2. Fleet
          sol_fleet <- calculate_max_concurrent_trips(gtfs, interval_hours)
          sol_fleet_compared <- compare_fleet(sol_fleet, base_fleet)

          sol_fleet_compared <- sol_fleet_compared |>
            mutate(
              objective = obj_name,
              solution = solution_name,
              .before = everything()
            )

          list(
            headways = sol_compared,
            fleet = sol_fleet_compared
          )
        },
        error = function(e) {
          message(glue::glue(
            "    ⚠️ Error processing {solution_name}: {e$message}"
          ))
          return(NULL)
        }
      )
    })
  })

  # Flatten and combine results
  results_flat <- unlist(results_nested, recursive = FALSE)
  results_flat <- results_flat[!sapply(results_flat, is.null)]

  if (length(results_flat) == 0) {
    return(list(
      base_headways = base_headways,
      all_solutions = NULL,
      base_fleet = base_fleet,
      all_solutions_fleet = NULL
    ))
  }

  all_solutions_headways <- map_dfr(results_flat, "headways")
  all_solutions_fleet <- map_dfr(results_flat, "fleet")

  list(
    base_headways = base_headways,
    all_solutions = all_solutions_headways,
    base_fleet = base_fleet,
    all_solutions_fleet = all_solutions_fleet
  )
}

#' Aggregate solution headway comparisons using overline
#'
#' @param solutions_df Dataframe from process_all_gtfs_solutions()$all_solutions
#' @param attrib Character vector of attributes to aggregate
#' @param ncores Number of cores for parallel processing
#'
#' @return An sf object with aggregated route network per objective/solution/interval
#'
aggregate_solution_comparisons <- function(
  solutions_df,
  attrib = c("num_trips", "num_trips_diff"),
  ncores = 3
) {
  message("Aggregating solution comparisons using overline...")

  # Count total groups for progress tracking
  grouped_data <- solutions_df |>
    group_by(objective, solution, interval_label) |>
    nest()

  total_groups <- nrow(grouped_data)
  message(glue::glue(
    "Processing {total_groups} solution-interval combinations...\n"
  ))

  # Add counter for progress
  counter <- 0

  result <- grouped_data |>
    mutate(
      rnet = map(
        data,
        ~ {
          counter <<- counter + 1
          message(glue::glue(
            "  [{counter}/{total_groups}] Processing {objective} | {solution} | {interval_label}"
          ))

          stplanr::overline(
            sl = .x,
            attrib = attrib,
            ncores = ncores,
            fun = sum
          )
        }
      )
    ) |>
    select(-data) |>
    unnest(rnet) |>
    ungroup() |>
    st_as_sf()

  message("\n✓ Aggregation complete")

  return(result)
}


#' Load DRT fleet deployment from JSON file
#'
#' @param drt_json_path Path to DRT JSON file
#'
#' @return Tibble with zone_id, interval_label, fleet_size
#'
load_drt_deployment <- function(drt_json_path) {
  message(glue::glue("Reading DRT deployment from: {drt_json_path}"))

  if (!file.exists(drt_json_path)) {
    warning(glue::glue("DRT JSON file not found: {drt_json_path}"))
    return(NULL)
  }

  drt_data <- jsonlite::fromJSON(drt_json_path)

  # Extract fleet deployment for each zone
  drt_solutions <- drt_data$drt_solutions

  map_dfr(names(drt_solutions), function(zone_name) {
    zone_data <- drt_solutions[[zone_name]]
    fleet_deployment <- zone_data$fleet_deployment

    map_dfr(names(fleet_deployment), function(interval) {
      tibble(
        zone_id = zone_name,
        interval_label_drt = interval,
        fleet_size = fleet_deployment[[interval]]$fleet_size
      )
    })
  }) |>
    # Standardize interval labels to match GTFS format (e.g., "08-12h" -> "8-12")
    mutate(
      interval_label = interval_label_drt |>
        str_remove("h$") |>
        str_replace("^0(\\d)", "\\1") |> # Remove leading zero from start hour
        str_replace("-(0)(\\d)", "-\\2") # Remove leading zero from end hour
    ) |>
    # Map zone_id to match DRT polygon scenario names
    mutate(
      scenario = case_when(
        zone_id == "drt_ne" ~ "drtNE",
        zone_id == "drt_nw" ~ "drtNW",
        TRUE ~ zone_id
      )
    )
}


#' Load DRT deployments for all solutions in a directory
#'
#' @param solutions_parent_dir Parent directory containing objective folders
#' @param objective_names Vector of objective folder names to process
#' @param iteration_folder Name of the iteration folder (default: "iteration_01")
#'
#' @return Tibble with objective, solution, zone_id, interval_label, fleet_size
#'
load_all_drt_deployments <- function(
  solutions_parent_dir = "../../transit_opt/output",
  objective_names = NULL,
  iteration_folder = "iteration_01"
) {
  message("Loading all DRT deployments...")

  # Find all objective directories if not specified
  if (is.null(objective_names)) {
    objective_names <- list.dirs(
      solutions_parent_dir,
      full.names = FALSE,
      recursive = FALSE
    )
    objective_names <- objective_names[grepl(
      "sc_avg_var|sc_int_var|sc_peak_var|sc_sum_var|wt_avg_tot|wt_avg_var|wt_avg_atk|wt_int_tot|wt_int_var|wt_int_atk|wt_peak_tot|wt_peak_var|wt_peak_atk|wt_sum_tot|wt_sum_var|wt_sum_atk",
      objective_names
    )]
  }

  map_dfr(objective_names, function(obj_name) {
    message(glue::glue("  Processing objective: {obj_name}"))

    pso_results_dir <- file.path(
      solutions_parent_dir,
      obj_name,
      iteration_folder,
      "pso_results"
    )

    # Find all DRT JSON files
    drt_files <- list.files(
      pso_results_dir,
      pattern = "^combined_solution.*_drt\\.json$",
      full.names = TRUE
    )

    if (length(drt_files) == 0) {
      message(glue::glue("    No DRT JSON files found in {pso_results_dir}"))
      return(NULL)
    }

    map_dfr(drt_files, function(drt_path) {
      solution_name <- basename(drt_path) |>
        str_remove("_drt\\.json$")

      tryCatch(
        {
          load_drt_deployment(drt_path) |>
            mutate(
              objective = obj_name,
              solution = solution_name,
              .before = everything()
            )
        },
        error = function(e) {
          message(glue::glue(
            "    ⚠️ Error loading {solution_name}: {e$message}"
          ))
          return(NULL)
        }
      )
    })
  })
}


#' Join DRT fleet deployment to DRT zone polygons
#'
#' @param drt_zones sf object with DRT zone polygons (must have 'scenario' column)
#' @param drt_deployments Tibble from load_all_drt_deployments()
#' @param objective Filter to specific objective (optional)
#' @param solution Filter to specific solution (optional)
#' @param interval_label Filter to specific interval (optional)
#'
#' @return sf object with fleet_size joined
#'
join_drt_fleet_to_zones <- function(
  drt_zones,
  drt_deployments,
  objective = NULL,
  solution = NULL,
  interval_label = NULL
) {
  # Apply filters if specified
  filtered_deployments <- drt_deployments

  if (!is.null(objective)) {
    filtered_deployments <- filtered_deployments |>
      filter(objective == !!objective)
  }

  if (!is.null(solution)) {
    filtered_deployments <- filtered_deployments |>
      filter(solution == !!solution)
  }

  if (!is.null(interval_label)) {
    filtered_deployments <- filtered_deployments |>
      filter(interval_label == !!interval_label)
  }

  # Join to zones
  drt_zones |>
    left_join(
      filtered_deployments,
      by = "scenario"
    )
}
