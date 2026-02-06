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


#' Process all GTFS solutions in a directory and compare to base
#'
#' @param base_gtfs_path Path to base GTFS zip file
#' @param solutions_parent_dir Parent directory containing objective folders
#' @param objective_names Vector of objective folder names to process
#' @param interval_hours Integer. Number of hours per interval.
#' @param iteration_folder Name of the iteration folder (default: "iteration_01")
#'
#' @return A list with base_headways and a dataframe of all solution comparisons
#'
process_all_gtfs_solutions <- function(
  base_gtfs_path,
  solutions_parent_dir = "../../transit_opt/output",
  objective_names = NULL,
  interval_hours = 4,
  iteration_folder = "iteration_0"
) {
  # Calculate base headways
  message("========================================")
  message("Processing BASE GTFS...")
  message("========================================")
  base_headways <- calculate_headway_from_zip(base_gtfs_path, interval_hours)

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
  all_solutions <- map_dfr(objective_names, function(obj_name) {
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
    map_dfr(solution_files, function(sol_path) {
      solution_name <- tools::file_path_sans_ext(basename(sol_path))
      message(glue::glue("  Processing: {solution_name}"))

      tryCatch(
        {
          # Calculate headways for this solution
          sol_headways <- calculate_headway_from_zip(sol_path, interval_hours)

          # Compare to base
          sol_compared <- compare_headways(sol_headways, base_headways)

          # Add identifiers
          sol_compared |>
            mutate(
              objective = obj_name,
              solution = solution_name,
              .before = everything()
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

  list(
    base_headways = base_headways,
    all_solutions = all_solutions
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
