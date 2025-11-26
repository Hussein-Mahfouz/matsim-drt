library(tidyverse)
library(sf)

# 1. Read stops.txt and convert to sf
stops <- read_csv(unz(
  "../data/external/study_area_gtfs_bus.zip",
  "stops.txt"
)) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) |>
  st_transform(3857)

# get stop area boundary for cropping
boundary_sf = st_read("../data/external/study_area_boundary.geojson") |>
  st_transform(3857)

# Crop stops by study area boundary
stops = stops |>
  st_filter(boundary_sf, .predicate = st_within)

# 2. Read eqasim_trips.csv and convert origins to sf
# trips <- read_delim(
#   "../data/external/gtfs_optimisation/min_variance_stops/solution_01_gtfs/output/eqasim_trips.csv",
#   delim = ";"
# ) |>
#   st_as_sf(coords = c("origin_x", "origin_y"), crs = 3857)

# # 3. Spatial join: flag origins within 500m of any stop
# catchment_radius <- 250

# trips_near_pt <- trips |>
#   st_filter(stops |> st_buffer(catchment_radius), .predicate = st_intersects)

# # 4. Calculate PT mode share for people near stops
# pt_mode_share <- trips_near_pt |>
#   count(mode) |>
#   mutate(share = n / sum(n) * 100)

# pt_mode_share

trips <- read_delim(
  "../data/external/gtfs_optimisation/min_variance_stops/solution_01_gtfs/output/eqasim_trips.csv",
  delim = ";"
)

mode_share_catchment <- function(
  trips,
  stops,
  catchment_radius = 500,
  level = c("trip", "person"),
  access = c("origin", "origin+destination")
) {
  level <- match.arg(level)
  access <- match.arg(access)

  message("Creating unique trip id ...")
  trips <- trips |>
    mutate(unique_trip_id = paste0(person_id, "_", person_trip_id))

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
    message("Grouping and summarizing by person_id...")

    # Calculate total trips per person in full dataset
    total_trips_per_person <- trips |>
      count(person_id, name = "total_trips")

    # Calculate filtered trips per person
    filtered_trips_per_person <- trips_access |>
      count(person_id, name = "filtered_trips")

    # Keep only persons where all trips are in buffer
    persons_all_in <- filtered_trips_per_person |>
      inner_join(total_trips_per_person, by = "person_id") |>
      filter(filtered_trips == total_trips) |>
      pull(person_id)

    message("Filtering trips for persons with all trips in buffer...")
    trips_access <- trips_access |> filter(person_id %in% persons_all_in)
  }

  message("Counting mode share...")
  trips_access |>
    count(mode) |>
    mutate(share = n / sum(n) * 100)
}

pt_mode_share <- mode_share_catchment(
  trips = trips,
  stops = stops,
  catchment_radius = 100,
  level = "trip",
  access = "origin"
)

pt_mode_share2 <- mode_share_catchment(
  trips = trips,
  stops = stops,
  catchment_radius = 100,
  level = "trip",
  access = "origin+destination"
)

pt_mode_share3 <- mode_share_catchment(
  trips = trips,
  stops = stops,
  catchment_radius = 100,
  level = "person",
  access = "origin"
)

pt_mode_share4 <- mode_share_catchment(
  trips = trips,
  stops = stops,
  catchment_radius = 100,
  level = "person",
  access = "origin+destination"
)


mode_share_by_solution <- function(
  solutions_dir,
  stops,
  catchment_radius = 500,
  level = c("trip", "person"),
  access = c("origin", "origin+destination"),
  id_col = NULL
) {
  level <- match.arg(level)
  access <- match.arg(access)

  # Get all solution directories
  solution_dirs <- list.dirs(
    solutions_dir,
    full.names = TRUE,
    recursive = FALSE
  )
  solution_names <- basename(solution_dirs)

  # Read trips from each solution and calculate mode share
  results <- map_df(solution_dirs, function(sol_dir) {
    # Find trips file in solution directory
    trips_file <- list.files(
      sol_dir,
      pattern = "eqasim_trips\\.csv$",
      full.names = TRUE,
      recursive = TRUE
    )

    if (length(trips_file) == 0) {
      message(glue::glue("No eqasim_trips.csv found in {sol_dir}"))
      return(NULL)
    }

    trips_file <- trips_file[1] # Take first match if multiple

    message(glue::glue("Processing {basename(sol_dir)}..."))

    trips <- read_delim(trips_file, delim = ";")

    # Run mode share catchment analysis
    mode_share <- mode_share_catchment(
      trips = trips,
      stops = stops,
      catchment_radius = catchment_radius,
      level = level,
      access = access
    )

    # Extract parent directory name and solution name
    parent_dir <- basename(solutions_dir)
    solution_name <- basename(sol_dir)

    # Add identifiers and return
    mode_share |>
      mutate(
        analysis_set = parent_dir,
        solution = solution_name,
        .before = everything()
      )
  })

  results
}


all_solutions <- mode_share_by_solution(
  solutions_dir = "../data/external/gtfs_optimisation/min_variance_stops",
  stops = stops,
  catchment_radius = 100,
  level = "trip",
  access = "origin"
)

all_solutions
