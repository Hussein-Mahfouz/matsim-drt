library(tidyverse)
library(glue)

# Directory for saving plots
plot_dir <- "plots/stuck_analysis"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================

message("Loading Baseline Demand...")
demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
)

# Create trip_id for baseline
demand_original <- demand_original |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

all_baseline_trips <- unique(demand_original$trip_id)

# Setup Scenarios
scenarios <- c("zones", "all", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ==============================================================================
# 2. LOAD COMPLETED TRIPS & IDENTIFY MISSING ONES
# ==============================================================================

read_trips <- function(scenario, fleet_size) {
  file_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/eqasim_trips.csv"
  )

  if (!file.exists(file_path)) {
    return(NULL)
  }

  read_delim(file_path, delim = ";", show_col_types = FALSE) |>
    select(person_id, person_trip_id) |> # Optimize memory, we only need IDs
    mutate(
      scenario = scenario,
      fleet_size = fleet_size,
      trip_id = paste(person_id, person_trip_id, sep = "_")
    )
}

message("Reading output trips from all scenarios...")
demand_matsim <- purrr::pmap_dfr(combinations, read_trips)

# --- Find Missing Trips ---
message("Identifying missing trips...")

# Get unique completed trips per scenario
completed_lookup <- demand_matsim |>
  distinct(scenario, fleet_size, trip_id)

# Create a full grid of intended trips
# (We process per scenario to avoid massive cross-join memory issues)
find_missing_per_scenario <- function(scenario, fleet_size) {
  # Assign to local variables to avoid name collisions in filter()
  target_scen <- scenario
  target_fs <- fleet_size

  completed <- completed_lookup |>
    filter(scenario == target_scen, fleet_size == target_fs) |>
    pull(trip_id)

  missing_ids <- setdiff(all_baseline_trips, completed)

  if (length(missing_ids) > 0) {
    tibble(
      trip_id = missing_ids,
      scenario = target_scen,
      fleet_size = target_fs,
      person_id = as.numeric(str_split_i(missing_ids, "_", 1)) # Extract person_id for linking
    )
  } else {
    NULL
  }
}

missing_log <- purrr::pmap_dfr(combinations, find_missing_per_scenario)

message(glue(
  "Total missing trip instances found: {scales::comma(nrow(missing_log))}"
))

# ==============================================================================
# 3. LOAD DRT REJECTIONS
# ==============================================================================

read_rejections <- function(scenario, fleet_size) {
  dir_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/"
  )

  # Find all files matching the rejection pattern (e.g., _drt.csv, _drtNE.csv)
  files <- list.files(
    dir_path,
    pattern = "output_drt_rejections_.*\\.csv",
    full.names = TRUE
  )

  if (length(files) == 0) {
    return(NULL)
  }

  # Read and bind all rejection files for this scenario
  map_dfr(files, ~ read_delim(.x, delim = ";", show_col_types = FALSE)) |>
    mutate(
      scenario = scenario,
      fleet_size = fleet_size
    ) |>
    select(time, personIds, fromLinkId, toLinkId, scenario, fleet_size)
}

message("Reading DRT rejection logs...")
drt_rejections <- purrr::pmap_dfr(combinations, read_rejections)

# Clean IDs for joining
drt_rejections <- drt_rejections |>
  mutate(person_id = as.numeric(personIds)) |>
  select(-personIds) # Remove original string ID

# Summarize rejections: Does this person have ANY rejection in this scenario?
rejection_lookup <- drt_rejections |>
  distinct(scenario, fleet_size, person_id) |>
  mutate(has_rejection = TRUE)

# ==============================================================================
# 4. LINK MISSING TRIPS TO REJECTIONS
# ==============================================================================

message("Categorizing missing trips...")

# Join missing log with rejection lookup
stuck_analysis <- missing_log |>
  left_join(rejection_lookup, by = c("scenario", "fleet_size", "person_id")) |>
  mutate(
    cause = if_else(
      !is.na(has_rejection),
      "DRT Rejection",
      "Other (Network/Stuck)"
    )
  )

# Join with original demand to get mode/distance info
stuck_analysis_detailed <- stuck_analysis |>
  left_join(
    demand_original |>
      select(trip_id, original_mode = mode, distance = euclidean_distance),
    by = "trip_id"
  )
# ==============================================================================
# 5. ANALYSIS & PLOTTING
# ==============================================================================

if (nrow(stuck_analysis_detailed) > 0) {
  # --- A. Summary Table: Breakdown by Trips AND People ---
  freq_summary <- stuck_analysis_detailed |>
    group_by(scenario, fleet_size, cause) |>
    summarise(
      missing_trips = n(),
      missing_people = n_distinct(person_id),
      .groups = "drop"
    ) |>
    group_by(scenario, fleet_size) |>
    mutate(
      pct_trips = round(missing_trips / sum(missing_trips) * 100, 1),
      pct_people = round(missing_people / sum(missing_people) * 100, 1)
    )

  print("Breakdown of Missing Data (Trips vs People):")
  print(freq_summary)

  write_csv(
    freq_summary,
    file.path(plot_dir, "missing_summary_trips_and_people.csv")
  )

  # --- B. Plot: Absolute Count (Facet by Metric) ---
  # Reshape for plotting side-by-side
  freq_long <- freq_summary |>
    pivot_longer(
      cols = c(missing_trips, missing_people),
      names_to = "metric",
      values_to = "count"
    ) |>
    mutate(
      metric_label = if_else(
        metric == "missing_trips",
        "Total Missing Trips",
        "Unique People Affected"
      )
    )

  p1 <- ggplot(
    freq_long,
    aes(x = factor(fleet_size), y = count, fill = cause)
  ) +
    geom_col() +
    facet_grid(metric_label ~ scenario, scales = "free_y") +
    theme_bw() +
    labs(
      title = "Missing Data by Cause: Trips vs People",
      subtitle = "Upper row: Trip instances lost. Lower row: Unique agents affected.",
      x = "Fleet Size",
      y = "Count",
      fill = "Likely Cause"
    ) +
    theme(legend.position = "bottom")

  print(p1)
  ggsave(
    file.path(plot_dir, "missing_counts_trips_vs_people.png"),
    p1,
    width = 10,
    height = 8
  )

  # --- C. Compare Original Modes (Trips vs People) ---
  # We look at those marked as "DRT Rejection"
  rejection_modes <- stuck_analysis_detailed |>
    filter(cause == "DRT Rejection") |>
    group_by(scenario, fleet_size, original_mode) |>
    summarise(
      missing_trips = n(),
      missing_people = n_distinct(person_id),
      .groups = "drop"
    ) |>
    pivot_longer(
      c(missing_trips, missing_people),
      names_to = "metric",
      values_to = "count"
    ) |>
    mutate(
      metric_label = if_else(
        metric == "missing_trips",
        "Missing Trips",
        "People Rejected"
      )
    )

  if (nrow(rejection_modes) > 0) {
    p2 <- ggplot(
      rejection_modes,
      aes(x = factor(fleet_size), y = count, fill = original_mode)
    ) +
      geom_col() +
      facet_grid(metric_label ~ scenario, scales = "free_y") +
      theme_bw() +
      labs(
        title = "Who is getting rejected by DRT?",
        subtitle = "Breakdown by the mode they used in the Baseline scenario",
        x = "Fleet Size",
        y = "Count",
        fill = "Original Baseline Mode"
      ) +
      theme(legend.position = "bottom")

    print(p2)
    ggsave(
      file.path(plot_dir, "rejections_by_mode_trips_vs_people.png"),
      p2,
      width = 10,
      height = 8
    )
  }

  message(paste("All analysis plots saved to:", plot_dir))
} else {
  message("No missing trips found across scenarios!")
}
