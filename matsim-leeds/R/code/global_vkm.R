library(tidyverse)
library(stringr)

# This script looks at change in VKM. We compare VKM before and after simulation.
# It uses "Person Consistency Imputation" to ensure that agents who get stuck
# in the simulation are counted as reverting to their baseline mode (e.g. Car),
# rather than vanishing from the analysis.

# Setup
scenarios <- c("zones", "all", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)

scenario_labels <- c(
  "zones" = "Zone-based DRT",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
)

modes <- c("car", "taxi")

# Filter type definitions
filter_definitions <- list(
  global = list(
    name = "Global",
    filter_fn = function(df, scenario) rep(TRUE, nrow(df))
  ),
  trip_touch = list(
    name = "Trip touches zone",
    filter_fn = function(df, scenario) {
      if (scenario == "zones") {
        return(df$filter_zones)
      }
      if (scenario == "innerBUA") {
        return(df$filter_innerBUA)
      }
      if (scenario == "all") {
        return(df$filter_all)
      }
      return(rep(FALSE, nrow(df)))
    }
  ),
  resident = list(
    name = "Resident",
    filter_fn = function(df, scenario) {
      if (scenario == "zones") {
        return(df$resident_zones)
      }
      if (scenario == "innerBUA") {
        return(df$resident_innerBUA)
      }
      if (scenario == "all") {
        return(df$resident_all)
      }
      return(rep(FALSE, nrow(df)))
    }
  )
)

# Load Spatial Lookup
lookup_path <- "../data/interim/trips_spatial_lookup.rds"
if (file.exists(lookup_path)) {
  spatial_lookup <- readRDS(lookup_path)
} else {
  stop(
    "Spatial lookup file not found. Please run code/prep_spatial_lookup.R first."
  )
}

# ------------------------------------------------------------------------------
# 1. READ DATA & PREPARE
# ------------------------------------------------------------------------------

# Read baseline
demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
)

# Join spatial lookup to baseline immediately
demand_original <- demand_original |>
  left_join(spatial_lookup, by = c("person_id", "person_trip_id"))

# Function to read MATSim output
read_matsim <- function(scenario, fleet_size) {
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

  message(paste("Reading:", scenario, fleet_size))
  read_delim(file_path, delim = ";", show_col_types = FALSE) |>
    mutate(scenario = scenario, fleet_size = fleet_size)
}

combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

demand_matsim <- purrr::pmap_dfr(combinations, read_matsim)

# Join spatial lookup to MATSim result immediately
demand_matsim <- demand_matsim |>
  left_join(spatial_lookup, by = c("person_id", "person_trip_id"))

# ------------------------------------------------------------------------------
# 2. PERSON CONSISTENCY CHECK & IMPUTATION (CRITICAL STEP)
# ------------------------------------------------------------------------------
# We do NOT filter to common trips (intersection). Instead, we check if a person
# completed their day. If not (stuck), we count them as "Baseline Mode" (reverted).

message(
  "Checking person consistency (Imputing baseline behavior for stuck agents)..."
)

# A. Baseline Counts
baseline_counts <- demand_original |> count(person_id, name = "n_expected")

# B. Sim Counts
sim_counts <- demand_matsim |>
  count(scenario, fleet_size, person_id, name = "n_actual")

# C. Valid People (Complete Plans)
valid_people_lookup <- sim_counts |>
  inner_join(baseline_counts, by = "person_id") |>
  filter(n_actual == n_expected) |>
  select(scenario, fleet_size, person_id) |>
  mutate(is_valid = TRUE)

# D. Filter MATSim Data (Keep only valid)
demand_matsim_valid <- demand_matsim |>
  inner_join(valid_people_lookup, by = c("scenario", "fleet_size", "person_id"))

# E. Generate Reverted Trips (The "Penalized" VKM)
# For people who failed, we take their BASELINE rows and add them to the scenario results.
# This means if they drove a car in baseline, they generate Car VKM in the scenario too.

# Function to grab missing people for a specific scenario group
get_reverted <- function(curr_scen, curr_fs) {
  valid_ids <- valid_people_lookup |>
    filter(scenario == curr_scen, fleet_size == curr_fs) |>
    pull(person_id)

  # Take baseline rows for INVALID people
  demand_original |>
    filter(!person_id %in% valid_ids) |>
    mutate(
      scenario = curr_scen,
      fleet_size = curr_fs
      # mode stays as original mode (e.g. car)
      # routed_distance stays as original distance
    )
}

scen_combos <- distinct(demand_matsim, scenario, fleet_size)

demand_matsim_reverted <- purrr::pmap_dfr(
  scen_combos,
  function(scenario, fleet_size) {
    get_reverted(scenario, fleet_size)
  }
)

# F. Combine
demand_matsim_final <- bind_rows(demand_matsim_valid, demand_matsim_reverted)

message(sprintf(
  "Imputation Complete. Analyzed Trips: %s",
  scales::comma(nrow(demand_matsim_final))
))

# ------------------------------------------------------------------------------
# 3. DRT VKM (ADDITIVE)
# ------------------------------------------------------------------------------
# DRT VKM is calculated from vehicle movements, independent of successful passenger trips.

read_drt_vkm <- function(scenario, fleet_size) {
  file_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/eqasim_drt_vehicle_movements.csv"
  )
  if (!file.exists(file_path)) {
    return(NULL)
  }
  read_delim(file_path, delim = ";", show_col_types = FALSE) |>
    mutate(scenario = scenario, fleet_size = fleet_size)
}

demand_matsim_drt <- purrr::pmap_dfr(combinations, read_drt_vkm)

drt_vkm_summary <- demand_matsim_drt |>
  group_by(fleet_size, scenario) |>
  summarise(
    total_distance_km = round(sum(distance) / 1000),
    .groups = "drop"
  ) |>
  mutate(
    mode = "drt",
    total_distance_km_orig = 0,
    delta_km = total_distance_km,
    pct_change = NA_real_
  )

# ------------------------------------------------------------------------------
# 4. CALCULATE VKM CHANGE (HYBRID METHOD)
# ------------------------------------------------------------------------------

calculate_vkm <- function(orig_data, matsim_data, filter_type_name, filter_fn) {
  results_list <- list()

  for (scen in scenarios) {
    for (fs in fleet_sizes) {
      # 1. Filter Baseline (Denominator)
      keep_orig <- filter_fn(orig_data, scen)
      orig_filtered <- orig_data[keep_orig, ]

      # 2. Filter Scenario (Numerator - includes imputed baseline trips)
      matsim_subset <- matsim_data |> filter(scenario == scen, fleet_size == fs)
      keep_matsim <- filter_fn(matsim_subset, scen)
      matsim_filtered <- matsim_subset[keep_matsim, ]

      # 3. Calculate Baseline VKM (Car + Taxi)
      base_vkm <- orig_filtered |>
        filter(mode %in% modes) |>
        group_by(mode) |>
        summarise(
          total_distance_km_orig = sum(routed_distance, na.rm = TRUE) / 1000,
          .groups = "drop"
        )

      # 4. Calculate Scenario VKM (Car + Taxi)
      scenario_vkm <- matsim_filtered |>
        filter(mode %in% modes) |>
        group_by(mode) |>
        summarise(
          total_distance_km = sum(routed_distance, na.rm = TRUE) / 1000,
          .groups = "drop"
        )

      # 5. Combine and Calc Delta
      combined <- base_vkm |>
        full_join(scenario_vkm, by = "mode") |>
        mutate(
          scenario = scen,
          fleet_size = fs,
          filter_type = filter_type_name,
          delta_km = round(total_distance_km - total_distance_km_orig),
          pct_change = round(100 * delta_km / total_distance_km_orig)
        )

      # 6. Add DRT VKM (Only added to Total, not replacing anything)
      # DRT VKM is not filtered by "Resident" or "Trip Touch" because it's a service metric
      # representing the fleet cost. We apply it globally for simplicity, or
      # we could scale it, but usually VKM cost is a system-wide metric.
      drt_row <- drt_vkm_summary |>
        filter(scenario == scen, fleet_size == fs) |>
        mutate(filter_type = filter_type_name)

      combined <- bind_rows(combined, drt_row)

      # 7. Total Row
      total_row <- combined |>
        summarise(
          mode = "TOTAL",
          total_distance_km_orig = sum(total_distance_km_orig, na.rm = TRUE),
          total_distance_km = sum(total_distance_km, na.rm = TRUE),
          scenario = scen,
          fleet_size = fs,
          filter_type = filter_type_name,
          # Recalculate delta/pct for the total sum
          delta_km = total_distance_km - total_distance_km_orig,
          pct_change = round(100 * delta_km / total_distance_km_orig)
        )

      results_list[[length(results_list) + 1]] <- bind_rows(combined, total_row)
    }
  }
  bind_rows(results_list)
}

# Run Calculation
all_vkm_results <- map2_dfr(
  names(filter_definitions),
  filter_definitions,
  ~ calculate_vkm(demand_original, demand_matsim_final, .x, .y$filter_fn)
)

# Save
write_csv(all_vkm_results, "plots/global_vkm/global_vkm_change.csv")

# ------------------------------------------------------------------------------
# 5. STANDARD PLOTS
# ------------------------------------------------------------------------------

# Plot 1: Total VKM Comparison
ggplot(
  all_vkm_results |> filter(mode == "TOTAL"),
  aes(x = factor(fleet_size), y = delta_km / 1000, fill = filter_type)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labels)) +
  labs(
    title = "Total VKM Change by Filter Type",
    subtitle = "Change in vehicle kilometers (Car + Taxi + DRT)",
    y = "Change in VKM (Thousands)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  "plots/global_vkm/comparison_total_vkm_by_filter.png",
  width = 10,
  height = 6
)
