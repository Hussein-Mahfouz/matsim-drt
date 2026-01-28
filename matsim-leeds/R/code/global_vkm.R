library(tidyverse)
library(stringr)

# This script looks at change in VKM. We compare VKM before and after simulation.
# Outputs:
# - One combined CSV with all filter types identified by a 'filter_type' column
# - Comparison plots across filter types

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
      case_when(
        scenario == "zones" ~ df$filter_zones,
        scenario == "innerBUA" ~ df$filter_innerBUA,
        scenario == "all" ~ df$filter_all,
        TRUE ~ FALSE
      )
    }
  ),
  resident = list(
    name = "Resident",
    filter_fn = function(df, scenario) {
      case_when(
        scenario == "zones" ~ df$resident_zones,
        scenario == "innerBUA" ~ df$resident_innerBUA,
        scenario == "all" ~ df$resident_all,
        TRUE ~ FALSE
      )
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

# Read baseline
demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
)

demand_original <- demand_original |>
  left_join(spatial_lookup, by = c("person_id", "person_trip_id"))

# Read scenario data
read_and_process <- function(scenario, fleet_size, file_name) {
  file_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/",
    file_name,
    ".csv"
  )
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  print(paste("Reading file:", file_path))
  data <- read_delim(file_path, delim = ";", show_col_types = FALSE)
  data <- data |> mutate(scenario = scenario, fleet_size = fleet_size)
  return(data)
}

combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

demand_matsim <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_trips")
})

demand_matsim <- demand_matsim |>
  left_join(spatial_lookup, by = c("person_id", "person_trip_id"))

# # Filter to common people
# n_groups <- demand_matsim |> distinct(scenario, fleet_size) |> nrow()
# person_group_counts <- demand_matsim |>
#   distinct(person_id, scenario, fleet_size) |>
#   count(person_id, name = "n_groups_present") |>
#   filter(n_groups_present == n_groups)

# demand_matsim <- demand_matsim |>
#   filter(person_id %in% person_group_counts$person_id)
# demand_original <- demand_original |>
#   filter(person_id %in% person_group_counts$person_id)

# ---------- Filter to common trips across all scenarios + baseline

# Create unique trip ID
demand_original <- demand_original |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

demand_matsim <- demand_matsim |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

# Find trips that exist in ALL scenario-fleet combinations
n_combos <- demand_matsim |> distinct(scenario, fleet_size) |> nrow()

trips_in_all_scenarios <- demand_matsim |>
  distinct(trip_id, scenario, fleet_size) |>
  count(trip_id, name = "n_combos_present") |>
  filter(n_combos_present == n_combos) |>
  pull(trip_id)

# Also must exist in baseline
trips_in_baseline <- demand_original |> pull(trip_id) |> unique()

# Intersection: trips that exist in baseline AND all scenarios
common_trips <- intersect(trips_in_all_scenarios, trips_in_baseline)

message(sprintf(
  "Filtering to %s common trips (of %s baseline, %s across all scenarios)",
  scales::comma(length(common_trips)),
  scales::comma(length(trips_in_baseline)),
  scales::comma(length(trips_in_all_scenarios))
))

# Apply filter
demand_original <- demand_original |> filter(trip_id %in% common_trips)
demand_matsim <- demand_matsim |> filter(trip_id %in% common_trips)

# Verify
message("Trips per scenario after filtering:")
demand_matsim |>
  group_by(scenario, fleet_size) |>
  summarise(unique_trips = n_distinct(trip_id), .groups = "drop") |>
  print()


# DRT VKM (same for all filter types - vehicle-based, not trip-based)
demand_matsim_drt <- purrr::pmap_dfr(
  combinations,
  function(scenario, fleet_size) {
    read_and_process(scenario, fleet_size, "eqasim_drt_vehicle_movements")
  }
)

drt_vkm <- demand_matsim_drt |>
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

# ---------- Calculate VKM for all filter types

calculate_vkm <- function(orig_data, matsim_data, filter_type_name, filter_fn) {
  results_list <- list()

  for (scen in scenarios) {
    for (fs in fleet_sizes) {
      # Filter baseline data for this scenario's filter
      keep_orig <- filter_fn(orig_data, scen)
      orig_filtered <- orig_data[keep_orig, ]

      # Filter scenario data
      matsim_subset <- matsim_data |> filter(scenario == scen, fleet_size == fs)
      keep_matsim <- filter_fn(matsim_subset, scen)
      matsim_filtered <- matsim_subset[keep_matsim, ]

      # Calculate baseline VKM
      base_vkm <- orig_filtered |>
        filter(mode %in% modes, mode != "car_passenger") |>
        group_by(mode) |>
        summarise(
          total_distance_km_orig = sum(routed_distance, na.rm = TRUE) / 1000,
          .groups = "drop"
        )

      # Calculate scenario VKM
      scenario_vkm <- matsim_filtered |>
        filter(mode %in% modes, mode != "car_passenger") |>
        group_by(mode) |>
        summarise(
          total_distance_km = sum(routed_distance, na.rm = TRUE) / 1000,
          .groups = "drop"
        )

      # Combine
      combined <- base_vkm |>
        full_join(scenario_vkm, by = "mode") |>
        mutate(
          scenario = scen,
          fleet_size = fs,
          filter_type = filter_type_name,
          delta_km = round(total_distance_km - total_distance_km_orig),
          pct_change = round(100 * delta_km / total_distance_km_orig)
        )

      # Add DRT (not filtered - vehicle movements)
      drt_row <- drt_vkm |>
        filter(scenario == scen, fleet_size == fs) |>
        mutate(filter_type = filter_type_name)

      combined <- bind_rows(combined, drt_row)

      # Add total
      total_row <- combined |>
        summarise(
          mode = "TOTAL",
          total_distance_km_orig = sum(total_distance_km_orig, na.rm = TRUE),
          total_distance_km = sum(total_distance_km, na.rm = TRUE),
          scenario = scen,
          fleet_size = fs,
          filter_type = filter_type_name,
          delta_km = total_distance_km - total_distance_km_orig,
          pct_change = round(100 * delta_km / total_distance_km_orig)
        )

      results_list[[length(results_list) + 1]] <- bind_rows(combined, total_row)
    }
  }

  bind_rows(results_list)
}

# Calculate for all filter types
all_vkm_results <- map2_dfr(
  names(filter_definitions),
  filter_definitions,
  ~ calculate_vkm(demand_original, demand_matsim, .x, .y$filter_fn)
)

# Add formatted delta column
all_vkm_results <- all_vkm_results |>
  mutate(
    `Delta (Thousands of km)` = ifelse(
      is.na(pct_change),
      sprintf("%.0f (NA)", delta_km / 1000),
      sprintf("%.0f (%.0f%%)", delta_km / 1000, pct_change)
    )
  )

# Save combined results
write_csv(all_vkm_results, "plots/global_vkm/global_vkm_change.csv")

# ---------- Comparison Plots

# Plot 1: Total VKM change by filter type
total_vkm_comparison <- all_vkm_results |>
  filter(mode == "TOTAL")

ggplot(
  total_vkm_comparison,
  aes(x = factor(fleet_size), y = delta_km / 1000, fill = filter_type)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labels)) +
  labs(
    title = "Total VKM Change by Filter Type",
    subtitle = "Change in vehicle kilometers traveled (Car + Taxi + DRT)",
    x = "Fleet Size",
    y = "Change in VKM (Thousands)",
    fill = "Filter Type"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  "plots/global_vkm/comparison_total_vkm_by_filter.png",
  width = 10,
  height = 6
)

# Plot 2: Car VKM change by filter type
car_vkm_comparison <- all_vkm_results |>
  filter(mode == "car")

ggplot(
  car_vkm_comparison,
  aes(
    x = factor(fleet_size),
    y = pct_change,
    color = filter_type,
    group = filter_type
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labels)) +
  labs(
    title = "Car VKM Change by Filter Type",
    subtitle = "Percentage change in car vehicle kilometers",
    x = "Fleet Size",
    y = "% Change in Car VKM",
    color = "Filter Type"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  "plots/global_vkm/comparison_car_vkm_by_filter.png",
  width = 10,
  height = 6
)

# Plot 3: Stacked comparison of VKM by mode and filter
vkm_by_mode <- all_vkm_results |>
  filter(mode != "TOTAL") |>
  mutate(mode = factor(mode, levels = c("drt", "taxi", "car")))

ggplot(
  vkm_by_mode |> filter(fleet_size == 500),
  aes(x = filter_type, y = total_distance_km / 1000, fill = mode)
) +
  geom_col(position = "stack") +
  facet_wrap(~scenario, labeller = labeller(scenario = scenario_labels)) +
  labs(
    title = "VKM Composition by Filter Type (Fleet Size = 500)",
    x = "Filter Type",
    y = "Total VKM (Thousands)",
    fill = "Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  "plots/global_vkm/comparison_vkm_composition_by_filter.png",
  width = 10,
  height = 6
)
