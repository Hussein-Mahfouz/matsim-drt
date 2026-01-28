library(tidyverse)
library(gt)

# ==============================================================================
# 1. LOAD DATA
# ==============================================================================

# from operator_stats.R
drt_table <- read_csv(
  "plots/operator_stats/drt_daily_stats.csv",
  show_col_types = FALSE
)
# from feeder_stats.R
drt_feeder_table <- read_csv(
  "plots/feeder_stats/drt_feeder_daily_stats.csv",
  show_col_types = FALSE
)
# mode shift tables (NOW WITH filter_type)
mode_shift_all_raw <- read_csv(
  "plots/mode_share/mode_shift_all.csv",
  show_col_types = FALSE
)
mode_shift_drt_raw <- read_csv(
  "plots/mode_share/mode_shift_drt.csv",
  show_col_types = FALSE
)
# vkm table (NOW WITH filter_type)
vkm_change_raw <- read_csv(
  "plots/global_vkm/global_vkm_change.csv",
  show_col_types = FALSE
)

# ==============================================================================
# 2. FILTERING BY SPATIAL SCOPE
# ==============================================================================
# CRITICAL STEP: Select which spatial scope you want to report in the tables.
# Options: "global", "trip_touch", "resident"

SELECTED_FILTER <- "global"
# SELECTED_FILTER <- "resident" # Use this if you want tables to reflect only zone residents

print(paste("Generating tables for filter type:", SELECTED_FILTER))

mode_shift_all <- mode_shift_all_raw %>% filter(filter_type == SELECTED_FILTER)
mode_shift_drt <- mode_shift_drt_raw %>% filter(filter_type == SELECTED_FILTER)
vkm_change <- vkm_change_raw %>% filter(filter_type == SELECTED_FILTER)

# ==============================================================================
# 3. PRE-PROCESSING
# ==============================================================================

# round columns
mode_shift_drt <- mode_shift_drt %>%
  mutate(trips_moved_drt_frac = round(trips_moved_drt_frac, 1))

# ==============================================================================
# 4. TABLES
# ==============================================================================

# ----------------- Table for overall DRT stats -----------------

drt_table |>
  group_by(fleet_size) |>
  gt() |>
  cols_hide(columns = c(scenario)) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(
    title = "Vehicle and passenger distance travelled"
  ) |>
  tab_spanner(
    label = "Vehicle",
    columns = c(
      vkm,
      vehicle_distance_per_trip,
      vkm_per_vehicle,
      average_load_factor
    )
  ) |>
  tab_spanner(
    label = "Passenger",
    columns = c(pkm, passenger_distance_per_trip, pkm_per_vehicle)
  ) |>
  cols_label(
    operator_id = "Scenario",
    pkm = "Km travelled",
    passenger_distance_per_trip = "Avg distance per trip (km)",
    pkm_per_vehicle = "Avg distance per vehicle (km)",
    vkm = "Km travelled",
    vehicle_distance_per_trip = "Avg distance per trip (km)",
    vkm_per_vehicle = "Avg distance per vehicle (km)",
    average_load_factor = "pkm / vkm"
  ) -> distance_drt_table_latex

print(distance_drt_table_latex)
distance_drt_table_latex %>% as_latex()

# ----------------- Feeder table (spanners: Main | Feeder) -----------------

drt_feeder_table |>
  group_by(fleet_size_label) |>
  gt(rowname_col = "operator_id") |>
  cols_hide(columns = c(scenario, fleet_size, distance_frac_main)) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(
    title = "DRT feeder statistics",
    subtitle = "Distinguishing between standalone and feeder DRT trips"
  ) |>
  tab_spanner(
    label = "Standalone",
    columns = c(no_of_trips_main, distance_main, waiting_time_main)
  ) |>
  tab_spanner(
    label = "Feeder",
    columns = c(
      no_of_trips_feeder,
      no_of_trips_feeder_frac,
      distance_feeder,
      distance_frac_feeder,
      waiting_time_feeder,
      average_feeder_trips_per_route,
      average_feeder_trips_per_route_top_5_routes
    )
  ) |>
  cols_label(
    distance_main = "Distance (km)",
    distance_feeder = "Distance (km)",
    no_of_trips_main = "Number of trips",
    no_of_trips_feeder = "Number of trips",
    distance_frac_feeder = "Feeder % (distance)",
    no_of_trips_feeder_frac = "Feeder % (trips)",
    waiting_time_main = "Average waiting time (min)",
    waiting_time_feeder = "Average waiting time (min)",
    average_feeder_trips_per_route = "Feeder trips per bus route",
    average_feeder_trips_per_route_top_5_routes = "Feeder trips per bus route (top 5 routes)"
  ) -> feeder_drt_table_ltx_v1

print(feeder_drt_table_ltx_v1)
feeder_drt_table_ltx_v1 %>% as_latex()

# ----------------- Feeder table (spanners: Trips | Distances | PT connections) -----------------

drt_feeder_table |>
  select(!contains("waiting_time")) |>
  mutate(
    distance_feeder = paste0(distance_feeder, " (", distance_frac_feeder, "%)"),
    no_of_trips_feeder = paste0(
      no_of_trips_feeder,
      " (",
      no_of_trips_feeder_frac,
      "%)"
    )
  ) |>
  group_by(fleet_size) |>
  gt() |>
  cols_hide(
    columns = c(
      scenario,
      fleet_size_label,
      distance_frac_main,
      distance_frac_feeder,
      no_of_trips_feeder_frac
    )
  ) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(
    title = "DRT feeder statistics",
    subtitle = "Distinguishing between standalone and feeder DRT trips"
  ) |>
  tab_spanner(
    label = "Distance travelled (km)",
    columns = c(distance_main, distance_feeder)
  ) |>
  tab_spanner(
    label = "No. of trips",
    columns = c(no_of_trips_main, no_of_trips_feeder)
  ) |>
  tab_spanner(
    label = "PT connections",
    columns = c(
      average_feeder_trips_per_route,
      average_feeder_trips_per_route_top_5_routes
    )
  ) |>
  cols_label(
    operator_id = "Service Area",
    distance_main = "Standalone",
    distance_feeder = "Feeder (%)",
    no_of_trips_main = "Standalone",
    no_of_trips_feeder = "Feeder (%)",
    average_feeder_trips_per_route = "Avg no. of feeder trips per bus route served",
    average_feeder_trips_per_route_top_5_routes = "Avg no. of feeder trips per bus route served (top 5 routes)"
  ) -> feeder_drt_table_ltx

print(feeder_drt_table_ltx)
feeder_drt_table_ltx %>% as_latex()

# ----------------- Mode shift -----------------

# HYBRID: Global Counts + Local %

# 1. Get ABSOLUTE COUNTS from Global filter
mode_shift_counts <- mode_shift_drt_raw %>%
  filter(filter_type == "global") %>%
  select(input_mode, output_mode, fleet_size, scenario, trips)

# 2. Get LOCAL PERCENTAGES from trip_touch filter
mode_shift_local_pct <- mode_shift_drt_raw %>%
  filter(filter_type == "trip_touch") %>%
  select(input_mode, output_mode, fleet_size, scenario, trips_moved_drt_frac)

# 3. Join them together
mode_shift_hybrid <- mode_shift_counts %>%
  left_join(
    mode_shift_local_pct,
    by = c("input_mode", "output_mode", "fleet_size", "scenario")
  ) %>%
  # Round the percentage
  mutate(trips_moved_drt_frac = round(trips_moved_drt_frac, 1))

# 4. Add Service Area and Feeder columns (same logic as before)
mode_shift_hybrid <- mode_shift_hybrid %>%
  mutate(
    `Service Area` = case_when(
      str_starts(output_mode, "drtNE") ~ "drtNE",
      str_starts(output_mode, "drtNW") ~ "drtNW",
      str_starts(output_mode, "drtInner") ~ "drtInner",
      str_starts(output_mode, "drt") ~ "drtAll",
      TRUE ~ NA_character_
    ),
    Feeder = case_when(
      str_ends(output_mode, "_feeder") ~ "feeder",
      TRUE ~ "standalone"
    )
  )

# 5. Reshape for the table
mode_shift_hybrid_table <- mode_shift_hybrid %>%
  select(
    input_mode,
    trips,
    Feeder,
    trips_moved_drt_frac,
    fleet_size,
    `Service Area`
  ) %>%
  pivot_wider(
    names_from = c(Feeder, input_mode),
    values_from = c(trips, trips_moved_drt_frac),
    values_fill = 0
  )

# 6. Add totals for standalone and feeder
mode_shift_hybrid_table <- mode_shift_hybrid_table %>%
  mutate(
    trips_standalone_all = rowSums(
      select(., starts_with("trips_standalone_")),
      na.rm = TRUE
    ),
    trips_feeder_all = rowSums(
      select(., starts_with("trips_feeder_")),
      na.rm = TRUE
    )
  )

# 7. Create formatting columns (N (Local %))
mode_shift_hybrid_table <- mode_shift_hybrid_table %>%
  mutate(
    # Car
    trips_car_standalone = paste0(
      trips_standalone_car,
      " (",
      trips_moved_drt_frac_standalone_car,
      ")"
    ),
    trips_car_feeder = paste0(
      trips_feeder_car,
      " (",
      trips_moved_drt_frac_feeder_car,
      ")"
    ),
    # PT
    trips_pt_standalone = paste0(
      trips_standalone_pt,
      " (",
      trips_moved_drt_frac_standalone_pt,
      ")"
    ),
    trips_pt_feeder = paste0(
      trips_feeder_pt,
      " (",
      trips_moved_drt_frac_feeder_pt,
      ")"
    ),
    # Bike
    trips_bike_standalone = paste0(
      trips_standalone_bike,
      " (",
      trips_moved_drt_frac_standalone_bike,
      ")"
    ),
    trips_bike_feeder = paste0(
      trips_feeder_bike,
      " (",
      trips_moved_drt_frac_feeder_bike,
      ")"
    ),
    # Walk
    trips_walk_standalone = paste0(
      trips_standalone_walk,
      " (",
      trips_moved_drt_frac_standalone_walk,
      ")"
    ),
    trips_walk_feeder = paste0(
      trips_feeder_walk,
      " (",
      trips_moved_drt_frac_feeder_walk,
      ")"
    ),
    # Taxi
    trips_taxi_standalone = paste0(
      trips_standalone_taxi,
      " (",
      trips_moved_drt_frac_standalone_taxi,
      ")"
    ),
    trips_taxi_feeder = paste0(
      trips_feeder_taxi,
      " (",
      trips_moved_drt_frac_feeder_taxi,
      ")"
    )
  ) %>%
  select(
    fleet_size,
    `Service Area`,
    trips_car_standalone,
    trips_car_feeder,
    trips_pt_standalone,
    trips_pt_feeder,
    trips_bike_standalone,
    trips_bike_feeder,
    trips_walk_standalone,
    trips_walk_feeder,
    trips_taxi_standalone,
    trips_taxi_feeder
  )

# 8. Generate GT Table
mode_shift_hybrid_table_ltx <- mode_shift_hybrid_table %>%
  group_by(fleet_size) %>%
  gt() %>%
  tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = "Mode Shift to DRT",
    subtitle = "Number of Trips (% of Local Demand Shifted)"
  ) %>%
  tab_spanner(
    label = "Car",
    columns = c(trips_car_standalone, trips_car_feeder)
  ) %>%
  tab_spanner(
    label = "Public Transport",
    columns = c(trips_pt_standalone, trips_pt_feeder)
  ) %>%
  tab_spanner(
    label = "Bike",
    columns = c(trips_bike_standalone, trips_bike_feeder)
  ) %>%
  tab_spanner(
    label = "Walk",
    columns = c(trips_walk_standalone, trips_walk_feeder)
  ) %>%
  tab_spanner(
    label = "Taxi",
    columns = c(trips_taxi_standalone, trips_taxi_feeder)
  ) %>%
  cols_label(
    trips_car_standalone = "Standalone",
    trips_car_feeder = "Feeder",
    trips_pt_standalone = "Standalone",
    trips_pt_feeder = "Feeder",
    trips_bike_standalone = "Standalone",
    trips_bike_feeder = "Feeder",
    trips_walk_standalone = "Standalone",
    trips_walk_feeder = "Feeder",
    trips_taxi_standalone = "Standalone",
    trips_taxi_feeder = "Feeder"
  ) %>%
  tab_source_note(
    source_note = "How to read: 11,707 (2.8) = 11,707 trips shifted to DRT (2.8% of local demand for that mode shifted)"
  )

print(mode_shift_hybrid_table_ltx)
mode_shift_hybrid_table_ltx %>% as_latex()

# ----------------- VKM change -----------------

# 1. Get Absolute Delta & DRT VKM from GLOBAL
vkm_abs <- vkm_change_raw %>%
  filter(filter_type == "global") %>%
  select(scenario, fleet_size, mode, delta_km, total_distance_km)

# 2. Extract DRT VKM specifically (it's the 'total_distance_km' when mode is 'drt')
drt_vkm_lookup <- vkm_abs %>%
  filter(mode == "drt") %>%
  select(scenario, fleet_size, drt_vkm = total_distance_km)

# 3. Get Percent Change from LOCAL (trip_touch)
vkm_pct <- vkm_change_raw %>%
  filter(filter_type == "trip_touch") %>%
  select(scenario, fleet_size, mode, pct_change)

# 4. Join and Format
vkm_hybrid <- vkm_abs %>%
  filter(mode != "drt") %>% # We only want Car/Taxi/Total rows for the table body
  left_join(vkm_pct, by = c("scenario", "fleet_size", "mode")) %>%
  left_join(drt_vkm_lookup, by = c("scenario", "fleet_size")) %>% # Attach DRT VKM
  mutate(
    # Create the label: "Abs (Pct%)" -> e.g. "-121 (-8%)"
    # Convert delta_km to Thousands for readability
    label = sprintf("%.0f (%.0f%%)", delta_km / 1000, pct_change),
    # Handle NAs (e.g. infinite percent change or no local change)
    label = ifelse(is.na(pct_change), sprintf("%.0f", delta_km / 1000), label)
  )

# 5. Pivot for GT Table
vkm_hybrid_wide <- vkm_hybrid %>%
  select(scenario, fleet_size, mode, label, drt_vkm) %>%
  pivot_wider(
    names_from = mode,
    values_from = label
  ) %>%
  mutate(drt_vkm = round(drt_vkm / 1000)) # Clean up DRT column to thousands

# 6. Generate GT Table
vkm_hybrid_ltx <- vkm_hybrid_wide %>%
  group_by(fleet_size) %>%
  gt() %>%
  #cols_hide(columns = c(scenario)) %>%
  tab_options(row_group.as_column = TRUE) %>%
  tab_header(
    title = "VKM Change per mode",
    subtitle = "Global Absolute Change (Thousands of km) | Local % Change"
  ) %>%
  tab_spanner(
    label = "Change in VKM",
    columns = c(car, taxi, TOTAL)
  ) %>%
  cols_label(
    drt_vkm = "DRT VKM (k)",
    car = "Car",
    taxi = "Taxi",
    TOTAL = "Total"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_source_note(
    source_note = "Note: Absolute values represent system-wide impact (thousands of km). Percentages represent impact within the service zone (local efficiency)."
  )

print(vkm_hybrid_ltx)
vkm_hybrid_ltx %>% as_latex()
