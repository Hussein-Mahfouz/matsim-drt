library(tidyverse)
library(gt)

# from operator_stats.R
drt_table = read_csv("plots/operator_stats/drt_daily_stats.csv")
# from feeder_stats.R
drt_feeder_table = read_csv("plots/feeder_stats/drt_feeder_daily_stats.csv")
# mode shift tables
mode_shift_all = read_csv("plots/mode_share/mode_shift_all.csv")
mode_shift_drt = read_csv("plots/mode_share/mode_shift_drt.csv")


# ------ Summary statistics tables

# --- Table for overall DRT stats

drt_table |>
  group_by(fleet_size) |>
  gt() |>
  cols_hide(columns = c(scenario)) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "Vehicle and passenger distance travelled",
            # subtitle = "Average fleet and passenger distance under different scenarios"
             ) |>
  tab_spanner(
    label = "Vehicle",
    columns = c(vkm, vehicle_distance_per_trip, average_load_factor)
  ) |>
  tab_spanner(
    label = "Passenger",
    columns = c(pkm, passenger_distance_per_trip)
  ) |>
  cols_label(
    operator_id = "Scenario",
    pkm = "Km travelled",
    passenger_distance_per_trip = "Average distance per trip (km)",
    vkm = "Km travelled",
    vehicle_distance_per_trip = "Average distance per trip (km)",
    average_load_factor = "Average load factor",

  )


# --- Feeder table (spanners: Main | Feeder)

drt_feeder_table |>
  group_by(fleet_size_label) |>
  gt(rowname_col = "operator_id") |>
  cols_hide(columns = c(scenario, fleet_size, distance_frac_main)) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "DRT feeder statistics",
             subtitle = "Distinguishing between standalone and feeder DRT trips") |>
  tab_spanner(
    label = "Standalone",
    columns = c(no_of_trips_main,distance_main, waiting_time_main)
  ) |>
  tab_spanner(
    label = "Feeder",
    columns = c(no_of_trips_feeder, no_of_trips_feeder_frac, distance_feeder, distance_frac_feeder, waiting_time_feeder,
                average_feeder_trips_per_route, average_feeder_trips_per_route_top_5_routes)
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
    average_feeder_trips_per_route_top_5_routes = "Feeder trips per bus route (top 5 routes)")



# --- Feeder table (spanners: Trips | Distances | Waiting time)

drt_feeder_table |>
  group_by(fleet_size_label) |>
  gt() |>
  cols_hide(columns = c(scenario, fleet_size, distance_frac_main)) |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "DRT feeder statistics",
             subtitle = "Distinguishing between standalone and feeder DRT trips") |>
  tab_spanner(
    label = "Distance travelled (km)",
    columns = c(distance_main, distance_feeder, distance_frac_feeder)
  ) |>
  tab_spanner(
    label = "No. of trips",
    columns = c(no_of_trips_main, no_of_trips_feeder, no_of_trips_feeder_frac)
  ) |>
  tab_spanner(
    label = "Waiting times (s)",
    columns = c(waiting_time_main, waiting_time_feeder)
  ) |>
  tab_spanner(
    label = "PT connections",
    columns = c(average_feeder_trips_per_route, average_feeder_trips_per_route_top_5_routes)
  ) |>
  cols_label(
    operator_id = "Service Area",
    distance_main = "Standalone",
    distance_feeder = "Feeder",
    no_of_trips_main = "Standalone",
    no_of_trips_feeder = "Feeder",
    distance_frac_feeder = "Feeder %",
    no_of_trips_feeder_frac = "Feeder %",
    waiting_time_main = "Standalone",
    waiting_time_feeder = "Feeder",
    average_feeder_trips_per_route = "Feeder trips per bus route",
    average_feeder_trips_per_route_top_5_routes = "Feeder trips per bus route (top 5 routes)") %>%
  as_latex()






# ---------- Mode shift

# Add feeder and Service area (scenario) colums
mode_shift_drt = mode_shift_drt %>%
  mutate(
    `Service Area` = case_when(
      str_starts(output_mode, "drtNE") ~ "drtNE",
      str_starts(output_mode, "drtNW") ~ "drtNW",
      str_starts(output_mode, "drt") ~ "drt",
      TRUE ~ NA_character_
  ),
    Feeder = case_when(
      str_ends(output_mode, "_feeder") ~ "feeder",
      TRUE ~ "standalone")
  )

# One column per mode (pt, bike, car etc) tp signify <mode>(frac to drt) | <mode>(fract_to_drt_feeder)

mode_shift_drt_table = mode_shift_drt %>%
  select(c(input_mode, trips, Feeder, trips_moved_drt_frac, fleet_size, `Service Area`)) %>%
  pivot_wider(names_from = c(Feeder, input_mode),
              values_from = c(trips, trips_moved_drt_frac))


# Create 2 columns for each mode: <mode>(frac to drt) | <mode>(fract_to_drt_feeder)
mode_shift_drt_table = mode_shift_drt_table %>%
  mutate(
    # car
    trips_car_standalone = paste0(trips_standalone_car, " (", trips_moved_drt_frac_standalone_car, ")"),
    trips_car_feeder = paste0(trips_feeder_car, " (", trips_moved_drt_frac_feeder_car, ")"),
    # pt
    trips_pt_standalone = paste0(trips_standalone_pt, " (", trips_moved_drt_frac_standalone_pt, ")"),
    trips_pt_feeder = paste0(trips_feeder_pt, " (", trips_moved_drt_frac_feeder_pt, ")"),
    # bike,
    trips_bike_standalone = paste0(trips_standalone_bike, " (", trips_moved_drt_frac_standalone_bike, ")"),
    trips_bike_feeder = paste0(trips_feeder_bike, " (", trips_moved_drt_frac_feeder_bike, ")"),
    # walk
    trips_walk_standalone = paste0(trips_standalone_walk, " (", trips_moved_drt_frac_standalone_walk, ")"),
    trips_walk_feeder = paste0(trips_feeder_walk, " (", trips_moved_drt_frac_feeder_walk, ")"),
    # taxi
    trips_taxi_standalone = paste0(trips_standalone_taxi, " (", trips_moved_drt_frac_standalone_taxi, ")"),
    trips_taxi_feeder = paste0(trips_feeder_taxi, " (", trips_moved_drt_frac_feeder_taxi, ")")) %>%
  select(fleet_size, `Service Area`,
         ends_with("_standalone"), ends_with("_feeder"))


# --- Plot mode shift: Spanner groups = Modes
mode_shift_drt_table |>
  group_by(fleet_size) |>
  gt() |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "Which modes are DRT trips coming from?",
             subtitle = "For each mode, we show: Number of Trips shifted to DRT (% of mode total that shifted to DRT)") |>
  tab_spanner(
    label = "Car",
    columns = c(trips_car_standalone, trips_car_feeder)
  ) |>
  tab_spanner(
    label = "Public Transport",
    columns = c(trips_pt_standalone, trips_pt_feeder)
  ) |>
  tab_spanner(
    label = "Bike",
    columns = c(trips_bike_standalone, trips_bike_feeder)
  ) |>
  tab_spanner(
    label = "Walk",
    columns = c(trips_walk_standalone, trips_walk_feeder)
  ) |>
  tab_spanner(
    label = "Taxi",
    columns = c(trips_taxi_standalone, trips_taxi_feeder)
  ) |>
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
    trips_taxi_feeder = "Feeder") |>
  tab_source_note(source_note = "How to read: 150 (0.03) = 150 trips shifted to DRT (0.03% of Mode total shifted to DRT")


# --- Plot mode shift: Spanner groups = Standalone | Feeder
mode_shift_drt_table |>
  group_by(fleet_size) |>
  gt() |>
  tab_options(row_group.as_column = TRUE) |>
  tab_header(title = "Which modes are DRT trips coming from?",
             subtitle = "For each mode, we show: Number of Trips shifted to DRT (% of mode total that shifted to DRT)") |>
  tab_spanner(
    label = "Standalone",
    columns = c(trips_car_standalone, trips_pt_standalone, trips_bike_standalone, trips_walk_standalone, trips_taxi_standalone)
  ) |>
  tab_spanner(
    label = "Feeder",
    columns = c(trips_car_feeder, trips_pt_feeder, trips_bike_feeder, trips_walk_feeder, trips_taxi_feeder)
  ) |>
  cols_label(
    trips_car_standalone = "Car",
    trips_car_feeder = "Car",
    trips_pt_standalone = "PT",
    trips_pt_feeder = "PT",
    trips_bike_standalone = "Bike",
    trips_bike_feeder = "Bike",
    trips_walk_standalone = "Walk",
    trips_walk_feeder = "Walk",
    trips_taxi_standalone = "Taxi",
    trips_taxi_feeder = "Taxi") |>
  tab_source_note(source_note = "How to read: 150 (0.03) = 150 trips shifted to DRT (0.03% of Mode total shifted to DRT)")
