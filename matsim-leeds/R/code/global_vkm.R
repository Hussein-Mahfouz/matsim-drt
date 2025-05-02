library(tidyverse)

# This script looks at change in overall vkm. We compare all trip modes before and after
# the simulation for sum of: CAR, PT, TAXI, DRT


# We get VKM for the following modes: CAR, TAXI, DRT (PT does not change)

# Sources
# CAR + TAXI: eqasim_legs (tracks routed vehicle distance (this works as vehicles are not shared))
# DRT: eqasim_drt_vehicle_movements (the vehicles are shared so we can't use eqasim_legs as it may double count)



# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones",
               "all",
               "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)



# Define scenario names for plot subtitles
scenario_labels <- c(
  "zones" = "Zone-based DRT",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
)


# ---------- STEP 1: READ IN ALL THE DATA

# ----- STEP 1a: Original VKM (before introduction of DRT)


# vehicular modes
modes = c("car","taxi")

demand_original = read_delim("../scenarios/basic/sample_1.00/eqasim_trips.csv", delim =";")



# ----- STEP 1b: VKM for different scenarios (after introduction of DRT)


# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Read the data
  file_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv")
  # Check if file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)  # Safe fail
  }
  # Print status
  print(paste("Reading file:", file_path))
  # Read
  data <- read_delim(file_path, delim = ";")

  # Add the scenario and fleet size columns
  data <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  return(data)
}


# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)


# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
demand_matsim <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_trips")
})

# ----- STEP 2: Join input and output trips and calculate % change in VKM

# Get total distance per mode - ORIGINAL SCENARIO
demand_original_dist = demand_original %>%
  rename(pid = person_id) %>%
  select(pid, person_trip_id, mode, routed_distance) %>%
  group_by(mode) %>%
  summarise(total_distance_km_orig = sum(routed_distance) / 1000) %>%
  ungroup()

# Get total distance per mode - ALL SCENARIOS
demand_matsim_dist = demand_matsim %>%
  rename(pid = person_id) %>%
  select(pid, person_trip_id, mode, scenario, fleet_size, routed_distance) %>%
  group_by(mode, scenario, fleet_size) %>%
  summarise(total_distance_km = sum(routed_distance) / 1000) %>%
  ungroup()

# join
demand_compare = demand_original_dist %>%
  full_join(demand_matsim_dist, by = c("mode"))


# Keep only the modes that we want (CAR, TAXI)
demand_compare = demand_compare %>%
  filter(str_detect(mode, str_c(modes, collapse = "|"))) %>%
  filter(mode != "car_passenger")

# Calculate % change
demand_compare <- demand_compare %>%
  mutate(
    delta_km = round(total_distance_km - total_distance_km_orig),
    # No DRT in original scenario
    pct_change = round(100 * (delta_km / total_distance_km_orig)))


# ----- STEP 3: Get DRT VKM

# load in data
demand_matsim_drt <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_drt_vehicle_movements")
})

# get vkm per scenario (drtNE and drtNW are summed together, as they are the same scenario)
demand_matsim_drt_vkm = demand_matsim_drt %>%
  group_by(fleet_size, scenario) %>%
  summarise(total_distance_km = round(sum(distance) / 1000)) %>%
  ungroup() %>%
  mutate(mode = "drt",
         total_distance_km_orig = 0,
         delta_km = total_distance_km,
         pct_change = NA_real_)

# ----- STEP 4: COMBINE DRT VKM WITH CAR and TAXI VKM

demand_all_vkm = demand_compare %>%
  bind_rows(demand_matsim_drt_vkm)


# ----- Add a TOTAL row
demand_all_vkm_with_totals <- demand_all_vkm %>%
  bind_rows(
    demand_all_vkm %>%
      group_by(scenario, fleet_size) %>%
      summarise(
        mode = "TOTAL",
        total_distance_km_orig = sum(total_distance_km_orig, na.rm = TRUE),
        total_distance_km = sum(total_distance_km, na.rm = TRUE),
        delta_km = total_distance_km - total_distance_km_orig,
        pct_change = 100 * delta_km / total_distance_km_orig,
        .groups = "drop"
      )
  )

# Add a column with change in km and % change
demand_all_vkm_with_totals <- demand_all_vkm_with_totals %>%
  mutate(
    `Delta (Thousands of km)` = ifelse(
      is.na(pct_change),
      sprintf("%.0f (NA)", delta_km/1000),
      sprintf("%.0f (%.0f%%)", delta_km/1000, pct_change)
    )
  )


write_csv(demand_all_vkm_with_totals, "plots/global_vkm/global_vkm_change.csv")


