library(tidyverse)
library(arrow)
library(ggalluvial)
library(ggh4x)
library(data.table) # For fast fread

# This script looks at transitions in mode share. We compare all trip modes
# before and after the simulation.
# Outputs:
# - One combined CSV with all filter types identified by a 'filter_type' column
# - Comparison plots across filter types

# ---------- Setup

scenarios <- c("zones", "all", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)

scenario_labels <- c(
  "zones" = "Zone-based DRT (NE/NW)",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
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

# Filter type definitions (using if/else for robustness)
filter_definitions <- list(
  global = list(
    name = "Global",
    description = "All trips in study area",
    filter_fn = function(df, scenario) rep(TRUE, nrow(df))
  ),
  trip_touch = list(
    name = "Trip touches zone",
    description = "Origin OR destination in service zone",
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
    description = "Person lives in service zone",
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

# ==============================================================================
# 1. DATA READING (Fast with fread, no mode hierarchy override)
# ==============================================================================

read_and_process <- function(scenario, fleet_size) {
  file_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/eqasim_trips.csv"
  )

  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }

  message(paste0(
    "[",
    format(Sys.time(), "%H:%M:%S"),
    "] Reading: ",
    scenario,
    " (",
    fleet_size,
    ")"
  ))

  # Use fread for speed, keep original mode column as-is
  data <- fread(file_path, sep = ";")

  data <- data |>
    as_tibble() |>
    mutate(
      scenario = scenario,
      fleet_size = fleet_size
    )

  return(data)
}

# Read all scenario data
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)
demand_matsim <- purrr::pmap_dfr(combinations, read_and_process)

# Read baseline
demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
)

# # ==============================================================================
# # 2. FILTER TO COMMON TRIPS (STUCK AGENT REMOVAL)
# # ==============================================================================

# # Create unique trip ID
# demand_original <- demand_original |>
#   mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

# demand_matsim <- demand_matsim |>
#   mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

# # Find trips that exist in ALL scenario-fleet combinations
# n_combos <- demand_matsim |> distinct(scenario, fleet_size) |> nrow()

# trips_in_all_scenarios <- demand_matsim |>
#   distinct(trip_id, scenario, fleet_size) |>
#   count(trip_id, name = "n_combos_present") |>
#   filter(n_combos_present == n_combos) |>
#   pull(trip_id)

# # Also must exist in baseline
# trips_in_baseline <- demand_original |> pull(trip_id) |> unique()

# # Intersection: trips that exist in baseline AND all scenarios
# common_trips <- intersect(trips_in_all_scenarios, trips_in_baseline)

# message(sprintf(
#   "Filtering to %s common trips (of %s baseline, %s across all scenarios)",
#   scales::comma(length(common_trips)),
#   scales::comma(length(trips_in_baseline)),
#   scales::comma(length(trips_in_all_scenarios))
# ))

# # Apply filter
# demand_original <- demand_original |> filter(trip_id %in% common_trips)
# demand_matsim <- demand_matsim |> filter(trip_id %in% common_trips)

# ==============================================================================
# 2. PERSON CONSISTENCY CHECK & IMPUTATION
# ==============================================================================
# Logic: If a person has fewer trips in the simulation than in the baseline
# (i.e. they got stuck), we assume their ENTIRE day reverts to the baseline behavior.
# This ensures behavioral consistency and maintains constant denominators.

message(
  "Checking person consistency (Imputing baseline behavior for stuck agents)..."
)

# 1. Baseline Trip Counts per Person
baseline_counts <- demand_original |>
  count(person_id, name = "n_expected")

# 2. Simulation Trip Counts per Person (Grouped by Scenario)
sim_counts <- demand_matsim |>
  count(scenario, fleet_size, person_id, name = "n_actual")

# 3. Identify Valid People (Completed full plans in simulation)
valid_people_lookup <- sim_counts |>
  inner_join(baseline_counts, by = "person_id") |>
  filter(n_actual == n_expected) |>
  mutate(is_valid = TRUE) |>
  select(scenario, fleet_size, person_id, is_valid)

# 4. Filter Simulation Data (Keep only trips from valid people)
demand_matsim_valid <- demand_matsim |>
  inner_join(
    valid_people_lookup,
    by = c("scenario", "fleet_size", "person_id")
  )

# 5. Impute Reverted People
# For agents who failed/stuck, we insert their BASELINE trips.
# This effectively counts them as "Staying with Original Mode" (No Shift).

scen_fleet_combos <- distinct(demand_matsim, scenario, fleet_size)

# Helper function to generate fallback trips for a scenario
get_reverted_trips <- function(scen, fs) {
  # Who suceeded in this scenario?
  valid_ids <- valid_people_lookup |>
    filter(scenario == scen, fleet_size == fs) |>
    pull(person_id)

  # Create rows for everyone else (the failures), using their Baseline data
  reverted_trips <- demand_original |>
    filter(!person_id %in% valid_ids) |>
    mutate(
      scenario = scen,
      fleet_size = fs,
      # Important: The 'mode' here is the Baseline Input Mode.
      # By adding it to the 'Output' dataset, it becomes the Output Mode.
      # Result: Input=Car -> Output=Car (No Shift).
    )

  return(reverted_trips)
}

# Generate DataFrame of all reverted trips
demand_matsim_reverted <- purrr::pmap_dfr(
  scen_fleet_combos,
  function(scenario, fleet_size) {
    get_reverted_trips(scenario, fleet_size)
  }
)

# 6. Combine Valid Sim + Reverted Baseline
demand_matsim <- bind_rows(demand_matsim_valid, demand_matsim_reverted) %>%
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_")) # Ensure ID exists

message(sprintf(
  "Imputation Complete. Total Analyzed Trips: %s (across all scenarios).",
  scales::comma(nrow(demand_matsim))
))

# 7. Prepare Baseline (Just add trip_id for joining)
demand_original <- demand_original |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

# (We do NOT filter demand_original. We use the full universe.)

# ==============================================================================
# 3. DATA PREPARATION & JOINING
# ==============================================================================

demand_original_prep <- demand_original |>
  rename(pid = person_id) |>
  select(pid, person_trip_id, mode) |>
  rename_with(~ paste0("input_", .), .cols = c("mode")) |>
  group_by(input_mode) |>
  mutate(input_mode_trips = n()) |>
  ungroup() |>
  mutate(
    input_mode_trips_frac = round((input_mode_trips / n()) * 100, 1),
    input_mode_trips_with_frac = glue::glue(
      "{input_mode} ({input_mode_trips_frac}%)"
    )
  )

demand_matsim_prep <- demand_matsim |>
  rename(pid = person_id) |>
  select(pid, person_trip_id, mode, scenario, fleet_size) |>
  rename_with(~ paste0("output_", .), .cols = c("mode"))

# Join all data
demand_compare <- demand_original_prep |>
  left_join(demand_matsim_prep, by = c("pid", "person_trip_id")) |>
  left_join(
    spatial_lookup |> rename(pid = person_id),
    by = c("pid", "person_trip_id")
  )

# ==============================================================================
# 4. CALCULATE MODE SHIFT
# ==============================================================================

calculate_mode_shift <- function(data, filter_type_name, filter_fn) {
  results_list <- list()

  for (scen in scenarios) {
    for (fs in fleet_sizes) {
      df_subset <- data |> filter(scenario == scen, fleet_size == fs)
      if (nrow(df_subset) == 0) {
        next
      }

      keep_rows <- filter_fn(df_subset, scen)
      df_filtered <- df_subset[keep_rows, ]
      if (nrow(df_filtered) == 0) {
        next
      }

      df_summary <- df_filtered |>
        group_by(input_mode) |>
        mutate(local_input_trips = n()) |>
        ungroup() |>
        mutate(local_input_frac = round((local_input_trips / n()) * 100, 1)) |>
        group_by(
          input_mode,
          local_input_trips,
          local_input_frac,
          input_mode_trips_with_frac,
          output_mode
        ) |>
        summarise(trips = n(), .groups = "drop") |>
        mutate(
          scenario = scen,
          fleet_size = fs,
          filter_type = filter_type_name
        )

      results_list[[length(results_list) + 1]] <- df_summary
    }
  }

  bind_rows(results_list)
}

# Run All Calculations
all_mode_shifts <- map2_dfr(
  names(filter_definitions),
  filter_definitions,
  ~ calculate_mode_shift(demand_compare, .x, .y$filter_fn)
)

# Add DRT-specific summary
all_mode_shifts_drt <- all_mode_shifts |>
  filter(str_detect(output_mode, "drt")) |>
  mutate(trips_moved_drt_frac = round((trips / local_input_trips) * 100, 2)) |>
  group_by(input_mode, fleet_size, scenario, filter_type) |>
  mutate(
    total_drt_frac = sum(trips_moved_drt_frac),
    mode_with_trips_moved_drt_frac = glue::glue(
      "{input_mode} ({total_drt_frac}%)"
    )
  ) |>
  ungroup()

# Save combined results
write_csv(all_mode_shifts, "plots/mode_share/mode_shift_all.csv")
write_csv(all_mode_shifts_drt, "plots/mode_share/mode_shift_drt.csv")

# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

# Split DRT Output into Standalone vs Feeder
drt_split_data <- all_mode_shifts |>
  filter(str_detect(output_mode, "drt")) |>
  mutate(
    drt_service = if_else(
      str_detect(output_mode, "_feeder$"),
      "Feeder",
      "Standalone"
    )
  )

# --- Plot 1: Overall DRT Mode Share (Split by Service Type) ---
drt_share_summary <- drt_split_data |>
  group_by(scenario, fleet_size, filter_type, drt_service) |>
  summarise(
    drt_trips = sum(trips),
    total_system_trips = sum(local_input_trips) / n_distinct(input_mode),
    drt_share_pct = drt_trips / total_system_trips * 100,
    .groups = "drop"
  )

p1 <- ggplot(
  drt_share_summary,
  aes(x = factor(fleet_size), y = drt_share_pct, fill = filter_type)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  ggh4x::facet_nested(
    drt_service ~ scenario,
    scales = "free_y",
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Total DRT Mode Share",
    subtitle = "Comparing Standalone (door-to-door) vs Feeder (access/egress) uptake",
    x = "Fleet Size",
    y = "Mode Share (%)",
    fill = "Filter Type"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p1

ggsave(
  "plots/mode_share/plot_1_drt_share_split.png",
  p1,
  width = 12,
  height = 8
)

# --- Plot 2: Shift Percentage by Mode ---
mode_shift_pct <- drt_split_data |>
  filter(input_mode %in% c("car", "pt", "walk", "bike", "taxi")) |>
  group_by(scenario, fleet_size, filter_type, input_mode, drt_service) |>
  summarise(
    trips_shifted = sum(trips),
    total_input_trips = first(local_input_trips),
    pct_shifted = trips_shifted / total_input_trips * 100,
    .groups = "drop"
  )

p2 <- ggplot(
  mode_shift_pct,
  aes(
    x = factor(fleet_size),
    y = pct_shifted,
    color = filter_type,
    group = interaction(filter_type, drt_service),
    linetype = drt_service
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_grid(
    input_mode ~ scenario,
    scales = "free_y",
    labeller = labeller(scenario = scenario_labels)
  ) +
  labs(
    title = "Shift Intensity by Mode",
    subtitle = "Percentage of original mode shifting to DRT types",
    x = "Fleet Size",
    y = "% of Trips Shifting",
    color = "Filter Type",
    linetype = "Service Type"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p2

ggsave(
  "plots/mode_share/plot_2_shift_pct_by_mode.png",
  p2,
  width = 12,
  height = 12
)

# --- Plot 3: Intensity (%), Labeled by Volume (Trips) ---
p3 <- ggplot(
  mode_shift_pct,
  aes(x = filter_type, y = pct_shifted, fill = input_mode)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = scales::comma(trips_shifted), group = input_mode),
    position = position_dodge(width = 0.9),
    angle = 90,
    hjust = -0.2,
    vjust = 0.5,
    size = 2.5
  ) +
  ggh4x::facet_nested(
    fleet_size ~ scenario + drt_service,
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(
    title = "Mode Shift Impact of DRT",
    subtitle = "Bars = % Shifted. Labels = Number of Trips.",
    x = "Filter Type",
    y = "% of Original Mode Shifted",
    fill = "Original Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p3

ggsave(
  "plots/mode_share/plot_3_impact_bar_clean.png",
  p3,
  width = 16,
  height = 10
)

# --- Plot 3b: Volume (Trips), Labeled by Intensity (%) ---
p3b <- ggplot(
  mode_shift_pct,
  aes(x = filter_type, y = trips_shifted, fill = input_mode)
) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(
    aes(label = paste0(round(pct_shifted, 1), "%"), group = input_mode),
    position = position_dodge(width = 0.9),
    angle = 90,
    hjust = -0.2,
    vjust = 0.5,
    size = 2.2
  ) +
  ggh4x::facet_nested(
    fleet_size ~ scenario + drt_service,
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.4))
  ) +
  labs(
    title = "Mode Shift Impact of DRT",
    subtitle = "Bars = Number of Trips. Labels = % Shifted.",
    x = "Filter Type",
    y = "Number of Trips Shifted",
    fill = "Original Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p3b

ggsave(
  "plots/mode_share/plot_3b_volume_bar_with_pct_labels.png",
  p3b,
  width = 16,
  height = 10
)

# --- Plot 4: Volume Stacked ---
p4 <- ggplot(
  mode_shift_pct,
  aes(x = filter_type, y = trips_shifted, fill = input_mode)
) +
  geom_col(position = "stack") +
  ggh4x::facet_nested(
    fleet_size ~ scenario + drt_service,
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total DRT Volume by Source",
    subtitle = "Number of trips contributing to DRT ridership",
    x = "Filter Type",
    y = "Total Trips",
    fill = "Source Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

p4

ggsave(
  "plots/mode_share/plot_4_volume_stacked.png",
  p4,
  width = 16,
  height = 10
)


# ==============================================================================
# 6. ADDITIONAL PT TRIPS ENABLED BY DRT FEEDER
# ==============================================================================

# Identify trips that shifted TO feeder modes (these are NEW PT trips)
# Exclude input_mode == "pt" since those were already PT users
new_pt_trips <- all_mode_shifts |>
  filter(
    str_detect(output_mode, "_feeder$"), # Shifted to a feeder mode
    input_mode != "pt" # Exclude existing PT users
  ) |>
  mutate(
    # Categorize source modes
    source_category = case_when(
      input_mode %in% c("car", "taxi") ~ "Motorised",
      input_mode %in% c("walk", "bike") ~ "Active",
      input_mode == "car_passenger" ~ "Car Passenger",
      TRUE ~ "Other"
    )
  )

# --- Summary Table: New PT Trips by Source Category ---
new_pt_summary <- new_pt_trips |>
  group_by(scenario, fleet_size, filter_type, source_category) |>
  summarise(
    new_pt_trips = sum(trips),
    .groups = "drop"
  ) |>
  group_by(scenario, fleet_size, filter_type) |>
  mutate(
    total_new_pt = sum(new_pt_trips),
    pct_of_new_pt = round(new_pt_trips / total_new_pt * 100, 1)
  ) |>
  ungroup()

# --- Summary: Total New PT Trips ---
new_pt_totals <- new_pt_trips |>
  group_by(scenario, fleet_size, filter_type) |>
  summarise(
    total_new_pt_trips = sum(trips),
    from_motorised = sum(trips[source_category == "Motorised"]),
    from_active = sum(trips[source_category == "Active"]),
    from_car_passenger = sum(trips[source_category == "Car Passenger"]),
    .groups = "drop"
  ) |>
  mutate(
    pct_from_motorised = round(from_motorised / total_new_pt_trips * 100, 1),
    pct_from_active = round(from_active / total_new_pt_trips * 100, 1)
  )

# Save summary
write_csv(new_pt_totals, "plots/mode_share/new_pt_trips_from_feeder.csv")

print("New PT trips enabled by DRT Feeder:")
print(new_pt_totals)

# --- Plot 5: Total New PT Trips by Source Category ---
p5 <- ggplot(
  new_pt_summary,
  aes(x = factor(fleet_size), y = new_pt_trips, fill = source_category)
) +
  geom_col(position = "stack") +
  ggh4x::facet_nested(
    filter_type ~ scenario,
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "New PT Users Enabled by DRT Feeder",
    subtitle = "Trips that shifted from non-PT modes to DRT Feeder (= new PT trips)",
    x = "Fleet Size",
    y = "Number of New PT Trips",
    fill = "Previous Mode Category"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p5

ggsave(
  "plots/mode_share/plot_5_new_pt_trips_from_feeder.png",
  p5,
  width = 12,
  height = 8
)

# --- Plot 6: Comparison - Motorised vs Active Sources ---
# This highlights the policy-relevant question: Is DRT bringing car users to PT?

new_pt_long <- new_pt_totals |>
  select(
    scenario,
    fleet_size,
    filter_type,
    from_motorised,
    from_active,
    pct_from_motorised,
    pct_from_active
  ) |>
  pivot_longer(
    cols = c(from_motorised, from_active),
    names_to = "source",
    values_to = "trips"
  ) |>
  mutate(
    # Map the correct percentage to the row based on source
    pct = if_else(
      source == "from_motorised",
      pct_from_motorised,
      pct_from_active
    ),
    source_label = if_else(
      source == "from_motorised",
      "From Car/Taxi",
      "From Walk/Bike"
    )
  )

p6 <- ggplot(
  new_pt_long,
  aes(x = factor(fleet_size), y = trips, fill = source_label)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Add Percentage Labels
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 2.5
  ) +
  ggh4x::facet_nested(
    filter_type ~ scenario,
    labeller = labeller(scenario = scenario_labels)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.2)) # Add space for labels
  ) +
  scale_fill_manual(
    values = c("From Car/Taxi" = "#E41A1C", "From Walk/Bike" = "#4DAF4A")
  ) +
  labs(
    title = "Impact of DRT feeders on additional Public Transport trips",
    subtitle = "Distribution of mode shift to pt from private and active modes",
    x = "Fleet Size",
    y = "Number of New PT Trips",
    fill = "Previous Mode"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p6

ggsave(
  "plots/mode_share/plot_6_new_pt_motorised_vs_active.png",
  p6,
  width = 12,
  height = 8
)
