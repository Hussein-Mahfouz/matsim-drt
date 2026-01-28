library(tidyverse)
library(arrow)
library(ggalluvial)
library(ggh4x)
library(data.table) # Required for fread

# This script looks at transitions in mode share.
# We verify the "Main Mode" by looking at trip legs.

# ... [Setup section remains the same] ...

scenarios <- c("zones", "all", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)

scenario_labels <- c(
  "zones" = "Zone-based DRT (NE/NW)",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
)

# ... [Spatial Filters section remains the same] ...
# (Ensure lookup_path loading block is kept here)
lookup_path <- "../data/interim/trips_spatial_lookup.rds"
if (file.exists(lookup_path)) {
  spatial_lookup <- readRDS(lookup_path)
} else {
  stop(
    "Spatial lookup file not found. Please run code/prep_spatial_lookup.R first."
  )
}

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
# 1. ROBUST & FAST DATA READING (HIERARCHY CHECK)
# ==============================================================================

read_and_process_corrected <- function(scenario, fleet_size) {
  base_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/"
  )
  trips_file <- paste0(base_path, "eqasim_trips.csv")
  legs_file <- paste0(base_path, "eqasim_legs.csv")

  if (!file.exists(trips_file) || !file.exists(legs_file)) {
    warning(paste("Missing files for:", scenario, fleet_size))
    return(NULL)
  }

  message(paste0(
    "[",
    format(Sys.time(), "%H:%M:%S"),
    "] Starting: ",
    scenario,
    " (",
    fleet_size,
    ")"
  ))

  # 1. Read Legs using fread
  legs <- fread(
    legs_file,
    sep = ";",
    select = c("person_id", "person_trip_id", "mode")
  )

  # 2. Apply Hierarchy using tidytable:: verbs for speed
  # Note: tidytable verbs accept data.tables or data.frames
  true_modes <- legs |>
    tidytable::summarise(
      has_pt = any(mode == "pt"),
      has_drt = any(mode %like% "drt"),
      has_car = any(mode == "car"),
      has_taxi = any(mode == "taxi"),
      has_passenger = any(mode == "car_passenger"),
      has_bike = any(mode == "bike"),
      first_mode = first(mode),
      .by = c(person_id, person_trip_id)
    ) |>
    tidytable::mutate(
      hierarchical_mode = case_when(
        has_pt ~ "pt", # Main mode is PT (covers pure PT and Feeder)
        has_drt ~ "drt", # Main mode is DRT (Standalone)
        has_car ~ "car",
        has_taxi ~ "taxi",
        has_passenger ~ "car_passenger",
        has_bike ~ "bike",
        TRUE ~ first_mode
      ),
      is_feeder = has_pt & has_drt
    )

  # 3. Read Trips
  trips <- fread(
    trips_file,
    sep = ";"
  )

  # 4. Join using tidytable defaults (fast)
  trips_corrected <- trips |>
    tidytable::left_join(true_modes, by = c("person_id", "person_trip_id")) |>
    tidytable::mutate(
      original_output_mode = mode,
      mode = hierarchical_mode,
      scenario = scenario,
      fleet_size = fleet_size
    ) |>
    tidytable::select(
      -has_pt,
      -has_drt,
      -has_car,
      -has_taxi,
      -has_passenger,
      -has_bike,
      -hierarchical_mode,
      -first_mode,
      -is_feeder
    )

  message(paste0(
    "[",
    format(Sys.time(), "%H:%M:%S"),
    "] Finished: ",
    scenario,
    " (",
    fleet_size,
    ")"
  ))

  # Explicitly return a standard tibble to play nice with downstream dplyr code
  return(as_tibble(trips_corrected))
}

# Load All Data
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)
demand_matsim <- purrr::pmap_dfr(combinations, read_and_process_corrected)

# ... [Rest of the script follows unchanged] ...
# Load Baseline
demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
)

# ==============================================================================
# 2. FILTER TO COMMON TRIPS (STUCK AGENT REMOVAL)
# ==============================================================================

# Create unique trip ID
demand_original <- demand_original |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

demand_matsim <- demand_matsim |>
  mutate(trip_id = paste(person_id, person_trip_id, sep = "_"))

# Find trips present in ALL scenarios
n_combos <- demand_matsim |> distinct(scenario, fleet_size) |> nrow()

trips_in_all_scenarios <- demand_matsim |>
  distinct(trip_id, scenario, fleet_size) |>
  count(trip_id, name = "n_combos_present") |>
  filter(n_combos_present == n_combos) |>
  pull(trip_id)

trips_in_baseline <- demand_original |> pull(trip_id) |> unique()
common_trips <- intersect(trips_in_all_scenarios, trips_in_baseline)

message(sprintf(
  "Filtering to %s common trips.",
  scales::comma(length(common_trips))
))

# Apply filter
demand_original <- demand_original |> filter(trip_id %in% common_trips)
demand_matsim <- demand_matsim |> filter(trip_id %in% common_trips)


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
    input_mode_trips_with_frac = paste0(
      input_mode,
      " (",
      input_mode_trips_frac,
      "%)"
    )
  )

demand_matsim_prep <- demand_matsim |>
  rename(pid = person_id) |>
  select(
    pid,
    person_trip_id,
    mode,
    original_output_mode,
    scenario,
    fleet_size
  ) |>
  rename(
    output_mode = mode, # This is the CORRECTED mode (pt, drt, etc)
    output_original = original_output_mode # This helps identify Feeder vs Standalone
  )

# Join
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
      # Filter subset
      df_subset <- data |> filter(scenario == scen, fleet_size == fs)
      if (nrow(df_subset) == 0) {
        next
      }

      # Apply spatial filter
      keep_rows <- filter_fn(df_subset, scen)
      df_filtered <- df_subset[keep_rows, ]
      if (nrow(df_filtered) == 0) {
        next
      }

      # Summarise
      # We group by BOTH output_mode (corrected) AND output_original (detail)
      # This preserves the ability to distinguish Feeder/Standalone later
      df_summary <- df_filtered |>
        group_by(input_mode) |>
        mutate(local_input_trips = n()) |>
        ungroup() |>
        mutate(local_input_frac = round((local_input_trips / n()) * 100, 1)) |>
        group_by(
          input_mode,
          local_input_trips,
          local_input_frac,
          output_mode, # e.g. "pt" (includes feeder)
          output_original # e.g. "drt_feeder"
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

# --- DRT Summary Table (Including Feeder trips which are now labeled PT) ---
# We use output_original to capture ALL trips that involved DRT (Standalone + Feeder)
all_mode_shifts_drt <- all_mode_shifts |>
  filter(str_detect(output_original, "drt")) |>
  mutate(
    trips_moved_drt_frac = round((trips / local_input_trips) * 100, 2)
  ) |>
  # Re-label output_mode using original to explicitly show Feeder vs Standalone in CSV
  mutate(output_mode_detail = output_original)

# Save
write_csv(all_mode_shifts, "plots/mode_share/mode_shift_all.csv")
write_csv(all_mode_shifts_drt, "plots/mode_share/mode_shift_drt.csv")


# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

# Prepare Data for Plots (Using output_original to distinguish services)
# - Standalone: output_mode = "drt"
# - Feeder: output_mode = "pt" BUT output_original contains "feeder"

drt_split_data <- all_mode_shifts |>
  # Keep trips that involve any DRT usage (Standalone or Feeder)
  filter(str_detect(output_original, "drt")) |>
  mutate(
    drt_service = if_else(
      str_detect(output_original, "_feeder") |
        str_detect(output_original, "drt.*_feeder"),
      "Feeder",
      "Standalone"
    )
  )

# --- Plot 1: Overall DRT Mode Share (Split: Feeder vs Standalone) ---

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
    title = "Total DRT Usage Share",
    subtitle = "Shared: 'Standalone' = DRT trips. 'Feeder' = PT trips accessed via DRT.",
    x = "Fleet Size",
    y = "Share of Total Trips (%)",
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
    subtitle = "Percentage of original mode shifting to DRT services",
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
# Question: Does the shift matter?

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
    title = "Mode Shift Impact: Intensity vs Volume",
    subtitle = "Bars = % Shifted (Intensity). Labels = Number of Trips (Volume).",
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
# Question: What generates ridership?

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
    title = "Mode Shift Volume: Volume vs Intensity",
    subtitle = "Bars = Number of Trips (Volume). Labels = % Shifted (Intensity).",
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
    subtitle = "Number of trips contributing to DRT usage (Feeder vs Standalone)",
    x = "Filter Type",
    y = "Total Trips",
    fill = "Source Mode"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

ggsave(
  "plots/mode_share/plot_4_volume_stacked.png",
  p4,
  width = 16,
  height = 10
)
