library(tidyverse)
library(arrow)
library(ggalluvial)
library(ggh4x)

# This script looks at transitions in mode share. We compare all trip modes
# before and after the simulation.
# Outputs:
# - One combined CSV with all filter types identified by a 'filter_type' column
# - Comparison plots across filter types

# ---------- Read in the data

demand_original <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";"
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

# Setup
scenarios <- c("zones", "all", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)

scenario_labels <- c(
  "zones" = "Zone-based DRT (NE/NW",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
)

# Filter type definitions
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
    description = "Person lives in service zone",
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

# Read all scenario data
demand_matsim <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_trips")
})

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
  "Filtering to %s common trips (of %s baseline, %s min across scenarios)",
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


# ---------- Prepare data for joining

demand_original_prep <- demand_original |>
  rename(pid = person_id) |>
  select(pid, person_trip_id, mode) |>
  rename_with(~ paste0("input_", .), .cols = c("mode"))

# Global input mode shares (for labelling)
demand_original_prep <- demand_original_prep |>
  group_by(input_mode) |>
  mutate(input_mode_trips = n()) |>
  ungroup() |>
  mutate(input_mode_trips_frac = round((input_mode_trips / n()) * 100, 1)) |>
  mutate(
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

# ---------- Calculate mode shift for all filter types

calculate_mode_shift <- function(data, filter_type_name, filter_fn) {
  results_list <- list()

  for (scen in scenarios) {
    for (fs in fleet_sizes) {
      # Filter data for this scenario/fleet_size
      df_subset <- data |>
        filter(scenario == scen, fleet_size == fs)

      if (nrow(df_subset) == 0) {
        next
      }

      # Apply the filter
      keep_rows <- filter_fn(df_subset, scen)
      df_filtered <- df_subset[keep_rows, ]

      if (nrow(df_filtered) == 0) {
        next
      }

      # Recalculate local mode shares
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

# Calculate for all filter types
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
# COMPARISON PLOTS (CLEANED & SPLIT)
# ==============================================================================

# 1. Prepare Main Data: Split DRT Output into Standalone vs Feeder
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
# Question: How much total demand is DRT capturing?
# Improvement: Distinguishes "Real" DRT trips (Standalone) from Access trips (Feeder)

drt_share_summary <- drt_split_data |>
  group_by(scenario, fleet_size, filter_type, drt_service) |>
  summarise(
    drt_trips = sum(trips),
    # To get total system trips, sum local_input_trips for unique input_modes
    total_system_trips = sum(local_input_trips) / n_distinct(input_mode),
    drt_share_pct = drt_trips / total_system_trips * 100,
    .groups = "drop"
  )

p1 <- ggplot(
  drt_share_summary,
  aes(
    x = factor(fleet_size),
    y = drt_share_pct,
    fill = filter_type
  )
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_grid(
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
  plot = p1,
  width = 12,
  height = 8
)


# --- Plot 2: Percent Shift by Original Mode (Split by Service Type) ---
# Question: What % of Car/PT/Walk trips switch to Feeder vs Standalone?
# Improvement: Faceted by Mode to reduce clutter. Linetype distinguishes service.

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
  plot = p2,
  width = 12,
  height = 12
)


# --- Plot 3: Volume & Intensity (Less Cluttered) ---
# Question: What is the absolute impact (Volume) vs relative impact (Intensity)?
# Improvement: Separation of Feeder/Standalone into columns. Labels cleaned up.

p3 <- ggplot(
  mode_shift_pct,
  aes(
    x = filter_type,
    y = pct_shifted,
    fill = input_mode
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  # Label: Absolute number of trips
  geom_text(
    aes(
      label = scales::comma(trips_shifted),
      group = input_mode
    ),
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
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.4)) # Extra space for labels
  ) +
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
  plot = p3,
  width = 16,
  height = 10
)

# --- Plot 3b: Volume & Intensity Flipped ---
# Y-Axis: Absolute Volume (Trips)
# Label: Intensity (%)
p3b <- ggplot(
  mode_shift_pct,
  aes(
    x = filter_type,
    y = trips_shifted,
    fill = input_mode
  )
) +
  geom_col(position = position_dodge(width = 0.9)) +
  # Label: Percentage (Intensity)
  geom_text(
    aes(
      label = paste0(round(pct_shifted, 1), "%"),
      group = input_mode
    ),
    position = position_dodge(width = 0.9),
    angle = 90, # Vertical text
    hjust = -0.2, # Just above bar
    vjust = 0.5, # Center alignment
    size = 2.2
  ) +
  ggh4x::facet_nested(
    fleet_size ~ scenario + drt_service,
    labeller = labeller(scenario = scenario_labels),
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.4)) # Room for text
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
    legend.position = "bottom",
    # strip.background = element_blank(),
    # ggh4x.facet.nestline = element_line(colour = "black")
  )

p3b

ggsave(
  "plots/mode_share/plot_3b_volume_bar_with_pct_labels.png",
  plot = p3b,
  width = 16,
  height = 10
)

# --- Plot 4: Volume Only (Stacked) ---
# Question: Where is the ridership coming from?
# Improvement: Clear stacked view separated by service type.

p4 <- ggplot(
  mode_shift_pct,
  aes(x = filter_type, y = trips_shifted, fill = input_mode)
) +
  geom_col(position = "stack") +
  facet_grid(
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
  plot = p4,
  width = 16,
  height = 10
)
