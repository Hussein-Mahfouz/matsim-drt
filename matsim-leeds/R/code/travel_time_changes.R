library(tidyverse)
library(ggh4x)


# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones", "all", "innerBUA")

fleet_sizes <- c(100, 200, 500, 1000)
# fleet_sizes <- c(100, 200, 500)

# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Construct the file path
  file_path <- paste0(
    "../scenarios/fleet_sizing/",
    scenario,
    "/",
    fleet_size,
    "/sample_1.00/",
    file_name,
    ".csv"
  )

  # Check if file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL) # Safe fail
  }

  # Print status
  print(paste("Reading file:", file_path))
  # Read the file
  data <- read_delim(file_path, delim = ";")
  # Add identifiers
  data_filtered <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  # # We are only interested in trips that have a drt leg. Let's do this pre-processing step here
  # # instead of on the whole dataset
  # trips_with_drt = data %>%
  #   group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  #   # keep only the groups where "drt" exists in one of the rows in the mode column
  #   filter(any(str_detect(mode, "drt"))) %>%
  #   ungroup()

  return(data_filtered)
}


# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ---------- Load in all  trips ---------- #
legs <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_legs")
})

# ---------- baseline data
legs_base_scenario = read_delim(
  "../scenarios/basic/sample_1.00/eqasim_legs.csv"
)


# -------------------------- Calculate travel time for each trip  -------------------------- #

legs = tidytable::as_tidytable(legs)

# Step 1: Identify if trip uses drt_feeder or drt

# Create new columns drt_feeder and drt
# if "mode" column contains rows with "drt" and "pt" then the drt is a feeder: add a new column "drt_feeder" with TRUE,
# else if "mode" column contains rows with "drt" but no row with "pt", the  new column "drt" with TRUE
# legs <- legs %>%
#   tidytable::group_by(person_id, person_trip_id, scenario, fleet_size) %>%
#   tidytable::mutate(
#     # Check if the GROUP contains ANY "drt" AND ANY "pt"
#     drt_feeder = any(str_detect(mode, "^drt")) & any(str_detect(mode, "pt")),
#     # Check if the GROUP contains ANY "drt" AND NOT ANY "pt"
#     drt_standalone = any(str_detect(mode, "^drt")) & !any(str_detect(mode, "pt"))
#   ) %>%
#   ungroup()

legs <- legs %>%
  tidytable::group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  tidytable::mutate(
    # Instead of running str_detect() multiple times, we determine the group's
    # conditions just once and store them in temporary logicals. This avoids
    # repeatedly scanning the 'mode' column for the same patterns.
    .has_drt = any(str_detect(mode, "^drt")),
    .has_pt = any(str_detect(mode, "pt")),

    # Now, use the pre-calculated logicals to create the final columns.
    # This is much faster as it's just a logical comparison.
    drt_feeder = .has_drt & .has_pt,
    drt_standalone = .has_drt & !.has_pt,

    # Clean up the temporary columns (drop them)
    .has_drt = NULL,
    .has_pt = NULL
  ) %>%
  ungroup()


# Step 2: Calculate travel time for each trip
legs_tt = legs %>%
  tidytable::arrange(person_id, person_trip_id) %>%
  tidytable::group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  tidytable::summarise(
    travel_time = sum(travel_time, na.rm = TRUE),
    departure_time = min(departure_time, na.rm = TRUE),
    drt_feeder = first(drt_feeder),
    drt_standalone = first(drt_standalone)
  ) %>%
  tidytable::ungroup()


# Step 3: Add a column to bucket the travel time into categories ased on departure time

# Departure time column has seconds up to 24 hours, so we can bucket it into n-hour intervals
hours_bucket = 3

# add a new column "departure_time_interval" that buckets the departure time into n-hour intervals
legs_tt = legs_tt %>%
  tidytable::mutate(
    departure_time_interval = cut(
      departure_time,
      breaks = seq(0, 24 * 3600, by = hours_bucket * 3600),
      labels = paste(
        seq(0, 24 - hours_bucket, by = hours_bucket),
        seq(hours_bucket, 24, by = hours_bucket),
        sep = "-"
      ),
      include.lowest = TRUE
    )
  )

# Step 4: Add baseline data (Travel Time + Baseline Mode)

legs_base_scenario = tidytable::as_tidytable(legs_base_scenario)

# Determine Baseline Trip Mode (Hierarchy: PT > Car > Bike > Walk)
# We aggregate the baseline legs to find the main mode of the trip
legs_base_processed = legs_base_scenario %>%
  tidytable::arrange(person_id, person_trip_id) %>%
  tidytable::group_by(person_id, person_trip_id) %>%
  tidytable::summarise(
    travel_time_baseline = sum(travel_time, na.rm = TRUE),
    # Logic to identify the main mode of the baseline trip
    baseline_mode = case_when(
      any(str_detect(mode, "pt")) ~ "Public Transport",
      any(mode == "car") ~ "Car",
      any(mode == "taxi") ~ "Taxi",
      any(mode == "bike") ~ "Bike",
      TRUE ~ "Walk" # Default fallback
    )
  ) %>%
  tidytable::ungroup()

# Join to main dataset
legs_tt = legs_tt %>%
  tidytable::left_join(
    legs_base_processed,
    by = c("person_id", "person_trip_id")
  ) %>%
  tidytable::mutate(
    travel_time_change = (travel_time - travel_time_baseline) / 60, # minutes
    travel_time_change_percentage = (travel_time - travel_time_baseline) /
      travel_time_baseline *
      100
  )


# Step 5: Plotting

# Filter for DRT usage and Clean Labels
legs_tt_plot <- legs_tt %>%
  tidytable::filter(drt_feeder | drt_standalone) %>%
  tidytable::filter(!is.na(baseline_mode)) %>%
  tidytable::mutate(
    drt_type = ifelse(drt_feeder, "Feeder", "Standalone"),
    scenario_label = case_when(
      scenario == "zones" ~ "drtNE|NW",
      scenario == "innerBUA" ~ "drtInner",
      scenario == "all" ~ "drtAll"
    ),
    # Ensure factors for ordering
    baseline_mode = factor(
      baseline_mode,
      levels = c("Car", "Taxi", "Public Transport", "Bike", "Walk")
    )
  ) %>%
  # Filter early morning outliers if needed
  filter(as.character(departure_time_interval) != "0-3")

# --- PLOT 1: Original (By Departure Time) ---
p_original <- ggplot(
  legs_tt_plot,
  aes(x = drt_type, y = travel_time_change, fill = departure_time_interval)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(outliers = FALSE, alpha = 0.8) +
  coord_cartesian(ylim = c(-100, 100)) +
  facet_grid(scenario_label ~ fleet_size) +
  scale_fill_brewer(palette = "Dark2", name = "Departure time (3hr Interval)") +
  labs(
    title = "Travel Time Impact (Time of Day)",
    x = "DRT Type",
    y = "Net travel time change (min)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

p_original

ggsave(
  "plots/travel_time_changes/drt_trips_time_change.png",
  p_original,
  width = 12,
  height = 10
)


# --- PLOT 2: Scenario Comparison (Side-by-Side / Dodged) ---
p_scenario <- ggplot(
  legs_tt_plot,
  aes(
    x = scenario_label,
    y = travel_time_change,
    fill = scenario_label,
    alpha = drt_type # Use transparency to distinguish Feeder vs Standalone within the same bar cluster
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(
    outliers = FALSE,
    position = position_dodge(width = 0.8) # Explicit dodge usually helps, though boxplot handles factors automatically
  ) +
  # Facet only by fleet size (columns), removing the row facet for drt_type
  facet_grid(. ~ fleet_size) +
  scale_fill_brewer(palette = "Set2") +
  scale_alpha_manual(
    values = c("Feeder" = 0.4, "Standalone" = 1.0),
    name = "DRT Service",
    guide = "none"
  ) +
  labs(
    title = "Travel Time Impact by Scenario",
    subtitle = "Comparing net travel time change. Solid = Standalone, Transparent = Feeder.",
    x = "Service Design",
    y = "Time Change (min)",
    fill = "Scenario"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_scenario

ggsave(
  "plots/travel_time_changes/drt_time_change_by_scenario.png",
  p_scenario,
  width = 12,
  height = 8
)


# --- PLOT 3: By Baseline Mode (Nested Facets) ---
# Improvement: Uses ggh4x::facet_nested to grouping headers (Fleet Size -> DRT Type)
p_origin <- ggplot(
  legs_tt_plot,
  aes(x = baseline_mode, y = travel_time_change, fill = baseline_mode)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(outliers = FALSE, alpha = 0.8) +
  # coord_cartesian(ylim = c(-45, 65)) +

  # Nested Facets: Groups Fleet Size headers together cleanly
  # Rows: Scenario
  # Cols: Fleet Size -> DRT Type
  ggh4x::facet_nested(
    scenario_label ~ fleet_size + drt_type
  ) +

  scale_fill_brewer(palette = "Set1", name = "Previous Mode") +
  labs(
    title = "Travel time change of DRT users relative to their original mode",
    subtitle = "Relative time change aggregated by fleet size and service type",
    x = "Original Mode",
    y = "Time Change (min)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(), # Remove X text as fill/legend explains mode
    axis.ticks.x = element_blank()
  )

p_origin

ggsave(
  "plots/travel_time_changes/drt_time_change_by_origin_mode_split.png",
  p_origin,
  width = 14,
  height = 10
)


# --- PLOT 4: By Baseline Mode (Side-by-Side / Dodged) ---
# New Plot: Feeder and Standalone NEXT to each other.
# We dodge by 'drt_type' and distinct them using Transparency (Alpha)

p_origin_dodged <- ggplot(
  legs_tt_plot,
  aes(
    x = baseline_mode,
    y = travel_time_change,
    fill = baseline_mode,
    alpha = drt_type # Use transparency to distinguish Feeder vs Standalone
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_boxplot(outliers = FALSE) +
  # coord_cartesian(ylim = c(-45, 65)) +

  # Simplified Facet: Just Scenario vs Fleet Size
  # DRT Type is now combined inside the plot area
  ggh4x::facet_nested(scenario_label ~ fleet_size) +

  scale_fill_brewer(palette = "Set1", name = "Previous Mode") +
  scale_alpha_manual(
    values = c("Feeder" = 0.4, "Standalone" = 1.0),
    name = "DRT Service",
    guide = "none"
  ) +
  labs(
    title = "Travel time change of DRT users relative to their original mode",
    subtitle = "Solid = Standalone, Transparent = Feeder",
    x = "Original Mode",
    y = "Time Change (min)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_origin_dodged

ggsave(
  "plots/travel_time_changes/drt_time_change_by_origin_mode_combined.png",
  p_origin_dodged,
  width = 14,
  height = 8
)
