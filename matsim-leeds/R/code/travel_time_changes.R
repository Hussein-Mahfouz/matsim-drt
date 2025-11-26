library(tidyverse)


# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones", "all", "innerBUA")

# fleet_sizes <- c(100, 200, 500, 1000)
fleet_sizes <- c(100, 200, 500)


# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Construct the file path
  file_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv")

  # Check if file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)  # Safe fail
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
legs_base_scenario = read_delim("../scenarios/basic/sample_1.00/eqasim_legs.csv")





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


# Step 4: Add data from the base scenario (to get baseline travel times for comparison)

legs_base_scenario = tidytable::as_tidytable(legs_base_scenario)

# Calculate travel time for each trip


legs_base_scenario = legs_base_scenario %>%
  tidytable::arrange(person_id, person_trip_id) %>%
  tidytable::group_by(person_id, person_trip_id) %>%
  tidytable::summarise(
    travel_time_baseline = sum(travel_time, na.rm = TRUE)) %>%
  tidytable::ungroup()


# Add data from the base scenario to the scenarios data frame

legs_tt = legs_tt %>%
  tidytable::left_join(legs_base_scenario, by = c("person_id", "person_trip_id")) %>%
  tidytable::mutate(
    travel_time_change = (travel_time - travel_time_baseline) / 60,  # convert to minutes
    # Calculate the percentage change in travel time
    travel_time_change_percentage = (travel_time - travel_time_baseline) / travel_time_baseline * 100)




# Step 5: Compare travel times for trips that use drt_feeder and drt_standalone

# boxplot showing travel_time_change for drt_feeder and drt_standalone, faceted
# by scenario and fleet_size, and colors = time of day

# filter legs to only include trips that have drt_feeder or drt_standalone
legs_tt <- legs_tt %>%
  tidytable::filter(drt_feeder | drt_standalone) %>%
  # Create a new column to indicate if the trip is a feeder or standalone
  tidytable::mutate(
    drt_type = ifelse(drt_feeder, "Feeder", "Standalone"),
    departure_time_interval = as.factor(departure_time_interval)
  )

# prepare columns for plotting
legs_tt <- legs_tt %>%
  mutate(scenario = case_when(
    scenario == "zones" ~ "drtNE|NW",
    scenario == "innerBUA" ~ "drtInner",
    scenario == "all" ~ "drtAll"))


# Plot travel time change for drt_feeder and drt_standalone trips (boxplot with each box being a departure time interval)
ggplot(legs_tt %>%
         filter(departure_time_interval != "0-3"),
       aes(x = drt_type, y = travel_time_change, fill = departure_time_interval)) +
  geom_boxplot() +
  facet_grid(scenario ~ fleet_size) +
  labs(
    title = "Travel time change for all trips that used DRT",
    x = "DRT type (Feeder vs Standalone)",
    y = "Net travel time change (min)"
  ) +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    name = "Departure time (3hr Interval)") +
  theme(legend.position = "bottom")

# save plot
ggsave("plots/travel_time_changes/drt_trips_time_change.png", width = 12)

# similar plot but showing number of trips that use drt_feeder and drt_standalone per time bucket
ggplot(legs_tt %>%
         filter(departure_time_interval != "0-3")
       , aes(x = drt_type, fill = departure_time_interval)) +
  geom_bar(position = "dodge") +
  facet_grid(scenario ~ fleet_size) +
  labs(
    title = "Number of trips that used DRT",
    x = "DRT Type (Feeder vs Standalone)",
    y = "Number of trips"
  ) +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2",
                    name = "Departure time (3hr Interval)") +
  theme(legend.position = "bottom")

# save plot
ggsave("plots/travel_time_changes/drt_trips_per_interval.png", width = 12)




