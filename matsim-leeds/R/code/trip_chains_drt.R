# This is a draft script to show the unique combinations of trip chains that include a feeder DRT trip. It is meant to illustrate
# what trips that use DRT look like

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
  data <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  # We are only interested in trips that have a drt leg. Let's do this pre-processing step here
  # instead of on the whole dataset
  trips_with_drt = data %>%
    group_by(person_id, person_trip_id, scenario, fleet_size) %>%
    # keep only the groups where "drt" exists in one of the rows in the mode column
    filter(any(str_detect(mode, "drt"))) %>%
    ungroup()

  return(trips_with_drt)
}


# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ----------  All DRT trips ---------- #

# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
legs <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_legs")
})

# convert any mode that contains "drt" to "drt"
legs <- legs %>%
  mutate(mode = case_when(
    str_detect(mode, "drt") ~ "drt",
    TRUE ~ mode
  ))

#############################################################################################
# -------- Analysis 1: Get distinct combination of trip chains that include a feeder DRT trip
#############################################################################################

# 1. Remove consecutive walk legs (e.g. walk-drt-walk-walk-pt ---> walk-drt-walk-pt ..... it is just one walk leg really)
# Function to remove consecutive duplicates from a character vector
collapse_consecutive <- function(x) {
  x[c(TRUE, x[-1] != x[-length(x)])]
}

# Step 1: Collapse consecutive identical modes
trip_chains <- legs %>%
  arrange(scenario, fleet_size, person_trip_id, leg_index) %>%
  group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  summarise(
    trip_chain = paste(collapse_consecutive(mode), collapse = "->"),
    #trip_chain = paste(mode, collapse = "-"),
    .groups = "drop"
  )

# Step 2: Count occurrences of each trip chain
trip_chains_unique = trip_chains %>%
  group_by(trip_chain, scenario, fleet_size) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  group_by(scenario, fleet_size) %>%
  mutate(percentage = round(count / sum(count) * 100)) %>%
  ungroup()

# Plot the distribution of trip chains
ggplot(trip_chains_unique %>%
         filter(percentage >=1)
       , aes(x = reorder(trip_chain, count), y = percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of trip chains: all trips with a DRT leg", x = "Trip Chain", y = "Percent") +
  theme_bw() +
  facet_grid(scenario ~ fleet_size)

# save plot (name should include fleet_sizes)
ggsave("plots/trip_chains_drt/trip_chain_combinations.png", width = 10, height = 6)



#############################################################################################
# -------- Analysis 2: Get the distribution of travel time by mode at each leg of the trip chain
#############################################################################################


# Step 1: Add the trip chain for each unique person trip (for grouping)
trips_with_drt_chain <- legs %>%
  arrange(person_id, person_trip_id, leg_index, scenario, fleet_size) %>%
  group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  mutate(
    trip_chain = paste(collapse_consecutive(mode), collapse = "->")
    #trip_chain = paste(mode, collapse = "-"),
  ) %>%
  ungroup()

# Step 2: Remove consecutive walk legs

# a: identify legs where there is no mode transition
trips_with_drt_chain = trips_with_drt_chain %>%
  arrange(person_id, person_trip_id, leg_index, scenario, fleet_size) %>%
  group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  # is the mode different from the previous mode?
  mutate(mode_shift = mode != lag(mode, default = "")) %>%
  # group is based on the mode_shift, if mode changes, then group increases by one
  mutate(group = cumsum(mode_shift)) %>%
  ungroup()

# b: use the group column to remove consecutive walk legs
trips_with_drt_chain = trips_with_drt_chain  %>%
  # summarise to remove consecutive walk legs
  group_by(person_id, person_trip_id, group, trip_chain, scenario, fleet_size) %>%
  summarise(
    #person_id = first(person_id),
    #person_trip_id = first(person_trip_id),
    #leg_index = first(leg_index),  # optional
    # trip_chain = first(trip_chain),
    mode = first(mode),
    travel_time = sum(travel_time)) %>%
  ungroup()

# Step 3: Plot
# Join the unique counts / percentages onto the trips
trips_with_drt_chain <- trips_with_drt_chain %>%
  inner_join(trip_chains_unique, by = c("trip_chain", "scenario", "fleet_size"))

# Keep only chains where % is higher than a certain threshold of occurrences
trips_with_drt_chain_sample <- trips_with_drt_chain %>%
  filter(percentage >= 3)

# a: Add column with leg index. We use this for the whisker plot
trips_with_drt_chain_plot <- trips_with_drt_chain_sample %>%
  group_by(person_id, person_trip_id, scenario, fleet_size) %>%
  mutate(leg_index = row_number()) %>%
  ungroup() %>%
  # new mode column (mode - leg_index)
  mutate(mode_leg = paste(leg_index, mode, sep = "-")) %>%
  # convert travel time to minutes
  mutate(travel_time = round(travel_time / 60),
         trip_chain_pct = paste0(trip_chain, " (", percentage, "%)"))


scenario = "zones"
# b: Create the whisker plot (boxplot)
ggplot(trips_with_drt_chain_plot %>%
         filter(scenario ==scenario),
       aes(x = mode_leg, y = travel_time, fill = mode)) +
  geom_boxplot(outlier.alpha = 0.3) +
  #facet_wrap(~ trip_chain_pct, scales = "free_x") +
  labs(
    x = "Mode (in order of appearance)",
    y = "Travel Time (minutes)",
    title = "Distribution of travel time by mode for each unique trip chain"
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 6)) +
  facet_grid(fleet_size ~ trip_chain)

# save plot
ggsave(paste0("plots/trip_chains_drt/trip_chains_unique-travel_time_distributions-scenario_", scenario, ".png"), width = 16)
























##################################################################
# Same analysis but only for one scenario and fleet size
##################################################################











scenario = "zones"
fleet_size = 200
file_name = "eqasim_legs"

legs =  read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv"), delim = ";")

# keep only trips (unique: person_id + person_trip_id) that have at least one row where mode is drt
trips_with_drt = legs %>%
  group_by(person_id, person_trip_id) %>%
  # keep only the groups where "drt" exists in one of the rows in the mode column
  filter(any(str_detect(mode, "drt"))) %>%
  ungroup()

#############################################################################################
# -------- Analysis 1: Get distinct combination of trip chains that include a feeder DRT trip
#############################################################################################

# 1. Remove consecutive walk legs (e.g. walk-drt-walk-walk-pt ---> walk-drt-walk-pt ..... it is just one walk leg really)
# Function to remove consecutive duplicates from a character vector
collapse_consecutive <- function(x) {
  x[c(TRUE, x[-1] != x[-length(x)])]
}

# Step 0: create a mode_abr column that is an abbreviation of the mode column (e.g. walk = w)
# trips_with_drt <- trips_with_drt %>%
#   mutate(mode_abr = case_when(
#     mode == "walk" ~ "w",
#     # if mode contains "drt", replace it with "drt"
#    # str_detect(mode, "drt") ~ "drt",
#     mode == "pt" ~ "pt",
#     mode == "car" ~ "car",
#     mode == "car_passenger" ~ "cp",
#     mode == "bike" ~ "b",
#     mode == "taxi" ~ "t",
#     TRUE ~ mode
#   ))

# Step 1: Collapse consecutive identical modes
trip_chains <- trips_with_drt %>%
  arrange(person_trip_id, leg_index) %>%
  group_by(person_id, person_trip_id) %>%
  summarise(
    trip_chain = paste(collapse_consecutive(mode), collapse = "->"),
    #trip_chain = paste(mode, collapse = "-"),
    .groups = "drop"
  )

# Step 2: Count occurrences of each trip chain
trip_chains_unique = trip_chains %>%
  group_by(trip_chain) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  # calculate the percentage of each trip chain
  mutate(percentage = round(count / sum(count) * 100))

# Plot the distribution of trip chains
ggplot(trip_chains_unique, aes(x = reorder(trip_chain, count), y = percentage)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of Trip Chains with DRT", x = "Trip Chain", y = "Percent") +
  theme_bw()

# save plot
ggsave(paste0("plots/trip_chains_drt/trip_chain_combinations_fleet_size_", fleet_size, "_scenario_", scenario, ".png"), width = 10, height = 6)


#############################################################################################
# -------- Analysis 2: Get the distribution of travel time by mode at each leg of the trip chain
#############################################################################################


# Step 1: Add the trip chain for each unique person trip (for grouping)
trips_with_drt_chain <- trips_with_drt %>%
  arrange(person_id, person_trip_id, leg_index) %>%
  group_by(person_id, person_trip_id) %>%
  mutate(
    trip_chain = paste(collapse_consecutive(mode), collapse = "->")
    #trip_chain = paste(mode, collapse = "-"),
  ) %>%
  ungroup()

# Step 2: Remove consecutive walk legs

# a: identify legs where there is no mode transition
trips_with_drt_chain = trips_with_drt_chain %>%
  arrange(person_id, person_trip_id, leg_index) %>%
  group_by(person_id, person_trip_id) %>%
  # is the mode different from the previous mode?
  mutate(mode_shift = mode != lag(mode, default = "")) %>%
  # group is based on the mode_shift, if mode changes, then group increases by one
  mutate(group = cumsum(mode_shift)) %>%
  ungroup()

# b: use the group column to remove consecutive walk legs
trips_with_drt_chain = trips_with_drt_chain  %>%
  # summarise to remove consecutive walk legs
  group_by(person_id, person_trip_id, group, trip_chain) %>%
  summarise(
    leg_index = first(leg_index),  # optional
    mode = first(mode),
    travel_time = sum(travel_time)) %>%
  ungroup()

# Step 3: Plot

# Join the unique counts / percentages onto the trips
trips_with_drt_chain <- trips_with_drt_chain %>%
  inner_join(trip_chains_unique, by = "trip_chain")

# Keep only chains where % is higher than a certain threshold of occurrences
trips_with_drt_chain_sample <- trips_with_drt_chain %>%
  filter(percentage >= 3)

# a: Add column with leg index. We use this for the whisker plot
trips_with_drt_chain_plot <- trips_with_drt_chain_sample %>%
  group_by(person_id, person_trip_id) %>%
  mutate(leg_index = row_number()) %>%
  ungroup() %>%
  # new mode column (mode - leg_index)
  mutate(mode_leg = paste(leg_index, mode, sep = "-")) %>%
  # convert travel time to minutes
  mutate(travel_time = round(travel_time / 60),
         trip_chain_pct = paste0(trip_chain, " (", percentage, "%)"))


# b: Create the whisker plot (boxplot)
ggplot(trips_with_drt_chain_plot, aes(x = mode_leg, y = travel_time, fill = mode)) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ trip_chain_pct, scales = "free_x") +
  labs(
    x = "Mode (in order of appearance)",
    y = "Travel Time (minutes)",
    title = "Distribution of Travel Time by Mode in Each Unique Trip Chain"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 6))

# save plot
ggsave(paste0("plots/trip_chains_drt/trip_chains_unique-travel_time_distributions_fleet_size_", fleet_size, "_scenario_", scenario, ".png"), width = 12)

