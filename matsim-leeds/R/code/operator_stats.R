library(tidyverse)
library(sf)
library(tmap)


# --- Plots directory
plots_dir <- "plots/operator_stats/"
# Ensure the directory exists (optional)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# ----------  Breakdown of vehicles by occupancy (throughout the day) ---------- #


# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("all", "zones", "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)

# Function to read and process results of multiple scenarios, and combine them into one df
# This is used specifically for output_occupancy_time_profiles .txt files
# Unique identifier columns are add (scenario, fleet_size)
read_and_process <- function(scenario, fleet_size) {
  # Print the scenario and fleet size to verify function call
  print(paste("Processing scenario:", scenario, "Fleet size:", fleet_size))

  # Define directory path
  dir_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/")

  # List all matching files
  files <- list.files(path = dir_path, pattern = "^output_occupancy_time_profiles_.*\\.txt$", full.names = TRUE)

  # Print found files
  print(paste("Files found:", paste(files, collapse = ", ")))

  # If no matching file found, return NULL (to be ignored in `pmap_dfr`)
  if (length(files) == 0) {
    warning(paste("No matching file found for", scenario, fleet_size))
    return(NULL)
  }

  # Read and process all matching files
  data_list <- lapply(files, function(file) {
    # Extract file suffix (e.g., "drt", "drtNE", "drtNW" from "output_occupancy_time_profiles_drt.csv")
    file_suffix <- gsub("^output_occupancy_time_profiles_|\\.txt$", "", basename(file))
    print(paste("Processing file:", file, "Suffix:", file_suffix))


    # Read the file
    data <- read_delim(file, delim = ";")

    # Add columns for scenario, fleet size, and file suffix (operator ID)
    data %>%
      mutate(scenario = scenario,
             fleet_size = fleet_size,
             operator_id = file_suffix)
  })

  # Combine all files for this scenario & fleet size
  bind_rows(data_list)
}

# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# Read & combine all DRT trip data
drt_occupancy_time <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size)
})


drt_occupancy_time_l <- drt_occupancy_time %>%
  pivot_longer(cols = -c(time, scenario, fleet_size, operator_id),
               names_to = "Category",
               values_to = "Vehicles") %>%
  group_by(time, scenario, fleet_size, operator_id) %>%
  mutate(Vehicles_Share = Vehicles / sum(Vehicles)) %>%
  ungroup() %>%
  filter(minute(time) %% 30 == 0) %>% # A value every 5 minutes is excessive
  filter(as.numeric(time) < 24 * 60 * 60) %>% # Remove data points for time > 24
  mutate(time = as.POSIXct(time, format = "%H:%M:%S"))




drt_occupancy_time_l$Category <- forcats::fct_relevel(drt_occupancy_time_l$Category, "STAY", "RELOCATE",
                                                      "0 pax", "1 pax", "2 pax", "3 pax", "4 pax")

# --- Line plot
ggplot(drt_occupancy_time_l #%>%
         #filter(!(Category %in% c("STAY", "RELOCATE")))
       ,
       aes(x = time, y = Vehicles_Share, color = Category)) +
  geom_line() +
  scale_x_datetime(date_breaks = "4 hour",    # Adjust to show more labels every hour
                   date_labels = "%H:%M",
                   minor_breaks = "1 hour") +    # Show only hour and minute
  labs(title = "Vehicle occupancy by time of day",
       x = "",
       y = "Proportion of Vehicles",
       color = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(plots_dir, "vehicle_occupancy_line_facet_scenario_and_fleet_size.png"), bg = "white")

# --- Area plot
ggplot(drt_occupancy_time_l # %>%
        # filter(!(Category %in% c("STAY", "RELOCATE")))
       , aes(x = time, y = Vehicles_Share, fill = Category)) +
  geom_area(position = "stack") +
  scale_x_datetime(date_breaks = "4 hour",  # Adjust to show more labels every hour
                   date_labels = "%H:%M",
                   minor_breaks = "1 hour") +  # Show only hour and minute
  labs(title = "Vehicle occupancy by time of day",
       x = "",
       y = "Proportion of Vehicles",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(plots_dir, "vehicle_occupancy_area_facet_scenario_and_fleet_size.png"), bg = "white")

# ----------- DRT VKM travelled vs Passenger KM ---------- #

# Function to read and process a file with scenario & fleet size identifiers
read_and_process <- function(scenario,
                             fleet_size,
                             file_name,
                             base_dir = "../scenarios/fleet_sizing/",
                             sample_size = "1.00",
                             extension = "csv",
                             delim = ";") {

  # Construct file path
  file_path <- file.path(base_dir, scenario, as.character(fleet_size), sample_size, paste0(file_name, ".", extension))

  # Check if file exists to avoid errors
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)  # Return NULL to be ignored in pmap_dfr
  }

  # Print message to indicate which file is being read
  message("Reading file: ", file_path)
  # Read the file
  data <- read_delim(file_path, delim = delim, show_col_types = FALSE)

  # Add scenario and fleet size as metadata
  data <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  return(data)
}


# Create a dataframe of all combinations
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes, stringsAsFactors = FALSE)

# Read & combine all DRT trip data

# --- DRT vehicle KMs
drt_vkm <- purrr::pmap_dfr(combinations, function(scenario, fleet_size){
  read_and_process(scenario = scenario,
                   fleet_size = fleet_size,
                   file_name = "eqasim_drt_vehicle_movements",
                   base_dir = "../scenarios/fleet_sizing",
                   sample_size = "sample_1.00",
                   extension = "csv",
                   delim = ";")
})


# times as hour of day
drt_vkm_time = drt_vkm %>%
  mutate(departure_time_h = round(departure_time / 3600),
         arrival_time_h = round(arrival_time / 3600))

# total distance travlled by hour of day
drt_vkm_time = drt_vkm_time %>%
  group_by(arrival_time_h, operator_id, scenario, fleet_size) %>%
  summarise(distance_km = round(sum(distance) / 1000)) %>%
  mutate(metric = "vkm") %>%
  ungroup() %>%
  group_by(operator_id, scenario, fleet_size) %>%
  mutate(distance_km_cum = cumsum(distance_km)) %>% # cumulative distance
  ungroup()



# --- DRT Passenger KMs
# drt_pkm = read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/eqasim_drt_passenger_rides.csv"), delim =";")
drt_pkm = purrr::pmap_dfr(combinations, function(scenario, fleet_size){
  read_and_process(scenario = scenario,
                   fleet_size = fleet_size,
                   file_name = "eqasim_drt_passenger_rides",
                   base_dir = "../scenarios/fleet_sizing",
                   sample_size = "sample_1.00",
                   extension = "csv",
                   delim = ";")
})

drt_pkm_time = drt_pkm %>%
  mutate(departure_time_h = round(departure_time / 3600),
         arrival_time_h = round(arrival_time / 3600))

drt_pkm_time = drt_pkm_time %>%
  group_by(arrival_time_h, operator_id, scenario, fleet_size) %>%
  summarise(distance_km = round(sum(distance) / 1000)) %>%
  mutate(metric = "pkm") %>%
  ungroup() %>%
  group_by(operator_id, scenario, fleet_size) %>%
  mutate(distance_km_cum = cumsum(distance_km)) %>% # cumulative distance
  ungroup()


# --- Plot

# --- Combine vkm with pkm
drt_km_time = drt_pkm_time %>%
  bind_rows(drt_vkm_time)

# --- Plot pkm and vkm every hour
ggplot(drt_km_time, aes(x = arrival_time_h, y = distance_km, fill = metric)) +
  geom_col(position = "dodge") +
  labs(title = "Vehicle and passenger km throughout the day",
       x = "",
       y = "Distance (km)",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     "")) +
  scale_y_continuous(#trans = "log10",
                     labels = scales::comma)

ggsave(paste0(plots_dir, "vkm_pkm_bar_facet_scenario_and_fleet_size.png"), bg = "white")


# --- Plot CUMULATIVE pkm and vkm every hour
ggplot(drt_km_time,
       aes(x = arrival_time_h, y = distance_km_cum, color = metric)) +
  geom_line() +
  labs(title = "Cumulative vehicle and passenger km throughout the day",
       x = "",
       y = "Distance (km)",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     ""))  +
  scale_y_continuous(#trans = "log10",
                     labels = scales::comma)

ggsave(paste0(plots_dir, "vkm_pkm_cumulative_line_facet_scenario_and_fleet_size.png"), bg = "white")


# ------ Load factor
drt_km_time_load_factor = drt_km_time %>%
  pivot_wider(names_from = metric,
              values_from = c(distance_km, distance_km_cum)) %>%
  mutate(load_factor = distance_km_pkm / distance_km_vkm,
         load_factor_cum = distance_km_cum_pkm / distance_km_cum_vkm)

# plot
ggplot(drt_km_time_load_factor %>%
         filter(arrival_time_h > 3, arrival_time_h < 24),
       aes(x = arrival_time_h, y = load_factor, fill = load_factor)) +
  geom_col(position = "dodge") +
  labs(title = "Ratio of occupied km throughout the day",
       subtitle = "passenger km / vehicle km",
       x = "",
       y = "pkm / vkm",
       fill = "Load factor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     "")) +
  scale_y_continuous(
    #trans = "log10",
    labels = scales::comma) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1)

ggsave(paste0(plots_dir, "load_factor_bar_facet_scenario_and_fleet_size.png"), bg = "white")

# plot (cumulative) - i.e. how load factor is changing

ggplot(drt_km_time_load_factor %>%
         filter(arrival_time_h > 3, arrival_time_h < 24),
       aes(x = arrival_time_h, y = load_factor_cum, color = load_factor_cum)) +
  geom_line(lwd = 2) +
  labs(title = "Ratio of occupied km (Evolution over the day)",
       subtitle = "Cumulative pkm / vkm from start of day",
       x = "",
       y = "pkm / vkm",
       color = "Cumulative \nload factor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     "")) +
  scale_y_continuous(
    #trans = "log10",
    labels = scales::comma) +
  scale_color_distiller(palette = "RdYlGn", direction = 1)

ggsave(paste0(plots_dir, "load_factor_cumulative_line_facet_scenario_and_fleet_size.png"), bg = "white")


# ----- Person and vehicle distance travelled per passenger trip

drt_passengers = drt_pkm %>%
  mutate(departure_time_h = round(departure_time / 3600),
         arrival_time_h = round(arrival_time / 3600)) %>%
  group_by(arrival_time_h, operator_id, scenario, fleet_size) %>%
  summarise(passengers = n()) %>%
  ungroup() %>%
  group_by(operator_id, scenario, fleet_size) %>%
  mutate(passengers_cum = cumsum(passengers)) %>% # cumulative distance
  ungroup()

# Prepare pkm and vkm for joining

drt_pkm_passengers = drt_km_time %>%
  pivot_wider(names_from = metric,
              values_from = c(distance_km, distance_km_cum)) %>%
  left_join(drt_passengers, by = c("arrival_time_h", "operator_id", "scenario", "fleet_size")) %>%
  # add columns that represent the distance travel by:
      # (a) passenger per trip (at every hour and a cumulative trend)
      # (b) vehicle per passenger trip  (at every hour and a cumulative trend)
  mutate(passenger_distance_per_trip = round(distance_km_pkm / passengers, 2),
         passenger_distance_per_trip_cum = round(distance_km_cum_pkm / passengers_cum, 2),
         vehicle_distance_per_trip = round(distance_km_vkm / passengers, 2),
         vehicle_distance_per_trip_cum = round(distance_km_cum_vkm / passengers_cum, 2))



# Pivot longer for facet plots
drt_pkm_passengers = drt_pkm_passengers %>%
  pivot_longer(cols = c(#distance_km_pkm, distance_km_cum_pkm,
                        #distance_km_vkm, distance_km_cum_vkm,
                        #passengers, passengers_cum,
                        passenger_distance_per_trip, passenger_distance_per_trip_cum,
                        vehicle_distance_per_trip, vehicle_distance_per_trip_cum),
               names_to = "metric")

# add group column for plotting
drt_pkm_passengers = drt_pkm_passengers %>%
  mutate(category = case_when(stringr::str_starts(metric,"passenger_") ~ "passenger",
                              stringr::str_starts(metric, "vehicle_") ~ "vehicle"),
         cumulative = case_when(stringr::str_ends(metric,"_cum") ~ "yes",
                                TRUE ~ "no"))

# Line plot (cumulative and non-cumulative measures)
ggplot(drt_pkm_passengers %>%
       filter(arrival_time_h > 4, arrival_time_h < 24),
       aes(x = arrival_time_h, y = value, colour = category, linetype = cumulative)) +
  geom_line() +
  labs(title = "Trip efficiency",
       subtitle = "Average passenger and vehicle distance per passenger trip (km)",
       x = "",
       y = "Distance per passenger trip (km)",
       color = "Distance (km)",
       linetype = "Cumulative?") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to bottom
        legend.direction = "horizontal") +  # Arrange items horizontally
  guides(colour = guide_legend(nrow = 2),  # Force a single-row legend
         linetype = guide_legend(nrow = 2)) +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 3 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     "")) +
  scale_y_continuous(
    #trans = "log10",
    labels = scales::comma)

ggsave(paste0(plots_dir, "distance_per_passenger_trip_line_facet_scenario_and_fleet_size.png"), bg = "white")


# Bar plot throughout day (not cumulative)
ggplot(drt_pkm_passengers %>%
         filter(arrival_time_h > 4, arrival_time_h < 24,
                # remove cumulative rows
                metric %in% c("passenger_distance_per_trip", "vehicle_distance_per_trip")),
       aes(x = arrival_time_h, y = value, fill = category)) +
  geom_col(position = "dodge") +
  labs(title = "Trip efficiency",
       subtitle = "Average passenger and vehicle distance per passenger trip (km)",
       x = "",
       y = "Distance per passenger trip (km)",
       fill = "Distance (km)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",  # Move legend to bottom
        legend.direction = "horizontal") +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 3 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     "")) +
  scale_y_continuous(
    #trans = "log10",
    labels = scales::comma)

ggsave(paste0(plots_dir, "distance_per_passenger_trip_bar_facet_scenario_and_fleet_size.png"), bg = "white")




# ------ Distance travelled by occupancy

drt_vkm_occupancy = drt_vkm %>%
  group_by(operator_id, number_of_passengers, scenario, fleet_size) %>%
  summarise(distance_km = sum(distance / 1000)) %>%
  ungroup()


ggplot(drt_vkm_occupancy, aes(x = factor(number_of_passengers), y = distance_km, fill = factor(number_of_passengers))) +
  geom_col() +
  facet_grid(vars(fleet_size), vars(operator_id)) +
  labs(title = "Total distance travelled by passenger load",
       x = "Number of Passengers",
       y = "Total Distance (KM)",
       fill = "Number of Passengers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(#trans = "log10",
    labels = scales::comma) +
  scale_fill_brewer(palette = "RdYlGn", direction = 1)

ggsave(paste0(plots_dir, "distance_by_passenger_count_bar_facet_scenario_and_fleet_size.png"), bg = "white")








# ------------------- Stats for a summary table

# pkm, vkm, and load factor
drt_km_time_table = drt_km_time %>%
  group_by(operator_id, scenario, fleet_size, metric) %>%
  summarise(distance_km_total = sum(distance_km)) %>%
  ungroup() %>%
  pivot_wider(names_from = metric, values_from = distance_km_total) %>%
  mutate(average_load_factor = round(pkm / vkm, 2))


# pkm and vkm per passenger trip
drt_pkm_passengers_table = drt_pkm_passengers %>%
  filter(cumulative == "no") %>%
  group_by(operator_id, scenario, fleet_size, metric) %>%
  summarise(average_distance_per_passenger_trip = round(mean(value, na.rm = TRUE), 1)) %>%
  ungroup() %>%
  pivot_wider(names_from = metric, values_from = average_distance_per_passenger_trip)


drt_table = drt_km_time_table %>%
  left_join(drt_pkm_passengers_table,
            by = c("operator_id", "scenario", "fleet_size"))


write_csv(drt_table, paste0(plots_dir, "drt_daily_stats.csv"))

