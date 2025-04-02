library(tidyverse)
library(sf)
library(tmap)

# ----------- Specfy scenario

# Output demand
scenario = "zones"
# scenario = "all"
fleet_size = 200 # 20, 100, 200, 500, 1000
drt_name = "drtNW"

#  --------------- CONTINUE BUILDING ON THIS TO PLOT SCENARIOS!
#
# # Set up a list of scenarios and fleet sizes to read in (file directories should exist)
# scenarios <- c("all", "zones")
# fleet_sizes <- c(100, 200, 500, 1000)
#
# # Function to read and process all matching files
# read_and_process <- function(scenario, fleet_size) {
#   # Define directory path
#   dir_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/")
#
#   # List all matching files
#   files <- list.files(path = dir_path, pattern = "^output_occupancy_time_profiles_.*\\.txt$", full.names = TRUE)
#
#   # If no matching file found, return NULL (to be ignored in `pmap_dfr`)
#   if (length(files) == 0) {
#     warning(paste("No matching file found for", scenario, fleet_size))
#     return(NULL)
#   }
#
#   # Read and process all matching files
#   data_list <- lapply(files, function(file) {
#     # Extract file suffix (e.g., "drt", "drtNE", "drtNW" from "output_occupancy_time_profiles_drt.csv")
#     file_suffix <- gsub("^output_occupancy_time_profiles_|\\.txt$", "", basename(file))
#
#     # Read the file
#     data <- read_delim(file, delim = ";")
#
#     # Add columns for scenario, fleet size, and file suffix (operator ID)
#     data %>%
#       mutate(scenario = scenario,
#              fleet_size = fleet_size,
#              operator_id = file_suffix)
#   })
#
#   # Combine all files for this scenario & fleet size
#   bind_rows(data_list)
# }
#
# # Create a data frame of all combinations of scenarios and fleet sizes to read in
# combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)
#
# # Read & combine all DRT trip data
# drt_occupancy_time <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
#   read_and_process(scenario, fleet_size)
# })
#






# ----------  Breakdown of vehicles by occupany (throughout the day) ---------- #

drt_occupancy_time = read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/output_occupancy_time_profiles_", drt_name, ".txt"), delim =";")


drt_occupancy_time_l <- drt_occupancy_time %>%
  pivot_longer(cols = -time, names_to = "Category", values_to = "Vehicles") %>%
  group_by(time) %>%
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ----------- DRT VKM travelled vs Passenger KM ---------- #

# --- DRT vehicle KMs
drt_vkm = read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/eqasim_drt_vehicle_movements.csv"), delim =";")

# times as hour of day
drt_vkm_time = drt_vkm %>%
  mutate(departure_time_h = round(departure_time / 3600),
         arrival_time_h = round(arrival_time / 3600))

# total distance travlled by hour of day
drt_vkm_time = drt_vkm_time %>%
  group_by(arrival_time_h, operator_id) %>%
  summarise(distance_km = sum(distance) / 1000) %>%
  mutate(metric = "vkm") %>%
  ungroup() %>%
  group_by(operator_id) %>%
  mutate(distance_km_cum = cumsum(distance_km)) %>% # cumulative distance
  ungroup()



# --- DRT Passenger KMs
drt_pkm = read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/eqasim_drt_passenger_rides.csv"), delim =";")


drt_pkm_time = drt_pkm %>%
  mutate(departure_time_h = round(departure_time / 3600),
         arrival_time_h = round(arrival_time / 3600))

drt_pkm_time = drt_pkm_time %>%
  group_by(arrival_time_h, operator_id) %>%
  summarise(distance_km = sum(distance) / 1000) %>%
  mutate(metric = "pkm") %>%
  ungroup() %>%
  group_by(operator_id) %>%
  mutate(distance_km_cum = cumsum(distance_km)) %>% # cumulative distance
  ungroup()

# --- Plot

# --- Combine vkm with pkm
drt_km_time = drt_pkm_time %>%
  bind_rows(drt_vkm_time)

# --- Plot pkm and vkm every hour
ggplot(drt_km_time, aes(x = arrival_time_h, y = distance_km, color = metric)) +
  geom_line() +
  labs(title = "Vehicle and passenger km throughout the day",
       x = "",
       y = "Distance (km)",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(. ~ operator_id) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     ""))

# --- Plot CUMULATIVE pkm and vkm every hour
ggplot(drt_km_time, aes(x = arrival_time_h, y = distance_km_cum, color = metric)) +
  geom_line() +
  labs(title = "Cumulative vehicle and passenger km throughout the day",
       x = "",
       y = "Distance (km)",
       color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(. ~ operator_id) +
  scale_x_continuous(breaks = seq(0, 24, by = 1),
                     labels = ifelse(seq(0, 24, by = 1) %% 4 == 0,
                                     paste0(seq(0, 24, by = 1), ":00"),
                                     ""))


# ------ Distance travelled by occupancy

drt_vkm_occupancy = drt_vkm %>%
  group_by(operator_id, number_of_passengers) %>%
  summarise(distance_km = sum(distance / 1000)) %>%
  ungroup()


ggplot(drt_vkm_occupancy, aes(x = factor(number_of_passengers), y = distance_km, fill = factor(number_of_passengers))) +
  geom_col() +
  facet_wrap(~ operator_id) +  #
  labs(title = "Total distance travelled by passenger load",
       x = "Number of Passengers",
       y = "Total Distance (KM)",
       fill = "Number of Passengers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


