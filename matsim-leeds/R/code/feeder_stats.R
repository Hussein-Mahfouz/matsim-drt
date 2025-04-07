library(tidyverse)
library(tidytransit)
library(tmap)

# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones", "all")
fleet_sizes <- c(100, 200, 500, 1000)

# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Read the data
  file_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv")
  data <- read_delim(file_path, delim = ";")

  # Add the scenario and fleet size columns
  data <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  return(data)
}

# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ----------  All DRT trips ---------- #

# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
drt_trips <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_drt_passenger_rides")
})

# Add a column to identify if this is a feeder or not
drt_trips <- drt_trips %>%
  mutate(mode_type = ifelse(str_detect(main_mode, "_feeder"), "feeder", "main")) %>%
  # for better labelling
  mutate(fleet_size_label = factor(paste0("fleet size = ", fleet_size),
                             levels = paste0("fleet size = ", c(100, 200, 500, 1000))))

# ----------  All DRT FEEDER trips only---------- #

drt_feeder_trips <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_feeder_drt_trips")
})
# where are feeder trips happening?
# what PT routes are they connecting to?

drt_feeder_trips <- drt_feeder_trips %>%
  # for better labelling
  mutate(fleet_size_label = factor(paste0("fleet size = ", fleet_size),
                             levels = paste0("fleet size = ", c(100, 200, 500, 1000))))



# --------------- Working with ALL TRIPS (FEEDER + STANDALONE) ---------------- #



# ----- Distance and waiting time by mode (operator + feeder or not)
drt_trips_distance = drt_trips %>%
  group_by(operator_id, mode_type, scenario, fleet_size, fleet_size_label) %>%
  summarise(distance = round(sum(distance) / 1000),
            waiting_time = round(mean(waiting_time, na.rm = TRUE)),
            no_of_trips = n()) %>%
  ungroup() %>%
  group_by(operator_id, scenario, fleet_size, fleet_size_label) %>%
  mutate(distance_frac = round((distance / sum(distance)) * 100)) %>%
  ungroup()


# Plot distance by mode type (feeder / standalone) for each scenario
ggplot(drt_trips_distance, aes(x = operator_id, y = distance, fill = mode_type)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = paste0(distance_frac, "%"),
                 hjust = ifelse(mode_type == "feeder", -0.5, 1)),  # Feeder goes right, standalone goes left
             position = position_stack(vjust = 0.5),
             color = "white", size = 3, label.size = 0) +
  labs(title = "DRT Usage: Feeder vs. Standalone Trips",
       subtitle = "Share of travel distance by feeder trips vs. full DRT trips for different service areas",
       x = "Scenario",
       y = "Total Distance (KM)",
       fill = "") +
  scale_y_continuous(labels = scales::comma) +  # Add commas to the y-axis values
  theme_light() +
  facet_wrap(. ~ fleet_size_label)

ggsave(paste0("plots/feeder_stats/standalone_vs_feeder_bar_facet_fleet_size.png"))


# ----- Distance travelled by feeder vs standalone (bucketed by hour of day)
drt_trips_distance_time = drt_trips %>%
  mutate(departure_time_hr = round(departure_time / 3600),
         arrival_time_hr = round(arrival_time / 3600)) %>%
  group_by(operator_id, mode_type, departure_time_hr, scenario, fleet_size, fleet_size_label) %>%
  summarise(distance = sum(distance) / 1000,
            waiting_time = mean(waiting_time, na.rm = TRUE),
            no_of_trips = n()) %>%
  ungroup() %>%
  group_by(operator_id, departure_time_hr, scenario, fleet_size, fleet_size_label) %>%
  mutate(distance_frac = round((distance / sum(distance)) * 100))



ggplot(drt_trips_distance_time, aes(x = departure_time_hr, y = distance, fill = mode_type)) +
  geom_bar(stat = "identity") +
  facet_grid(operator_id ~ fleet_size_label) +
  #facet_grid(fleet_size ~ operator_id) +
  labs(title = "DRT Usage: Feeder vs. Standalone Trips throughout the day",
       subtitle = "Share of travel distance by feeder trips vs. full DRT trips for different service areas",
       x = "Hour of Day",
       y = "Total Distance (KM)",
       fill = "") +
  scale_y_continuous(labels = scales::comma) +  # Add commas to the y-axis values
  theme_light()

ggsave(paste0("plots/feeder_stats/standalone_vs_feeder_bar_temporal_facet_fleet_size.png"))




# -------------------- Working with FEEDER TRIPS ONLY --------------------- #




drt_feeder_trips = drt_feeder_trips %>%
  mutate(trip_type = case_when(
    !is.na(access_departure_time) & !is.na(egress_departure_time) ~ "access_egress",
    is.na(egress_departure_time) ~ "access",
    is.na(access_departure_time) ~ "egress",
    TRUE ~ "neither"  # Keeps other cases as NA if needed
  ))

# Add hour column for grouping
drt_feeder_trips = drt_feeder_trips %>%
  mutate(access_departure_time_hr = round(access_departure_time / 3600),
         egress_departure_time_hr = round(egress_departure_time / 3600))

# --- Add operator ID

# lookup table to match vehciles to operators
vehicle_operator_link = drt_trips %>%
  select(operator_id, vehicle_id) %>%
  #filter(!is.na(vehicle_id) %>% # not working for some reason (using line below for now)
  filter(str_starts(vehicle_id, "drt_")) %>%
  distinct()

# add the operator ID
drt_feeder_trips = drt_feeder_trips %>%
  left_join(vehicle_operator_link %>%
              rename("access_operator_id" = "operator_id"),
            by = c("access_vehicle_id" = "vehicle_id")) %>%
  left_join(vehicle_operator_link %>%
              rename("egress_operator_id" = "operator_id"),
            by = c("egress_vehicle_id" = "vehicle_id"))


# ----- Work on each type of trip separately

# access trips only
drt_feeder_trips_access = drt_feeder_trips %>%
  filter(trip_type == "access") %>%
  group_by(access_transit_line_id, trip_type, access_departure_time_hr, access_operator_id, scenario, fleet_size, fleet_size_label) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  rename(transit_line_id = access_transit_line_id,
         departure_time_hr = access_departure_time_hr,
         operator_id = access_operator_id)

# egress trips only
drt_feeder_trips_egress = drt_feeder_trips %>%
  filter(trip_type == "egress") %>%
  group_by(egress_transit_line_id, trip_type, egress_departure_time_hr, egress_operator_id, scenario, fleet_size, fleet_size_label) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  rename(transit_line_id = egress_transit_line_id,
         departure_time_hr = egress_departure_time_hr,
         operator_id = egress_operator_id)

# access_egress: access part
drt_feeder_trips_access_2 = drt_feeder_trips %>%
  filter(trip_type == "access_egress") %>%
  group_by(access_transit_line_id, trip_type, access_departure_time_hr, access_operator_id, scenario, fleet_size, fleet_size_label) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  rename(transit_line_id = access_transit_line_id,
         departure_time_hr = access_departure_time_hr,
         operator_id = access_operator_id)



# access_egress: egress part
drt_feeder_trips_egress_2 = drt_feeder_trips %>%
  filter(trip_type == "access_egress") %>%
  group_by(egress_transit_line_id, trip_type, egress_departure_time_hr, egress_operator_id, scenario, fleet_size, fleet_size_label) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  rename(transit_line_id = egress_transit_line_id,
         departure_time_hr = egress_departure_time_hr,
         operator_id = egress_operator_id)



# ----- Add all together
drt_trips_feeder_lines_time = bind_rows(
  drt_feeder_trips_access,
  drt_feeder_trips_egress,
  drt_feeder_trips_access_2,
  drt_feeder_trips_egress_2)

# character so that it matches gtfs data type
drt_trips_feeder_lines_time = drt_trips_feeder_lines_time %>%
  mutate(transit_line_id = as.character(transit_line_id))



# sum without accounting for hour of day
drt_trips_feeder_lines = drt_trips_feeder_lines_time %>%
  group_by(transit_line_id, operator_id, scenario, fleet_size, fleet_size_label) %>%
  summarise(trips = sum(trips)) %>%
  ungroup()





# ----------------- Prepare GTFS

# Load in the GTFS feed

gtfs = read_gtfs("../data/external/study_area_gtfs_merged.zip")

# gtfs_rail = read_gtfs("../data/external/study_area_gtfs_rail.zip")
# gtfs_bus = read_gtfs("../data/external/study_area_gtfs_bus.zip")


gtfs_sf = gtfs_as_sf(gtfs, crs = 3857)

gtfs_routes = gtfs_sf$trips %>%
  select(route_id, shape_id) %>% # Take a random shape for now (there are different shapes for the same route depending on the service)
  distinct(route_id, .keep_all = TRUE)

# Add geometry to route_id
gtfs_routes = gtfs_routes %>%
  left_join(gtfs_sf$shapes, by = "shape_id") %>%
  st_as_sf()


# --------------- Other spatial layers for plotting

# ----- some useful datasets for plotting
study_area = st_read("../data/external/study_area_boundary.geojson") %>%
  st_union() %>%
  st_transform(3857) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(scenario = "drt")

cluster_nw = st_read("../data/supply/drt/nw_cluster_08_00_11_00.shp") %>%
  st_transform(3857) %>%
  mutate(scenario = "drtNW")

cluster_ne = st_read("../data/supply/drt/ne_cluster_08_00_11_00.shp") %>%
  st_transform(3857) %>%
  mutate(scenario = "drtNE")

# bind together for plotting
scenario_extents =
  study_area %>%
  bind_rows(cluster_ne) %>%
  bind_rows(cluster_nw)

# Expand scenario_extents for each fleet size (for facet plot)
scenario_extents <- scenario_extents %>%
  tidyr::crossing(fleet_size = fleet_sizes) %>%
  st_as_sf()

# Add geometry to feeder stats df
drt_trips_feeder_lines_sf = drt_trips_feeder_lines %>%
  left_join(gtfs_routes, by = c("transit_line_id" = "route_id")) %>%
  st_as_sf()


# --- Spatial plots

tm_shape(study_area) +
  tm_borders(lwd = 3) +
tm_shape(study_area) +
  tm_fill(col = "white") +
tm_shape(scenario_extents) +
  tm_borders(col = "darkgreen",
             lwd = 3.5,
             lty = "dashed") +
  tm_facets(by = c("scenario", "fleet_size"),
            free.coords = FALSE) +
  # tm_shape(gtfs_sf$shapes) +
  #   tm_lines(col = "grey75",
  #            alpha = 0.2) +
  tm_shape(drt_trips_feeder_lines_sf %>%
             filter(!st_is_empty(.))) +
  tm_lines(col = "trips",
           title.col = "Number of \nfeeder \nDRT trips",
           lwd = "trips",
           palette = "Reds",
           scale = 5,
           legend.lwd.show = FALSE) +
  # tm_facets(by = c("operator_id", "fleet_size"),
  tm_facets(by = c("operator_id", "fleet_size"),
            free.coords = FALSE) +
  tm_layout(fontfamily = 'Georgia',
            main.title = "Number of Feeder DRT trips connecting to each bus routes",
            main.title.size = 1.1,
            main.title.color = "azure4",
            main.title.position = "left",
            bg.color = "#FAF9F6",
            # legend.outside = TRUE,
            # legend.outside.position = "bottom",
            # legend.stack = "horizontal",
            # panel.label.size = 1,
            # panel.label.bg.color = "grey",
            #panel.labels = 1:length(unique(clusters_vis_mode_poly_filt_max$cluster)),
            frame = FALSE)  +
  tm_add_legend(type = "line", labels = 'Service area', col = 'darkgreen', lwd = 2, lty = "dashed") -> drt_feeder_bus_count

drt_feeder_bus_count

tmap_save(tm = drt_feeder_bus_count, filename = "plots/feeder_stats/map_drt_feeder_bus_route_count.png", width = 12, dpi = 1080, asp = 0)




# ------------------- Stats for a summary table

# distance, proportion of trips, and total number of trips (main and feeder)
drt_trips_distance_table = drt_trips_distance %>%
  pivot_wider(names_from = mode_type,
              values_from = c("distance", "distance_frac", "waiting_time", "no_of_trips"))

# add fraction of trips done by feeders
drt_trips_distance_table = drt_trips_distance_table %>%
  mutate(no_of_trips_feeder_frac = (round(no_of_trips_feeder / (no_of_trips_main + no_of_trips_feeder), 2)) * 100)





# Average no. of feeder trips per bus route (all bus routes, and top 5 bus routes)
drt_trips_feeder_lines_table = drt_trips_feeder_lines %>%
  group_by(operator_id, scenario, fleet_size) %>%
  arrange(desc(trips), .by_group = TRUE) %>%  # Ensure sorting within each group
  summarise(average_feeder_trips_per_route = round(sum(trips)/ n()),
            average_feeder_trips_per_route_top_5_routes = round(mean(trips[1:5]))  # Average trips for top 5 routes
            ) %>%
  ungroup()


drt_feeder_trips_table = drt_trips_distance_table %>%
  left_join(drt_trips_feeder_lines_table,
            by = c("operator_id", "scenario", "fleet_size"))


write_csv(drt_feeder_trips_table, "plots/feeder_stats/drt_feeder_daily_stats.csv")
