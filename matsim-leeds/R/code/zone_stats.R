library(tidyverse)
library(sf)

# This script looks at population and trips from ach DRT zone

# ---------- Read in the data

# --- Input demand
# demand_original = arrow::read_parquet("../data/demand/legs_with_locations.parquet")
# get it from the actual population output of acbm
# demand_original = read_csv("../../../acbm/data/outputs/e41e0014bd/people.csv")
# get it from the trips data (some people may be missing - do not have a trip, or got stuck in the analysis)
demand_original = read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";"
)


# Boundaries of each DRT zone

# --------------- Other spatial layers for plotting

# ----- some useful datasets for plotting
study_area = st_read("../data/external/study_area_boundary.geojson") %>%
  st_union() %>%
  st_transform(3857) %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(scenario = "drtAll")

cluster_nw = st_read("../data/supply/drt/nw_cluster_08_00_11_00.shp") %>%
  st_transform(3857) %>%
  mutate(scenario = "drtNW")

cluster_ne = st_read("../data/supply/drt/ne_cluster_08_00_11_00.shp") %>%
  st_transform(3857) %>%
  mutate(scenario = "drtNE")

# Inner zone based on Built up area
inner_zone = st_read("../data/supply/drt/built_up_area_leeds.shp") %>%
  st_transform(3857) %>%
  mutate(scenario = "drtInner")

# bind together for plotting
scenario_extents =
  study_area %>%
  bind_rows(cluster_ne) %>%
  bind_rows(cluster_nw) %>%
  bind_rows(inner_zone) %>%
  select(scenario)


# Calculate population in each zone. We use the demand data and keep only the first trip

people = demand_original %>%
  # keep only the first trip
  filter(person_trip_id == 0) %>%
  distinct(person_id, .keep_all = TRUE)

people_sf = people %>%
  st_as_sf(coords = c("origin_x", "origin_y"), crs = 3857)


# calculate number of people living in each zone
people_per_zone = st_join(people_sf, scenario_extents, join = st_intersects) %>%
  st_drop_geometry() %>%
  group_by(scenario) %>%
  summarise(population = n()) %>%
  ungroup()


# Calculate trips starting in each zone. We use all of the demand data

trips_sf = demand_original %>%
  st_as_sf(coords = c("origin_x", "origin_y"), crs = 3857)

# calculate number of trips starting in each zone
trips_in_zones = st_join(trips_sf, scenario_extents, join = st_intersects)


# calculate number of trips starting in each zone (per mode then total)
trips_per_zone = trips_in_zones %>%
  st_drop_geometry() %>%
  group_by(scenario, mode) %>%
  summarise(trips = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = mode, values_from = trips, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric))))

# same as above but add %
trips_per_zone_pct = trips_per_zone %>%
  mutate(across(bike:walk, ~ paste0(., " (", round(. / total * 100, 1), "%)")))


# Combine population and trips data

zone_stats = people_per_zone %>%
  left_join(trips_per_zone, by = "scenario") %>%
  mutate(trips_per_person = total / population) %>%
  filter(!is.na(scenario))

# Write the results to a CSV file
write_csv(zone_stats, "plots/zone_stats/zone_stats.csv")

zone_stats_pct = people_per_zone %>%
  left_join(trips_per_zone_pct, by = "scenario") %>%
  filter(!is.na(scenario))


# Write the results to a CSV file
write_csv(zone_stats_pct, "plots/zone_stats/zone_stats_pct.csv")
