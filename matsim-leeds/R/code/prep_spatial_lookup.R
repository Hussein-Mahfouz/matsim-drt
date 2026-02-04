library(tidyverse)
library(sf)

# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================

dir.create("../data/interim", recursive = TRUE, showWarnings = FALSE)

# Load Boundaries
cluster_nw <- st_read(
  "../data/supply/drt/nw_cluster_08_00_11_00.shp",
  quiet = TRUE
) |>
  st_transform(3857)

cluster_ne <- st_read(
  "../data/supply/drt/ne_cluster_08_00_11_00.shp",
  quiet = TRUE
) |>
  st_transform(3857)

inner_zone <- st_read(
  "../data/supply/drt/built_up_area_leeds.shp",
  quiet = TRUE
) |>
  st_transform(3857)

study_area <- st_read(
  "../data/external/study_area_boundary.geojson",
  quiet = TRUE
) |>
  st_transform(3857)

# Combine clusters for the 'zones' scenario
cluster_zones_combined <- bind_rows(cluster_nw, cluster_ne) |> st_union()

# Load Baseline Trips
trips_df <- read_delim(
  "../scenarios/basic/sample_1.00/eqasim_trips.csv",
  delim = ";",
  show_col_types = FALSE
) |>
  select(
    person_id,
    person_trip_id,
    origin_x,
    origin_y,
    destination_x,
    destination_y
  )

print(paste("Processing", nrow(trips_df), "trips for spatial lookup..."))

# ==============================================================================
# 2. SPATIAL PROCESSING - TRIP-BASED FILTERS
# ==============================================================================

# Convert Trip Origins and Destinations to SF
origins_sf <- st_as_sf(
  trips_df,
  coords = c("origin_x", "origin_y"),
  crs = 3857,
  remove = FALSE
)

dests_sf <- st_as_sf(
  trips_df,
  coords = c("destination_x", "destination_y"),
  crs = 3857,
  remove = FALSE
)

# Function to check if Origin OR Destination is in the area
check_intersection <- function(geom) {
  o_in <- lengths(st_intersects(origins_sf, geom)) > 0
  d_in <- lengths(st_intersects(dests_sf, geom)) > 0
  return(o_in | d_in)
}

# Trip-based filters (origin OR destination in zone)
print("Calculating trip-based intersections for 'zones' (NE + NW)...")
in_zones_trip <- check_intersection(cluster_zones_combined)

print("Calculating trip-based intersections for 'innerBUA'...")
in_inner_trip <- check_intersection(inner_zone)

print("Calculating trip-based intersections for 'all' (Study Area)...")
in_all_trip <- check_intersection(study_area)

# ==============================================================================
# 3. SPATIAL PROCESSING - RESIDENT-BASED FILTERS
# ==============================================================================

# Identify home locations (first trip origin for each person)
print("Identifying home locations from first trip origins...")

home_locations <- trips_df |>
  filter(person_trip_id == 0) |>
  select(person_id, origin_x, origin_y)

home_sf <- st_as_sf(
  home_locations,
  coords = c("origin_x", "origin_y"),
  crs = 3857,
  remove = FALSE
)

# Check which people LIVE in each zone
print("Calculating resident intersections for 'zones' (NE + NW)...")
home_in_zones <- lengths(st_intersects(home_sf, cluster_zones_combined)) > 0

print("Calculating resident intersections for 'innerBUA'...")
home_in_inner <- lengths(st_intersects(home_sf, inner_zone)) > 0

print("Calculating resident intersections for 'all' (Study Area)...")
home_in_all <- lengths(st_intersects(home_sf, study_area)) > 0

# Create person-level lookup for residence
person_residence_lookup <- home_locations |>
  select(person_id) |>
  mutate(
    resident_zones = home_in_zones,
    resident_innerBUA = home_in_inner,
    resident_all = home_in_all
  )

# ==============================================================================
# 4. COMBINE AND SAVE LOOKUP
# ==============================================================================

# Create trip-level lookup with both trip-based and resident-based filters
lookup_df <- trips_df |>
  select(person_id, person_trip_id) |>
  mutate(
    # Trip-based filters (origin OR destination in zone)
    filter_zones = in_zones_trip,
    filter_innerBUA = in_inner_trip,
    filter_all = in_all_trip
  ) |>
  # Join resident-based filters

  left_join(person_residence_lookup, by = "person_id")

# Save as RDS for fast loading in R
saveRDS(lookup_df, "../data/interim/trips_spatial_lookup.rds")
write_csv(lookup_df, "../data/interim/trips_spatial_lookup.csv")

print("Done! Spatial lookup saved to ../data/interim/trips_spatial_lookup.rds")
print(paste("Columns:", paste(names(lookup_df), collapse = ", ")))
