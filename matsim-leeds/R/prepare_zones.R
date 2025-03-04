library(tidyverse)
library(sf)
library(fs) # reading in data
library(tmap)


# ---------------- Read in study area
study_area = st_read("../../../drt-potential/data/interim/study_area_boundary.geojson") %>%
  st_union()

# ---------------- Read in gtfs feeds
gtfs_bus = st_read("../../../drt-potential/data/interim/gtfs_freq/gtfs_bus_sf_temporal.geojson")

gtfs_bus = gtfs_bus %>%
  mutate(`veh / hour` = 3600 / headway_secs)

gtfs_bus = gtfs_bus %>%
  filter(startsWith(scenario, "pt_wkday")) %>%
  mutate(
    time = case_when(
      scenario == "pt_wkday_06_30" ~ "05:00 - 08:00",
      scenario == "pt_wkday_09_30" ~ "08:00 - 11:00",
      scenario == "pt_wkday_12_30" ~ "11:00 - 14:00",
      scenario == "pt_wkday_15_30" ~ "14:00 - 17:00",
      scenario == "pt_wkday_18_30" ~ "17:00 - 20:00",
      TRUE ~ scenario # Keep original if it doesn't match
    )
  )

gtfs_rail = st_read("../../../drt-potential/data/interim/gtfs_freq/gtfs_rail_sf_temporal.geojson")

gtfs_rail = gtfs_rail %>%
  st_filter(study_area, .predicate = st_intersects)

gtfs_rail = gtfs_rail %>%
  mutate(`veh / hour` = 3600 / headway_secs)

gtfs_rail = gtfs_rail %>%
  filter(startsWith(scenario, "pt_wkday")) %>%
  mutate(
    time = case_when(
      scenario == "pt_wkday_06_30" ~ "05:00 - 08:00",
      scenario == "pt_wkday_09_30" ~ "08:00 - 11:00",
      scenario == "pt_wkday_12_30" ~ "11:00 - 14:00",
      scenario == "pt_wkday_15_30" ~ "14:00 - 17:00",
      scenario == "pt_wkday_18_30" ~ "17:00 - 20:00",
      TRUE ~ scenario # Keep original if it doesn't match
    )
  )
# ---------------- Read in the DRT Zones / Original Clusters

# Set directory path containing the GeoJSON files
geojson_dir <- "../data/external/drt_zones/"
# Get all .geojson files in the directory
geojson_files <- fs::dir_ls(geojson_dir, glob = "*.geojson")

# Read all GeoJSON files into a named list
drt_zones_data <- setNames(
  lapply(geojson_files, st_read, quiet = TRUE),
  fs::path_ext_remove(fs::path_file(geojson_files))  # Extract filenames without extension
)

# Print the names of loaded GeoDataFrames
print(names(drt_zones_data))

# To access a specific file's data
# drt_zones_data[["pt_wkday_06_30_scenario_3_cluster_drt_zones"]]


# --------------- Extract Clusters

# Cluster numbers are not consistent across time, as there is no way to trace a cluster
# across time points. We do a manual inspection of the resuls, and then define what cluster
# n at time t maps onto at other time points. The function below automates the extraction

extract_clusters <- function(clusters,
                             cluster_var_suffix = "poly",
                             buffer = 1000,
                             union_or_largest = NULL,
                             check_touches_largest = FALSE) {
  #' Create a combined cluster polygon dataset. This function takes a list of sfs - each one
  #' representing clusters from one point in time - and returns one sf. For each point in time,
  #' we specify the cluster we want to extract, and whether we want to return the largest
  #' polygon in the cluster or a union of all geometries
  #'
  #'
  #' @param clusters Named list, mapping time keys (e.g., "06_30") to cluster numbers.
  #' @param cluster_var_suffix Character, suffix for cluster dataset names ("poly" or "drt_zones").
  #' @param buffer  how much to buffer the geometry by (units = meters)
  #' @param union_or_largest Named list indicating whether to "union" or keep the "largest" polygon for each time key.
  #' @param check_touches_largest Logical, If this is TRUE, we check which geometries intersect with out largest geometry, and
  #' union them with it. Only applies when union_or_largest = largest
  #'
  #' @return An `sf` object with combined and processed cluster polygons.

  # Define time intervals corresponding to time keys
  times <- c("05:00 - 08:00", "08:00 - 11:00", "11:00 - 14:00", "14:00 - 17:00", "17:00 - 20:00")
  time_keys <- c("06_30", "09_30", "12_30", "15_30", "18_30")

  # Default to "union" for all if union_or_largest is not provided
  if (is.null(union_or_largest)) {
    union_or_largest <- setNames(rep("union", length(time_keys)), time_keys)
  }

  # Process each time period
  cluster_list <- map2(time_keys, times, ~ {
    # Construct dataset name dynamically
    key <- paste0("pt_wkday_", .x, "_scenario_3_cluster_", cluster_var_suffix)

    # Check if the dataset exists in drt_zones_data
    if (!key %in% names(drt_zones_data)) stop(paste("Missing dataset:", key))

    # Extract and filter the dataset based on the cluster mapping
    cluster_sf <- drt_zones_data[[key]] %>%
      filter(cluster == clusters[[.x]]) %>%
      mutate(time = .y)  # Add time column

    # Decide whether to take the union or the largest polygon
    if (nrow(cluster_sf) > 1) {
      if (union_or_largest[[.x]] == "union") {
        cluster_sf <- cluster_sf %>%
          summarise(geometry = st_union(geometry), .groups = "drop") %>%
          mutate(time = .y)  # Re-add time after summarise removes it
      } else if (union_or_largest[[.x]] == "largest") {
        # store original geometries  in case we use check_touches_largest
        cluster_sf_original <- cluster_sf
        # get largest geometry
        cluster_sf <- cluster_sf %>%
          mutate(area = as.numeric(st_area(geometry))) %>%  # Convert to numeric
          slice_max(order_by = area, n = 1, with_ties = FALSE) %>%  # Keep only the largest polygon
          select(-area)  # Remove area column

        if (check_touches_largest) {
          largest_geom <- cluster_sf$geometry[[1]]
          # Find only geometries that intersect with the largest geometry
          touch_indices <- st_intersects(cluster_sf_original, largest_geom, sparse = FALSE)[, 1]
          touching_geoms <- cluster_sf_original[touch_indices, ]
          # If there are touching geometries, union them
          cluster_sf <- touching_geoms %>%
            summarise(geometry = st_union(geometry), .groups = "drop") %>%
            mutate(time = .y)
         }
        }
       }
      return(cluster_sf)
    })

  cluster_poly <- bind_rows(cluster_list) %>%
    st_buffer(buffer)

  return(cluster_poly)
  }







map_clusters = function(cluster_sf,
                        headway_threshold_bus = 1800,
                        headway_threshold_rail,
                        geographic_area){

  tm_shape(study_area) +
    tm_borders(col = "black") +

    # --- Public Transport (All bus lines)
    tm_shape(gtfs_bus) +
    tm_lines(col = "grey55",
             alpha = 0.5) +
    tm_facets(by = "time",
              free.coords = FALSE,
              nrow = 2) +

    # --- High-Frequency Bus Lines
    tm_shape(gtfs_bus %>%
               filter(headway_secs <= headway_threshold_bus)) +
    tm_lines(col = "red",
             lwd = "veh / hour",
             scale = 2,
             title.lwd = "Buses / Hour \n(All routes with >= 3 buses / hour)") +
    tm_facets(by = "time",
              free.coords = FALSE) +

    # --- Rail
    # tm_shape(gtfs_rail %>%
    #            filter(headway_secs <= headway_threshold_rail))  +
    #   tm_lines(col = "darkblue",
    #            lwd = "veh / hour",
    #            legend.lwd.show = FALSE) +
    #   tm_facets(by = "time",
    #             free.coords = FALSE) +

    # --- Clusters
    tm_shape(cluster_sf) +
    tm_borders(col = "darkgreen",
               lwd = 3) +
    tm_facets(by = "time",
              free.coords = FALSE) +
    tm_shape(cluster_sf) +
    tm_fill(col = "darkgreen",
            alpha = 0.2) +
    tm_facets(by = "time",
              free.coords = FALSE) +

    # --- Layout & Title
    tm_layout(fontfamily = 'Georgia',
              main.title = geographic_area,
              main.title.size = 1.1,
              main.title.color = "azure4",
              main.title.position = "left",
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              legend.stack = "horizontal",
              frame = FALSE) -> cluster_map

  return(cluster_map)
}




# -------- North West Cluster

# a) clusters
clusters_nw_poly <- extract_clusters(
  clusters = list("06_30" = 3, "09_30" = 2, "12_30" = 4, "15_30" = 6, "18_30" = 5),
  cluster_var_suffix = "poly",
  union_or_largest = list("06_30" = "union", "09_30" = "union", "12_30" = "union", "15_30" = "union", "18_30" = "union")
)

clusters_nw_poly_map = map_clusters(clusters_nw_poly,
                                    headway_threshold_bus = 1800,
                                    geographic_area = "North West (Entire Cluster)")
clusters_nw_poly_map

# b) DRT zones
clusters_nw_drt <- extract_clusters(
  clusters = list("06_30" = 3, "09_30" = 2, "12_30" = 4, "15_30" = 6, "18_30" = 5),
  cluster_var_suffix = "drt_zones",
  union_or_largest = list("06_30" = "union", "09_30" = "union", "12_30" = "union", "15_30" = "union", "18_30" = "union")
)

clusters_nw_poly_map = map_clusters(clusters_nw_drt,
                                    headway_threshold_bus = 1800,
                                    geographic_area = "North West (DRT Zone)")
clusters_nw_poly_map



# -------- North East Cluster

# a) clusters
clusters_ne_poly <- extract_clusters(
  clusters = list("06_30" = 1, "09_30" = 1, "12_30" = 5, "15_30" = 2, "18_30" = 9),
  cluster_var_suffix = "poly",
  union_or_largest = list("06_30" = "largest", "09_30" = "largest", "12_30" = "largest", "15_30" = "union", "18_30" = "largest"),
)

clusters_ne_poly_map = map_clusters(clusters_ne_poly,
                                    headway_threshold_bus = 1800,
                                    geographic_area = "North East (Entire Cluster)")

clusters_ne_poly_map

# b) DRT zones
clusters_ne_drt <- extract_clusters(
  clusters = list("06_30" = 1, "09_30" = 1, "12_30" = 5, "15_30" = 2, "18_30" = 9),
  cluster_var_suffix = "drt_zones",
  union_or_largest = list("06_30" = "largest", "09_30" = "largest", "12_30" = "largest", "15_30" = "union", "18_30" = "largest"),
  check_touches_largest = TRUE
)

clusters_ne_drt_map = map_clusters(clusters_ne_drt,
                                   headway_threshold_bus = 1800,
                                   geographic_area = "North East (DRT Zone)")
clusters_ne_drt_map




