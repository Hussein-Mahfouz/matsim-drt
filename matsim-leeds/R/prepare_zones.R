library(tidyverse)
library(sf)
library(fs) # reading in data
library(tmap)


crs_to_use = 3857
plots_path = "plots/"

# ---------------- Read in study area
study_area = st_read("../../../drt-potential/data/interim/study_area_boundary.geojson") %>%
  st_union() %>%
  st_transform(crs_to_use)

# ---------------- Read in gtfs feeds
gtfs_bus = st_read("../../../drt-potential/data/interim/gtfs_freq/gtfs_bus_sf_temporal.geojson") %>%
  st_transform(crs_to_use)


gtfs_bus = gtfs_bus %>%
  mutate(`veh / hour` = round(3600 / headway_secs))

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

gtfs_rail = st_read("../../../drt-potential/data/interim/gtfs_freq/gtfs_rail_sf_temporal.geojson") %>%
  st_transform(crs_to_use)


gtfs_rail = gtfs_rail %>%
  st_filter(study_area, .predicate = st_intersects)

gtfs_rail = gtfs_rail %>%
  mutate(`veh / hour` = round(3600 / headway_secs))

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
             legend.lwd.show = FALSE,
             # title.lwd = paste0("Buses / Hour \n(All routes with >= ", round(3600/headway_threshold_bus), " buses / hour)")
             n = 3) +
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

    # --- Manually Add Legends
    tm_add_legend(type = "line",
                  title = "All Bus Routes",
                  # is.portrait = FALSE,
                  col = "grey55",
                  lwd = 2,
                  alpha = 0.5) +
    tm_add_legend(type = "line",
                  title = paste0("Routes with >= ", round(3600/headway_threshold_bus), "\nbuses / hour)"),
                  # is.portrait = FALSE,
                  col = "red",
                  lwd = 2,
                  alpha = 0.5) +
    tm_add_legend(type = "fill",
                  title = paste0("Cluster"),
                  # is.portrait = FALSE,
                  col = "darkgreen",
                  border.col = "darkgreen",
                  border.lwd = 3,
                  alpha = 0.2) +

    # --- Layout & Title
    tm_layout(fontfamily = 'Georgia',
              main.title = geographic_area,
              main.title.size = 1.1,
              main.title.color = "azure4",
              main.title.position = "left",
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              # legend.stack = "horizontal",
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
                                    headway_threshold_bus = 1200,
                                    geographic_area = "North West (Entire Cluster)")
clusters_nw_poly_map

tmap_save(tm = clusters_nw_poly_map, filename = paste0(plots_path, "clusters_nw_poly.png"), width = 12, dpi = 1080, asp = 0)


# b) DRT zones
clusters_nw_drt <- extract_clusters(
  clusters = list("06_30" = 3, "09_30" = 2, "12_30" = 4, "15_30" = 6, "18_30" = 5),
  cluster_var_suffix = "drt_zones",
  union_or_largest = list("06_30" = "union", "09_30" = "union", "12_30" = "union", "15_30" = "union", "18_30" = "union")
)

clusters_nw_drt_map = map_clusters(clusters_nw_drt,
                                    headway_threshold_bus = 1200,
                                    geographic_area = "North West (DRT Zone)")
clusters_nw_drt_map

tmap_save(tm = clusters_nw_drt_map, filename = paste0(plots_path, "clusters_nw_drt_zone.png"), width = 12, dpi = 1080, asp = 0)



# -------- North East Cluster

# a) clusters
clusters_ne_poly <- extract_clusters(
  clusters = list("06_30" = 1, "09_30" = 1, "12_30" = 5, "15_30" = 2, "18_30" = 9),
  cluster_var_suffix = "poly",
  union_or_largest = list("06_30" = "largest", "09_30" = "largest", "12_30" = "largest", "15_30" = "union", "18_30" = "largest"),
)

clusters_ne_poly_map = map_clusters(clusters_ne_poly,
                                    headway_threshold_bus = 1200,
                                    geographic_area = "North East (Entire Cluster)")

clusters_ne_poly_map

tmap_save(tm = clusters_ne_poly_map, filename = paste0(plots_path, "clusters_ne_poly.png"), width = 12, dpi = 1080, asp = 0)


# b) DRT zones
clusters_ne_drt <- extract_clusters(
  clusters = list("06_30" = 1, "09_30" = 1, "12_30" = 5, "15_30" = 2, "18_30" = 9),
  cluster_var_suffix = "drt_zones",
  union_or_largest = list("06_30" = "largest", "09_30" = "largest", "12_30" = "largest", "15_30" = "union", "18_30" = "largest"),
  check_touches_largest = TRUE
)

clusters_ne_drt_map = map_clusters(clusters_ne_drt,
                                   headway_threshold_bus = 1200,
                                   geographic_area = "North East (DRT Zone)")
clusters_ne_drt_map

tmap_save(tm = clusters_ne_drt_map, filename = paste0(plots_path, "clusters_ne_drt_zone.png"), width = 12, dpi = 1080, asp = 0)



# ----- same map but both cluster + drt zones together

map_clusters_with_drt_zones = function(
    cluster_sf,
    drt_zone_sf,
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
             legend.lwd.show = FALSE,
             # title.lwd = paste0("Buses / Hour \n(All routes with >= ", round(3600/headway_threshold_bus), " buses / hour)")
             n = 3) +
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
    tm_shape(drt_zone_sf) +
    tm_fill(col = "darkgreen",
            alpha = 0.2) +
    tm_facets(by = "time",
              free.coords = FALSE) +

    # --- Manually Add Legends
    tm_add_legend(type = "line",
                  title = "All Bus Routes",
                  # is.portrait = FALSE,
                  col = "grey55",
                  lwd = 2,
                  alpha = 0.5) +
    tm_add_legend(type = "line",
                  title = paste0("Routes with >= ", round(3600/headway_threshold_bus), "\nbuses / hour)"),
                  # is.portrait = FALSE,
                  col = "red",
                  lwd = 2,
                  alpha = 0.5) +
    tm_add_legend(type = "line",
                  title = paste0("Entire Cluster"),
                  # is.portrait = FALSE,
                  col = "darkgreen",
                  lwd = 3) +
    tm_add_legend(type = "fill",
                  title = paste0("DRT Zone"),
                  # is.portrait = FALSE,
                  col = "darkgreen",
                  alpha = 0.2) +

    # --- Layout & Title
    tm_layout(fontfamily = 'Georgia',
              main.title = geographic_area,
              main.title.size = 1.1,
              main.title.color = "azure4",
              main.title.position = "left",
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              # legend.stack = "horizontal",
              frame = FALSE) -> cluster_map

  return(cluster_map)
}

clusters_nw_map = map_clusters_with_drt_zones(
  clusters_nw_poly,
  clusters_nw_drt,
  headway_threshold_bus = 1200,
  geographic_area = "North West (Entire Cluster + DRT Zone)")

clusters_nw_map
tmap_save(tm = clusters_nw_map, filename = paste0(plots_path, "clusters_nw_poly_and_drt_zone.png"), width = 12, dpi = 1080, asp = 0)



clusters_ne_map = map_clusters_with_drt_zones(
  clusters_ne_poly,
  clusters_ne_drt,
  headway_threshold_bus = 1200,
  geographic_area = "North East (Entire Cluster + DRT Zone)")

clusters_ne_map
tmap_save(tm = clusters_ne_map, filename = paste0(plots_path, "clusters_ne_poly_and_drt_zone.png"), width = 12, dpi = 1080, asp = 0)




# ------------------ Saving the shapefiles

save_clusters_as_shapefiles <- function(cluster_sf, output_dir, crs = NULL) {
  #' Save each row of an sf object as a separate Shapefile with optional reprojection
  #'
  #' @param cluster_sf An sf object with a "time" column
  #' @param output_dir Directory where Shapefiles will be saved
  #' @param crs Optional EPSG code or CRS string to reproject the geometries
  #'
  #' @return Saves separate Shapefiles, overwriting old ones if they exist

  # Create the directory if it does not exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Reproject if CRS is provided
  if (!is.null(crs)) {
    cluster_sf <- st_transform(cluster_sf, crs)
  }

  # Loop over each row in the sf object and save it as a Shapefile
  for (i in 1:nrow(cluster_sf)) {
    time_label <- gsub("[: -]", "_", cluster_sf$time[i])  # Replace unsafe filename characters
    filename <- file.path(output_dir, paste0("cluster_", time_label, ".shp"))

    st_write(cluster_sf[i, ], filename, driver = "ESRI Shapefile", delete_layer = TRUE)
  }
}


save_clusters_as_shapefiles(clusters_ne_drt, "shapefiles/north_east/", crs = 3857)
save_clusters_as_shapefiles(clusters_nw_drt, "shapefiles/north_west/", crs = 3857)







# ----------------------------------- Focus on intermodality

# ---------------- Read in raw gtfs feeds in order to get bus frequency per stop
gtfs_bus_stops = tidytransit::read_gtfs("../../../drt-potential/data/interim/gtfs_freq/study_area_gtfs_bus_f.zip")

gtfs_bus_stops = tidytransit::read_gtfs("../../../drt-potential/data/interim/study_area_gtfs_bus.zip")


# Ensure direction_id exists (tidytransit::get_Stop_frequency) does not work without it
if (!"direction_id" %in% names(gtfs_bus_stops$trips)) {
  gtfs_bus_stops$trips$direction_id <- 1  # Create column with default value 1
}

times = c("05:00:00", "08:00:00", "11:00:00", "14:00:00", "17:00:00", "20:00:00")


# ----------------------- Compute stop frequencies
#' Compute Stop Frequencies for Multiple Time Ranges
#'
#' This function calculates stop-level frequency statistics for a GTFS dataset over multiple time periods.
#' The results are returned as a list of `sf` objects with spatial information. One sf for each time period
#' The function is a wrapper around tidytransit::get_stop_frequency
#'
#' @param gtfs_obj A GTFS object containing stop and trip information.
#' @param times A vector of time strings (e.g., `"05:00:00"`, `"08:00:00"`, etc.). The function loops over consecutive pairs to define time intervals.
#' @param stops_crs The coordinate reference system (CRS) for the stop locations (e.g., `4326` for WGS 84).
#'
#' @return A list of `sf` objects, each representing stop frequencies for a specific time range.
#'
#' @export
get_stop_frequency_by_time <- function(gtfs_obj, times, stops_crs) {
  if (length(times) < 2) stop("times must have at least two values")

  # Convert stops to sf object with specified CRS
  stops_sf <- tidytransit::stops_as_sf(gtfs_obj$stops, crs = stops_crs) %>%
    dplyr::select(stop_id)

  results <- vector("list", length(times) - 1)

  for (i in seq_len(length(times) - 1)) {
    start_time <- times[i]
    end_time <- times[i + 1]

    # just for row labeling (to be consistent with polygon layers above)
    start_time_hh_mm <- str_sub(start_time, 1, 5)
    end_time_hh_mm <- str_sub(end_time, 1, 5)


    message(paste("Processing time range:", start_time, "-", end_time))

    result <- tidytransit::get_stop_frequency(
      gtfs_obj,
      start_time = start_time,
      end_time = end_time,
      service_ids = NULL,
      by_route = FALSE
    )

    # Add time range column
    result$time_range <- paste(start_time_hh_mm, "-", end_time_hh_mm)

    # Merge with stop spatial data
    result <- dplyr::left_join(result, stops_sf, by = "stop_id")
    result <- sf::st_as_sf(result)

    results[[i]] <- result
  }

  names(results) <- paste0("Range_", seq_along(results))  # Name list items

  return(results)
}


# ------------ Apply the function
times <- c("05:00:00", "08:00:00", "11:00:00", "14:00:00", "17:00:00", "20:00:00")

stop_frequency_results <- get_stop_frequency_by_time(
  gtfs_bus_stops,
  times = times,
  stops_crs = 3857
)

# ----- Convert result list to one sf
stop_frequency_results_sf = bind_rows(stop_frequency_results)



# ----------------------- Identify which stops are inside each polygon (considering temporal aspect also)


# This function compares the polygon at each time step to its corresponding stop layer.

get_stops_in_polygons <- function(stops_sf, polygons_sf) {
  # Ensure CRS matches
  stops_sf <- st_transform(stops_sf, st_crs(polygons_sf))


  # Create an empty list to store results
  results_list <- list()

  # Loop through each polygon
  for (i in seq_len(nrow(polygons_sf))) {
    polygon <- polygons_sf[i, ]  # Extract polygon
    polygon_time <- polygon$time  # Time range for this polygon

    # Filter stops that match the time range and fall within the polygon
    stops_in_polygon <- stops_sf %>%
      filter(time_range == polygon_time) %>%
      st_filter(polygon, .predicate = st_within)

    # If there are matching stops, store them
    if (nrow(stops_in_polygon) > 0) {
      stops_in_polygon$polygon_id <- i  # Label stops with polygon index
      results_list[[i]] <- stops_in_polygon
    }
  }

  # Combine results into a single sf object
  intersected_stops_sf <- do.call(rbind, results_list)

  return(intersected_stops_sf)
}

# --- Apply the function

# Entire polygons
clusters_nw_poly_stops <- get_stops_in_polygons(stop_frequency_results_sf, clusters_nw_poly)
clusters_ne_poly_stops <- get_stops_in_polygons(stop_frequency_results_sf, clusters_ne_poly)


# DRT Zones
clusters_nw_drt_stops <- get_stops_in_polygons(stop_frequency_results_sf, clusters_nw_drt)
clusters_ne_drt_stops <- get_stops_in_polygons(stop_frequency_results_sf, clusters_ne_drt)




# ---------- Save output as list (for use in matsim drt extension)

#' Save Filtered Stop IDs as Text Files
#'
#' This function filters a stops dataset based on a minimum departure frequency and saves the stop IDs as text files.
#' It can either create separate files for each `time_range` or a single file for the entire dataset.
#'
#' @param stops_layer A spatial dataframe (sf object) containing stop locations with columns `stop_id`, `n_departures`, and `time_range`.
#' @param min_frequency An integer specifying the minimum number of departures required to include a stop in the output.
#' @param output_name A string used as the base name for output files.
#' @param per_time_range A logical (`TRUE` or `FALSE`). If `TRUE` (default), creates separate files for each `time_range`.
#' If `FALSE`, applies the filter to the whole dataset and saves a single file.
#'
#' @return No return value, but writes one or more text files to the `stops/` directory.
#'
#' @examples
#' # Save separate files for each time range
#' save_high_freq_stops_as_list(clusters_nw_drt_stops, min_frequency = 3, output_name = "clusters_nw_drt_stops", per_time_range = TRUE)
#'
#' # Save a single file for all stops
#' save_high_freq_stops_as_list(clusters_nw_drt_stops, min_frequency = 3, output_name = "clusters_nw_drt_stops", per_time_range = FALSE)
#'
save_high_freq_stops_as_list = function(stops_layer, min_frequency, output_name, per_time_range = TRUE) {

  # Ensure output directory exists
  dir.create("stops", showWarnings = FALSE)

  if (per_time_range) {
    # Get unique time ranges
    time_ranges = unique(stops_layer$time_range)

    # Iterate over time ranges
    for (time in time_ranges) {
      stops_layer_filtered = stops_layer %>%
        filter(time_range == time, n_departures >= min_frequency)

      if (nrow(stops_layer_filtered) > 0) {
        stop_ids = unique(stops_layer_filtered$stop_id)

        # Clean time range for filename (replace spaces and colons)
        safe_time = gsub("[: ]", "_", time)
        output_file = paste0("stops/", output_name, "_", safe_time, "_min_", min_frequency, "_departures.txt")

        writeLines(stop_ids, con = output_file)
      }
    }
  } else {
    # Apply filter to the whole dataset
    stops_layer_filtered = stops_layer %>%
      filter(n_departures >= min_frequency)

    if (nrow(stops_layer_filtered) > 0) {
      stop_ids = unique(stops_layer_filtered$stop_id)
      output_file = paste0("stops/", output_name, "_min_", min_frequency, "_departures.txt")

      writeLines(stop_ids, con = output_file)
    }
  }
}


# --- North West

# drt zone
save_high_freq_stops_as_list(clusters_nw_drt_stops, min_frequency = 3, output_name = "clusters_nw_drt_stops", per_time_range = TRUE)
save_high_freq_stops_as_list(clusters_nw_drt_stops, min_frequency = 3, output_name = "clusters_nw_drt_stops", per_time_range = FALSE)

# entire polygon
save_high_freq_stops_as_list(clusters_nw_poly_stops, min_frequency = 3, output_name = "clusters_nw_poly_stops", per_time_range = TRUE)
save_high_freq_stops_as_list(clusters_nw_poly_stops, min_frequency = 3, output_name = "clusters_nw_poly_stops", per_time_range = FALSE)

# --- North East

# drt zone
save_high_freq_stops_as_list(clusters_ne_drt_stops, min_frequency = 3, output_name = "clusters_ne_drt_stops", per_time_range = TRUE)
save_high_freq_stops_as_list(clusters_ne_drt_stops, min_frequency = 3, output_name = "clusters_ne_drt_stops", per_time_range = FALSE)
# entire polygon
save_high_freq_stops_as_list(clusters_ne_poly_stops, min_frequency = 3, output_name = "clusters_ne_poly_stops", per_time_range = TRUE)
save_high_freq_stops_as_list(clusters_ne_poly_stops, min_frequency = 3, output_name = "clusters_ne_poly_stops", per_time_range = FALSE)



# ----------------- Plot the clusters with stops overlayed


map_clusters_with_stops = function(
    cluster_sf,
    stops_sf,
    headway_threshold_bus = 1800,
    departures_threshold_stop = 3,
    geographic_area) {

  tm_shape(study_area) +
    tm_borders() +
    tm_shape(study_area) +
    tm_fill(col = "grey75",
            alpha = 0.5) +
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
             legend.lwd.show = FALSE,
             scale = 2,
             title.lwd = paste0("Buses / Hour \n(All routes with \n>= ", round(3600/headway_threshold_bus) ,"buses / hr)")) +
    tm_facets(by = "time",
              free.coords = FALSE) +
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
   tm_shape(stops_sf %>%
               filter(n_departures >= departures_threshold_stop)) +
    tm_dots(col = "n_departures",
            size = "n_departures",
            legend.size.show = FALSE,
            # title = "Number of bus \ndepartures in \ntime window",
            # title = "Number of bus departures in time window",
            title = "Number of bus departures \nin time window",
            palette = "Blues",
            legend.is.portrait = TRUE) +
    # tm_dots(col = "darkblue",
    #         size = "n_departures",
    #         legend.size.show = TRUE,
    #         title = "Number of bus departures in time window",
    #         legend.is.portrait = FALSE) +
    tm_facets(by = "time_range",
              free.coords = FALSE) +
    tm_layout(fontfamily = 'Georgia',
              main.title = geographic_area,
              main.title.size = 1.1,
              main.title.color = "azure4",
              main.title.position = "left",
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              legend.title.size = 0.85,
              legend.stack = "horizontal",
              bg.color = "#faf9f6",
              frame = FALSE) -> plot

  return(plot)

}


# --- Plot

# ----- North West
clusters_nw_drt_stops_map = map_clusters_with_stops(
  cluster_sf = clusters_nw_drt,
  stops_sf = clusters_nw_drt_stops,
  headway_threshold_bus = 1800,
  departures_threshold_stop = 9,
  geographic_area = "North West (DRT Zones)")

clusters_nw_drt_stops_map
tmap_save(tm = clusters_nw_drt_stops_map, filename = paste0(plots_path, "clusters_nw_drt_high_freq_stops.png"), width = 12, dpi = 1080, asp = 0)


clusters_nw_poly_stops_map = map_clusters_with_stops(
  cluster_sf = clusters_nw_poly,
  stops_sf = clusters_nw_poly_stops,
  headway_threshold_bus = 1800,
  departures_threshold_stop = 15,
  geographic_area = "North West (Entire Cluster)")

clusters_nw_poly_stops_map
tmap_save(tm = clusters_nw_poly_stops_map, filename = paste0(plots_path, "clusters_nw_poly_high_freq_stops.png"), width = 12, dpi = 1080, asp = 0)



# ----- North East
clusters_ne_drt_stops_map = map_clusters_with_stops(
  cluster_sf = clusters_ne_drt,
  stops_sf = clusters_ne_drt_stops,
  headway_threshold_bus = 1800,
  departures_threshold_stop = 6,
  geographic_area = "North East (DRT Zones)")

clusters_ne_drt_stops_map
tmap_save(tm = clusters_ne_drt_stops_map, filename = paste0(plots_path, "clusters_ne_drt_high_freq_stops.png"), width = 12, dpi = 1080, asp = 0)


clusters_ne_poly_stops_map = map_clusters_with_stops(
  cluster_sf = clusters_ne_poly,
  stops_sf = clusters_ne_poly_stops,
  headway_threshold_bus = 1800,
  departures_threshold_stop = 6,
  geographic_area = "North East (Entire Cluster)")

clusters_ne_poly_stops_map
tmap_save(tm = clusters_ne_poly_stops_map, filename = paste0(plots_path, "clusters_ne_poly_high_freq_stops.png"), width = 12, dpi = 1080, asp = 0)







# ----------------------------------- Fleet size analysis

# Fleet size is a function of demand. We use the critical demand density identified
# in "Feeder transit services: Choosing between fixed and demand responsive policy"
# (10 - 50) customers / square mile


# Read in the demand data from AcBM
acbm_demand = arrow::read_parquet("../data/demand/legs_with_locations.parquet")

# clean it (remove NAs)
acbm_demand <- acbm_demand %>%
  filter(!is.na(start_location_geometry_wkt), grepl("^POINT|^LINESTRING|^POLYGON|^MULTI", start_location_geometry_wkt))

# convert back to sf
acbm_demand = acbm_demand %>%
  # mutate(geometry = st_as_sfc(start_location_geometry_wkt, crs = 3857)) %>%  # Set appropriate CRS
  #  select(-start_location_geometry_wkt) %>%  # Remove original WKT columns
  st_as_sf(wkt = "start_location_geometry_wkt", crs = crs_to_use, remove = TRUE)

# select useful columns only
acbm_demand = acbm_demand %>%
  select(pid, hid, tst, tet, tst_hour, tet_hour)



#  ---------- Create int columns for trip start and end times

# Get the hour values of the start and end times
acbm_demand = acbm_demand %>%
  mutate(
    tst_hour =  lubridate::hour(lubridate::ymd_hms(tst)),
    tet_hour =  lubridate::hour(lubridate::ymd_hms(tet))
  )



clusters_test = clusters_ne_drt %>%
  mutate(hour_start = as.integer(str_extract(time, "^\\d+")),
         hour_end = as.integer(str_extract(time, "(?<=- )\\d+")))




# Spatial join: Find points inside polygons
joined_data <- st_join(acbm_demand, clusters_test, join = st_within)

joined_data2 <- st_join(acbm_demand, clusters_test, join = st_within) %>%
  filter(tst_hour >= hour_start & tst_hour <= hour_end)  # Temporal filtering


# Count the number of points per polygon
polygon_counts <- joined_data2 %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarise(num_points = n())


clusters_test2 = clusters_test %>%
  left_join(polygon_counts, by = "time")




clusters_test2 %>%
  mutate(area = st_area(.) / 1000000,
         area_km2 = units::drop_units(st_area(.) / 1000000),
         area_mile2 = area_km2 * 0.382,
         demand_density = (num_points / 3) / area_mile2, # divide by 3 as our time range is 3 hours
         fleet_size = round(demand_density / 10),
         fleet_size2 = round(demand_density / 50),
  )






library(sf)
library(dplyr)
library(stringr)
library(lubridate)
library(arrow)


# Convert x_sf times to POSIXct (assuming all on the same reference day)
x_sf <- x_sf %>%
  mutate(
    tst = ymd_hms(paste("1900-01-01", tst)),
    tet = ymd_hms(paste("1900-01-01", tet))
  )

# Extract start and end times from 'time' column in clusters_nw_drt
clusters_nw_drt <- clusters_nw_drt %>%
  mutate(
    time_start = ymd_hms(paste("1900-01-01", str_extract(time, "^[0-9]{2}:[0-9]{2}"))),
    time_end   = ymd_hms(paste("1900-01-01", str_extract(time, "[0-9]{2}:[0-9]{2}$")))
  )

# Perform the spatio-temporal join
spatio_temporal_join <- clusters_nw_drt %>%
  rowwise() %>%
  mutate(
    matched_points = list(
      x_sf %>%
        filter(
          tst >= time_start & tst < time_end,  # Temporal condition
          st_within(start_location_geometry_wkt, geometry)  # Spatial condition
        )
    )
  ) %>%
  unnest(matched_points)  # Unnest to expand the results

# View results
print(spatio_temporal_join)










library(arrow)
x = arrow::read_parquet("../data/demand/legs_with_locations.parquet")

x_clean <- x %>%
  filter(!is.na(start_location_geometry_wkt), grepl("^POINT|^LINESTRING|^POLYGON|^MULTI", start_location_geometry_wkt))


x_sf <- x_clean %>%
 # mutate(geometry = st_as_sfc(start_location_geometry_wkt, crs = 3857)) %>%  # Set appropriate CRS
#  select(-start_location_geometry_wkt) %>%  # Remove original WKT columns
  st_as_sf(wkt = "start_location_geometry_wkt", crs = 3857, remove = TRUE)

# Check result
print(x_sf)


x_sf_joined = x_sf %>%
  st_join(clusters_nw_drt,
          join = st_within)

x_sf_joined_summary = x_sf_joined %>%
  st_drop_geometry() %>%
  group_by(time) %>%
  summarise(n = n()) %>%
  left_join(clusters_nw_drt, by = "time") %>%
  st_as_sf(crs = 3857)





x_sf_joined_summary %>%
  mutate(area = st_area(.) / 1000000,
         area_km2 = units::drop_units(st_area(.) / 1000000),
         area_mile2 = area_km2 * 0.382,
         # 10 customers / mile2 / hour)
         fleet_size = n / area_mile2 / 3)
