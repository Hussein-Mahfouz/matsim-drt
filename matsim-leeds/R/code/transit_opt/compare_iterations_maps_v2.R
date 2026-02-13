##################################################################################
# This script provides the same functionality as compare_iterations.maps.R. The  #
# only reason we add it is because tmap is unable to plot a facet grid that has
# changes to both a line and polygon layer. Only the changes of the last layer
# are shown. I therefore use tmap arrange here to bypass this limitation and
# plot changes to both pt (lines) and drt (polygons)
##################################################################################
library(tidyverse)
library(sf)
library(tmap)
library(glue)
library(grid) # Required for grid.newpage, pushViewport, grid.raster, etc.
library(png) # Required for readPNG

# Source helper functions
source("R/code/transit_opt/load_spatial_layers.R")

####################
#  Configuration
####################

ITERATIONS <- c("iteration_01", "iteration_02", "iteration_03")
BASE_PATH <- "R/output"
RANKING_PATH_TEMPLATE <- "R/plots/transit_opt_paper/{iter}/tables/table_4_1_solution_rankings_all.csv"

# Output
PLOT_DIR <- "R/plots/transit_opt_paper/comparisons"
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

# Objectives to plot
OBJECTIVES_OF_INTEREST <- c(
  "wt_int_tot",
  "wt_int_atk",
  "wt_avg_tot",
  "wt_avg_atk"
)

# Visual Thresholds
MIN_TRIP_DIFF <- 15
MAP_INTERVAL <- "8-12"

# Load Spatial Layers
layers <- load_all_spatial_layers()
study_area <- layers$study_area
drt_zones_sf <- layers$drt

####################
#  Helper: Get Target Solution IDs
####################

# Function to get mapping of objective -> solution_pattern for a specific metric
get_target_solutions <- function(metric = "rank0") {
  # We need a dataframe: iteration | objective | solution_pattern | solution_id_int
  # solution_pattern is regex for filtering files/rows (e.g. "_00_gtfs$" or "_15_gtfs$")

  map_dfr(ITERATIONS, function(iter) {
    if (metric == "rank0") {
      # Default: solution 0 represents best PSO rank
      tibble(
        iteration = iter,
        objective = OBJECTIVES_OF_INTEREST,
        solution_id = 0
      )
    } else if (metric == "best_share") {
      # Load ranking table
      ranking_path <- glue::glue(RANKING_PATH_TEMPLATE, iter = iter)
      if (!file.exists(ranking_path)) {
        warning(glue("Ranking file not found for {iter}: {ranking_path}"))
        return(NULL)
      }

      read_csv(ranking_path, show_col_types = FALSE) |>
        filter(
          objective %in% OBJECTIVES_OF_INTEREST,
          rank_pt_drt_share == 1
        ) |>
        select(objective, solution_id) |>
        mutate(iteration = iter)
    }
  }) |>
    mutate(
      # Regex pattern for GTFS comparisons (e.g. _05_gtfs$)
      pattern_gtfs = paste0("_", sprintf("%02d", solution_id), "_gtfs$"),
      # Regex pattern for Fleet CSV (e.g. _05$)
      pattern_fleet = paste0("_", sprintf("%02d", solution_id), "$")
    )
}


####################
#  Main Process Wrapper
####################

run_comparison_maps <- function(metric_type, title_suffix) {
  message(glue::glue("\n--- Generating Maps for: {metric_type} ---\n"))

  # 1. Get Target Solutions Lookup
  target_map <- get_target_solutions(metric_type)

  if (nrow(target_map) == 0) {
    message("Skipping - No solution mapping found.")
    return()
  }

  ####################
  #  Load Data
  ####################

  # 1. Load Iteration Differences (GTFS)
  load_diff_data <- function(iter_id) {
    path <- file.path(
      BASE_PATH,
      iter_id,
      "gtfs_headway_comparisons_overline.rds"
    )
    if (!file.exists(path)) {
      return(NULL)
    }

    # Get patterns for this iteration
    iter_targets <- target_map |> filter(iteration == iter_id)

    readRDS(path) |>
      filter(
        objective %in% OBJECTIVES_OF_INTEREST,
        interval_label == MAP_INTERVAL
      ) |>
      # Inner join to filter only the specific solution for each objective
      inner_join(
        iter_targets |> select(objective, valid_pattern = pattern_gtfs),
        by = "objective"
      ) |>
      filter(str_detect(solution, valid_pattern)) |>
      mutate(
        iteration = iter_id,
        iter_num = str_extract(iteration, "\\d+")
      ) |>
      select(-valid_pattern)
  }

  diff_data <- map_dfr(ITERATIONS, load_diff_data) |> st_as_sf()

  # 2. Load DRT Fleet Info
  load_fleet_data <- function(iter_id) {
    path <- file.path(BASE_PATH, iter_id, "drt_fleet_deployments.csv")
    if (!file.exists(path)) {
      return(NULL)
    }

    iter_targets <- target_map |> filter(iteration == iter_id)

    read_csv(path, show_col_types = FALSE) |>
      filter(
        objective %in% OBJECTIVES_OF_INTEREST,
        interval_label == MAP_INTERVAL
      ) |>
      inner_join(
        iter_targets |> select(objective, valid_pattern = pattern_fleet),
        by = "objective"
      ) |>
      filter(str_detect(solution, valid_pattern)) |>
      mutate(iteration = iter_id) |>
      select(-valid_pattern) |>
      # Fix fleet size: 0 -> 25
      mutate(fleet_size = if_else(fleet_size == 0, 25, fleet_size))
  }

  fleet_data <- map_dfr(ITERATIONS, load_fleet_data)

  fleet_join <- fleet_data |>
    select(objective, iteration, scenario, fleet_size)

  # 3. Load Combined Fleet Sizes (for labels) - using same logic
  load_label_data <- function(iter_id) {
    path <- file.path(BASE_PATH, iter_id, "combined_fleet_sizes.csv")
    if (!file.exists(path)) {
      return(NULL)
    }

    iter_targets <- target_map |> filter(iteration == iter_id)

    read_csv(path, show_col_types = FALSE) |>
      filter(
        objective %in% OBJECTIVES_OF_INTEREST,
        interval_label == MAP_INTERVAL
      ) |>
      inner_join(
        iter_targets |> select(objective, valid_pattern = pattern_fleet),
        by = "objective"
      ) |>
      filter(str_detect(solution, valid_pattern)) |>
      mutate(iteration = iter_id) |>
      select(-valid_pattern) |>
      # Fix DRT fleet size (min 25) and recalculate totals
      mutate(
        drt_fleet_solution = if_else(
          drt_fleet_solution == 0,
          25,
          drt_fleet_solution
        ),
        total_fleet_diff = (bus_fleet_solution + drt_fleet_solution) -
          bus_fleet_base,
        total_fleet_pct_change = round(
          (total_fleet_diff / bus_fleet_base) * 100,
          0
        ) # Note: label text uses bus_fleet_pct_change variable name but logic implies total change in fleets relative to bus base
      )
  }

  fleet_labels_df <- map_dfr(ITERATIONS, load_label_data)

  ####################
  #  Data Preparation
  ####################

  # A. Lines
  plot_diff_sf <- diff_data |>
    filter(abs(num_trips_diff) >= MIN_TRIP_DIFF) |>
    mutate(col_label = paste0("Iteration ", iter_num, " (Diff)"))

  # B. DRT Fleet
  fleet_levels_num <- c(25, 50, 100, 150)
  fleet_pal <- c(
    "#ffffb2",
    "#feb24c",
    "#fc4e2a",
    "#b10026"
  )

  drt_diff_layer <- drt_zones_sf |>
    cross_join(distinct(
      fleet_join,
      objective,
      iteration,
      scenario,
      fleet_size
    )) |>
    filter(scenario.x == scenario.y) |>
    rename(scenario = scenario.x) |>
    select(-scenario.y) |>
    mutate(
      fleet_label = factor(fleet_size, levels = fleet_levels_num),
      iter_num = str_extract(iteration, "\\d+"),
      col_label = paste0("Iteration ", iter_num, " (Diff)")
    )

  # C. Context
  network_context_sf <- diff_data |>
    mutate(col_label = paste0("Iteration ", iter_num, " (Diff)"))

  plot_additions_sf <- network_context_sf |>
    filter(num_trips_diff > 0)

  plot_substractions_sf <- network_context_sf |>
    filter(num_trips_diff < 0) |>
    mutate(abs_diff = abs(num_trips_diff))

  # D. Grid Layout
  grid_layout <- expand_grid(obj = OBJECTIVES_OF_INTEREST, iter = ITERATIONS) |>
    mutate(
      iter_num = str_extract(iter, "\\d+"),
      col_label = paste0("Iteration ", iter_num, " (Diff)")
    )

  # E. Limits
  max_abs_diff <- max(abs(plot_diff_sf$num_trips_diff), na.rm = TRUE)
  diff_limits <- c(-max_abs_diff, max_abs_diff)
  max_additions <- max(plot_additions_sf$num_trips_diff, na.rm = TRUE)
  max_substractions <- max(plot_substractions_sf$abs_diff, na.rm = TRUE)

  # F. Fleet Labels
  fleet_label_lookup <- fleet_labels_df |>
    mutate(
      fleet_text = paste0(
        "Bus: ",
        sprintf("%+d", bus_fleet_diff),
        " (",
        sprintf("%+.0f", bus_fleet_pct_change),
        "%)",
        " | DRT: ",
        drt_fleet_solution
      )
    ) |>
    select(objective, iteration, fleet_text)

  ####################
  #  Plotting Functions (Local scope captures prepped data)
  ####################

  build_diff_panel <- function(target_obj, target_iter, show_legend = FALSE) {
    lines_sub <- plot_diff_sf |>
      filter(objective == target_obj, iteration == target_iter)
    drt_sub <- drt_diff_layer |>
      filter(objective == target_obj, iteration == target_iter)

    iter_num <- str_extract(target_iter, "\\d+")
    fleet_row <- fleet_label_lookup |>
      filter(objective == target_obj, iteration == target_iter)
    fleet_suffix <- if (nrow(fleet_row) > 0) {
      paste0("\n", fleet_row$fleet_text)
    } else {
      ""
    }

    panel_title <- paste0(target_obj, " | It", iter_num, fleet_suffix)

    # Use diff_limits from local scope
    tm_shape(study_area) +
      tm_polygons(fill = "gray20", col_alpha = 0) +
      tm_shape(lines_sub) +
      tm_lines(
        col = "num_trips_diff",
        lwd = "num_trips",
        lwd.scale = tm_scale_continuous(values.scale = 4),
        col.scale = tm_scale_continuous(
          values = "brewer.rd_yl_bu",
          midpoint = 0,
          limits = diff_limits
        ),
        col.legend = tm_legend(title = "Trip Change\n(vs Base)"),
        lwd.legend = tm_legend_hide()
      ) +
      tm_shape(drt_sub) +
      tm_polygons(
        fill = "fleet_label",
        col = "white",
        lwd = 1,
        fill.scale = tm_scale_categorical(values = fleet_pal),
        fill.legend = tm_legend(title = "DRT Fleet Size"),
        fill_alpha = 0.4
      ) +
      tm_title(panel_title) +
      tm_layout(
        legend.show = show_legend,
        legend.outside = show_legend,
        legend.outside.position = "left",
        frame = FALSE,
        title.size = 0.8,
        title.fontface = "bold",
        title.position = c("left", "top"),
        inner.margins = c(0.01, -0.1, 0.12, 0.01)
      )
  }

  build_additions_panel <- function(
    target_obj,
    target_iter,
    show_legend = FALSE
  ) {
    context_sub <- network_context_sf |>
      filter(objective == target_obj, iteration == target_iter)
    adds_sub <- plot_additions_sf |>
      filter(objective == target_obj, iteration == target_iter)
    drt_sub <- drt_diff_layer |>
      filter(objective == target_obj, iteration == target_iter)

    iter_num <- str_extract(target_iter, "\\d+")
    fleet_row <- fleet_label_lookup |>
      filter(objective == target_obj, iteration == target_iter)
    fleet_suffix <- if (nrow(fleet_row) > 0) {
      paste0("\n", fleet_row$fleet_text)
    } else {
      ""
    }

    panel_title <- paste0(target_obj, " | It", iter_num, fleet_suffix)

    tm_shape(study_area) +
      tm_polygons(fill = "gray20", col_alpha = 0) +
      tm_shape(context_sub) +
      tm_lines(col = "white", lwd = 0.3, col_alpha = 0.15) +
      tm_shape(adds_sub) +
      tm_lines(
        col = "num_trips_diff",
        lwd = "num_trips",
        lwd.scale = tm_scale_continuous(values.scale = 4),
        col.scale = tm_scale_continuous(
          values = "brewer.yl_gn_bu",
          limits = c(0, max_additions)
        ),
        col.legend = tm_legend(title = "Service Added\n(Trips vs Base)"),
        lwd.legend = tm_legend_hide()
      ) +
      tm_shape(drt_sub) +
      tm_polygons(
        fill = "fleet_label",
        col = "white",
        lwd = 1,
        fill.scale = tm_scale_categorical(values = fleet_pal),
        fill.legend = tm_legend(title = "DRT Fleet Size"),
        fill_alpha = 0.3
      ) +
      tm_layout(
        title = panel_title,
        title.position = c("left", "top"),
        title.size = 1.0,
        title.fontface = "bold",
        legend.show = show_legend,
        legend.outside = show_legend,
        legend.outside.position = "left",
        frame = FALSE,
        inner.margins = c(0.01, -0.1, 0.12, 0.01)
      )
  }

  build_substractions_panel <- function(
    target_obj,
    target_iter,
    show_legend = FALSE
  ) {
    context_sub <- network_context_sf |>
      filter(objective == target_obj, iteration == target_iter)
    subs_sub <- plot_substractions_sf |>
      filter(objective == target_obj, iteration == target_iter)
    drt_sub <- drt_diff_layer |>
      filter(objective == target_obj, iteration == target_iter)

    iter_num <- str_extract(target_iter, "\\d+")
    fleet_row <- fleet_label_lookup |>
      filter(objective == target_obj, iteration == target_iter)
    fleet_suffix <- if (nrow(fleet_row) > 0) {
      paste0("\n", fleet_row$fleet_text)
    } else {
      ""
    }

    panel_title <- paste0(target_obj, " | It", iter_num, fleet_suffix)

    tm_shape(study_area) +
      tm_polygons(fill = "gray20", col_alpha = 0) +
      tm_shape(context_sub) +
      tm_lines(col = "white", lwd = 0.3, col_alpha = 0.15) +
      tm_shape(subs_sub) +
      tm_lines(
        col = "abs_diff",
        lwd = "num_trips",
        lwd.scale = tm_scale_continuous(values.scale = 4),
        col.scale = tm_scale_continuous(
          values = "brewer.or_rd",
          limits = c(0, max_substractions)
        ),
        col.legend = tm_legend(title = "Service Removed\n(Trips vs Base)"),
        lwd.legend = tm_legend_hide()
      ) +
      tm_shape(drt_sub) +
      tm_polygons(
        fill = "fleet_label",
        col = "white",
        lwd = 1,
        fill.scale = tm_scale_categorical(values = fleet_pal),
        fill.legend = tm_legend(title = "DRT Fleet Size"),
        fill_alpha = 0.3
      ) +
      tm_layout(
        title = panel_title,
        title.position = c("left", "top"),
        title.size = 0.8,
        title.fontface = "bold",
        legend.show = show_legend,
        legend.outside = show_legend,
        legend.outside.position = "left",
        frame = FALSE,
        inner.margins = c(0.01, -0.1, 0.12, 0.01)
      )
  }

  ####################
  #  Build & Save
  ####################

  # Dimensions (cm)
  n_cols <- length(ITERATIONS)
  panel_width_cm <- n_cols * 10
  legend_width_cm <- 4.5
  total_width_cm <- panel_width_cm + legend_width_cm
  total_height_cm <- 32

  saver <- function(panel_fn, fname, title) {
    panels_no_legend <- pmap(
      grid_layout,
      function(obj, iter, iter_num, col_label) {
        panel_fn(obj, iter, show_legend = FALSE)
      }
    )

    legend_panel <- panel_fn(
      grid_layout$obj[1],
      grid_layout$iter[1],
      show_legend = TRUE
    ) +
      tm_layout(legend.only = TRUE, scale = 2.0)

    tmp_panels <- tempfile(fileext = ".png")
    arranged <- tmap_arrange(
      panels_no_legend,
      ncol = n_cols,
      outer.margins = 0.01
    )
    tmap_save(
      arranged,
      tmp_panels,
      width = panel_width_cm,
      height = total_height_cm - 1,
      units = "cm",
      dpi = 300
    )

    tmp_legend <- tempfile(fileext = ".png")
    tmap_save(
      legend_panel,
      tmp_legend,
      width = legend_width_cm,
      height = total_height_cm - 1,
      units = "cm",
      dpi = 300
    )

    img_panels <- readPNG(tmp_panels)
    img_legend <- readPNG(tmp_legend)

    out_path <- file.path(PLOT_DIR, fname)
    png(
      out_path,
      width = total_width_cm,
      height = total_height_cm,
      units = "cm",
      res = 300
    )
    grid.newpage()
    pushViewport(viewport(x = 0.5, y = 0.97, width = 1, height = 0.06))
    grid.text(title, gp = gpar(fontsize = 13, fontface = "bold"))
    popViewport()
    panel_frac <- panel_width_cm / total_width_cm
    pushViewport(viewport(
      x = panel_frac / 2,
      y = 0.47,
      width = panel_frac,
      height = 0.9
    ))
    grid.raster(img_panels)
    popViewport()
    legend_frac <- legend_width_cm / total_width_cm
    pushViewport(viewport(
      x = panel_frac + legend_frac / 2,
      y = 0.47,
      width = legend_frac,
      height = 0.9
    ))
    grid.raster(img_legend)
    popViewport()
    dev.off()
    unlink(c(tmp_panels, tmp_legend))
    message(glue::glue("✓ {fname} saved."))
  }

  saver(
    build_diff_panel,
    paste0("map_iteration_comparison_diffs_", metric_type, ".png"),
    paste0("Optimization Changes vs Base (", title_suffix, ")")
  )
  saver(
    build_additions_panel,
    paste0("map_iteration_comparison_additions_", metric_type, ".png"),
    paste0("Service Additions vs Base (", title_suffix, ")")
  )
  saver(
    build_substractions_panel,
    paste0("map_iteration_comparison_substractions_", metric_type, ".png"),
    paste0("Service Reductions vs Base (", title_suffix, ")")
  )
}

# --- EXECUTE ---

# 1. Best PSO Rank (Default, Solution 0)
run_comparison_maps(
  metric_type = "rank0",
  title_suffix = "Best PSO Rank"
)

# 2. Best Mode Share (Dynamic Solution ID)
run_comparison_maps(
  metric_type = "best_share",
  title_suffix = "Best PT+DRT Share"
)
