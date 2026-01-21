library(tidyverse)
library(glue)

# Configuration
ITERATIONS <- c("iteration_01", "iteration_02")
BASE_PATH <- "R/output"

# Output Directory
PLOT_DIR <- "R/plots/transit_opt_paper/comparisons"
dir.create(
  file.path(PLOT_DIR, "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

# Objective Filtering
OBJECTIVES_TO_INCLUDE <- NULL
OBJECTIVES_TO_EXCLUDE <- "^sc_|_var$|_sum_|_peak_"

# Helper to load and tag data
load_iter_data <- function(iter_name, file_name) {
  path <- file.path(BASE_PATH, iter_name, file_name)
  if (!file.exists(path)) {
    return(NULL)
  }
  read_csv(path, show_col_types = FALSE) |>
    mutate(iteration = iter_name)
}

# 1. Load Base Objectives (Needed for PSO Fractions)
obj_base_path <- "../../transit_opt/output/base_objective_values.csv"
if (file.exists(obj_base_path)) {
  obj_base <- read_csv(obj_base_path, show_col_types = FALSE) |>
    rename(baseline_objective_value_pen = penalized_objective_value)
} else {
  warning(glue::glue(
    "Base objective values not found at {obj_base_path}. PSO fractions will be NA."
  ))
  obj_base <- tibble(
    config_name = character(),
    baseline_objective_value_pen = numeric()
  )
}

# 2. Load Raw Data
message("Loading raw data...")
res_mode_share_raw <- map_dfr(
  ITERATIONS,
  ~ load_iter_data(.x, "mode_share_by_objective.csv")
)
drt_fleet_raw <- map_dfr(
  ITERATIONS,
  ~ load_iter_data(.x, "drt_fleet_deployments.csv")
)
obj_pso_raw <- map_dfr(
  ITERATIONS,
  ~ load_iter_data(.x, "pso_objective_values.csv")
)

# Adjust DRT fleet sizes (set zeros to 25)
drt_fleet <- drt_fleet_raw |>
  mutate(fleet_size = ifelse(fleet_size == 0, 25, fleet_size))

# Filter PSO solutions (sanity check)
obj_pso_raw <- obj_pso_raw |> filter(rank <= 511)

# 3. Process PSO Data (Join with Base)
obj_pso_joined <- obj_pso_raw |>
  left_join(
    obj_base |> select(config_name, baseline_objective_value_pen),
    by = c("objective_name" = "config_name")
  ) |>
  rename(objective_sol = objective) |>
  mutate(
    pso_frac_of_base = objective_sol / baseline_objective_value_pen
  )

# 4. Join PSO Data to Mode Share Results
res_mode_share <- res_mode_share_raw |>
  left_join(
    obj_pso_joined,
    by = c(
      "objective" = "objective_name",
      "solution" = "solution_id",
      "iteration"
    )
  )

# 5. Create Combined Mode (PT+DRT) and Calculate Share Deltas
message("Creating combined PT+DRT mode...")

pt_drt_combined <- res_mode_share |>
  filter(mode == "pt" | str_detect(mode, "^drt")) |>
  group_by(objective, solution, solution_id, level, access, zones, iteration) |>
  summarise(
    n_solution = sum(n_solution, na.rm = TRUE),
    share_solution = sum(share_solution, na.rm = TRUE),
    n_base = sum(n_base, na.rm = TRUE),
    share_base = sum(share_base, na.rm = TRUE),
    pso_frac_of_base = first(pso_frac_of_base),
    .groups = "drop"
  ) |>
  mutate(mode = "pt+drt")

# Combine back and calculate share_delta
res_mode_share <- bind_rows(res_mode_share, pt_drt_combined) |>
  mutate(share_delta = share_solution - share_base)

message("Data prep complete.")

# 6. Apply Objective Filters (After data is fully prepared)
if (!is.null(OBJECTIVES_TO_INCLUDE)) {
  message(glue::glue(
    "Filtering to include only: {paste(OBJECTIVES_TO_INCLUDE, collapse = ', ')}"
  ))
  res_mode_share <- res_mode_share |>
    filter(objective %in% OBJECTIVES_TO_INCLUDE)
  drt_fleet <- drt_fleet |> filter(objective %in% OBJECTIVES_TO_INCLUDE)
} else if (!is.null(OBJECTIVES_TO_EXCLUDE)) {
  message(glue::glue("Filtering to exclude: {OBJECTIVES_TO_EXCLUDE}"))
  res_mode_share <- res_mode_share |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
  drt_fleet <- drt_fleet |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
}

# ------------------------------------------------------------------
# ANALYSIS PREP
# ------------------------------------------------------------------

# PREP DRT FLEET STRINGS (Rank 0 only)
# Format: "NE: [25, 25...] | NW: [50, 50...]"
rank0_fleet_detailed <- drt_fleet |>
  filter(str_detect(solution, "_00$")) |>
  arrange(objective, iteration, scenario, interval_label) |>
  group_by(objective, iteration, scenario) |>
  summarise(
    fleet_seq = paste(fleet_size, collapse = ", "),
    total_fleet_zone = sum(fleet_size),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = scenario,
    values_from = c(fleet_seq, total_fleet_zone)
  ) |>
  mutate(
    drt_fleet_str = glue::glue(
      "NE:[{fleet_seq_drtNE}] | NW:[{fleet_seq_drtNW}]"
    ),
    total_system_fleet = total_fleet_zone_drtNE + total_fleet_zone_drtNW
  ) |>
  select(objective, iteration, drt_fleet_str, total_system_fleet)


# ------------------------------------------------------------------
# ANALYSIS 1: ORIGINAL VIEW (Trip | O+D | PT+DRT)
# ------------------------------------------------------------------

# Filter to main subset
res_best_view <- res_mode_share |>
  filter(
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt",
    mode == "pt+drt"
  )

# Table 4.2c (Original - Simple Fleet Sum)
rank0_summary <- res_best_view |>
  filter(solution_id == 0) |>
  select(objective, iteration, share_delta, pso_frac_of_base)

table_4_2c <- rank0_summary |>
  left_join(
    rank0_fleet_detailed |>
      select(objective, iteration, total_fleet = total_system_fleet),
    by = c("objective", "iteration")
  ) |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, pso_frac_of_base, total_fleet),
    names_glue = "{iteration}_{.value}"
  ) |>
  mutate(
    share_improvement = iteration_02_share_delta - iteration_01_share_delta,
    fleet_change = iteration_02_total_fleet - iteration_01_total_fleet
  ) |>
  select(
    objective,
    it1_share = iteration_01_share_delta,
    it1_fleet = iteration_01_total_fleet,
    it2_share = iteration_02_share_delta,
    it2_fleet = iteration_02_total_fleet,
    share_improv_pp = share_improvement
  ) |>
  arrange(desc(share_improv_pp))

write_csv(
  table_4_2c,
  file.path(PLOT_DIR, "tables/table_4_2c_iteration_comparison.csv")
)

# Plot 1: Standard Cloud Shift (Trip | O+D | PT+DRT)
plot_cloud <- res_best_view |>
  filter(!is.na(pso_frac_of_base)) |>
  ggplot(aes(x = pso_frac_of_base, y = share_delta, color = iteration)) +
  geom_point(alpha = 0.5, size = 2) +
  stat_ellipse(level = 0.9, linetype = "dashed", size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.8) +
  facet_wrap(~objective, scales = "free") +
  labs(
    title = "Optimization Convergence: Iteration 1 vs 2",
    subtitle = "Analysis Filter: Trip | O+D | PT+DRT",
    x = "Proxy Objective Cost (Fraction of Base)",
    y = "PT+DRT Mode Share Gain (pp)",
    color = "Iteration"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

ggsave(
  file.path(PLOT_DIR, "fig_iteration_cloud_shift.png"),
  plot_cloud,
  width = 14,
  height = 10
)


# ------------------------------------------------------------------
# ANALYSIS 2: MULTI-CATCHMENT VIEW (New Plot + Extended Table)
# ------------------------------------------------------------------

# Define catchments of interest
target_catchments <- tribble(
  ~level   , ~access              , ~zones   , ~catchment_label        , ~shape_code ,
  "trip"   , "origin+destination" , "pt+drt" , "Trip | O+D | PT+DRT"   ,          16 , # Circle
  "person" , "origin+destination" , "pt+drt" , "Person | O+D | PT+DRT" ,          17 , # Triangle
  "all"    , "all"                , "all"    , "All | All | All"       ,          15 # Square
)

# Filter data for these definitions
res_catchment_view <- res_mode_share |>
  filter(mode == "pt+drt") |>
  inner_join(target_catchments, by = c("level", "access", "zones"))

# Plot 2: Multi-Catchment Cloud
plot_cloud_catchment <- res_catchment_view |>
  filter(!is.na(pso_frac_of_base)) |>
  ggplot(aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = iteration,
    shape = catchment_label
  )) +
  geom_point(alpha = 0.6, size = 2.5) +
  facet_wrap(~objective, scales = "free") +
  labs(
    title = "Optimization Convergence by Catchment Definition",
    subtitle = "Comparing solution distribution across different metric filters",
    x = "Proxy Objective Cost",
    y = "PT+DRT Mode Share Gain (pp)",
    shape = "Catchment Filter"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  scale_shape_manual(values = c(15, 17, 16)) + # Map specific shapes
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2))

ggsave(
  file.path(PLOT_DIR, "fig_iteration_cloud_catchment.png"),
  plot_cloud_catchment,
  width = 14,
  height = 10
)


# Table 4.2c Extended (All Catchments + Fleet Strings)
rank0_catchment <- res_catchment_view |>
  filter(solution_id == 0) |>
  select(objective, iteration, catchment_label, share_delta)

# Join Fleet (Fleet is same per objective/iteration regardless of catchment)
table_data_long <- rank0_catchment |>
  left_join(rank0_fleet_detailed, by = c("objective", "iteration"))

table_4_2c_extended <- table_data_long |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, total_system_fleet, drt_fleet_str),
    names_glue = "{iteration}_{.value}"
  ) |>
  mutate(
    delta_share_pp = iteration_02_share_delta - iteration_01_share_delta,
    catchment = factor(
      catchment_label,
      levels = target_catchments$catchment_label
    )
  ) |>
  select(
    objective,
    catchment,

    it1_val = iteration_01_share_delta,
    it1_fleet_tot = iteration_01_total_system_fleet,
    it1_fleet_str = iteration_01_drt_fleet_str,

    it2_val = iteration_02_share_delta,
    it2_fleet_tot = iteration_02_total_system_fleet,
    it2_fleet_str = iteration_02_drt_fleet_str,

    delta_val = delta_share_pp
  ) |>
  arrange(objective, catchment)

write_csv(
  table_4_2c_extended,
  file.path(PLOT_DIR, "tables/table_4_2c_iteration_comparison_extended.csv")
)

# ------------------------------------------------------------------
# ANALYSIS 3: ARROWS (Rank 0 Movement)
# ------------------------------------------------------------------

# Arrow Plot (Trip | O+D | PT+DRT only - consistent with original)
arrow_data <- rank0_summary |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, pso_frac_of_base)
  )

plot_arrows <- ggplot(arrow_data) +
  geom_segment(
    aes(
      x = pso_frac_of_base_iteration_01,
      y = share_delta_iteration_01,
      xend = pso_frac_of_base_iteration_02,
      yend = share_delta_iteration_02,
      color = objective
    ),
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    size = 1
  ) +
  geom_point(
    aes(x = pso_frac_of_base_iteration_01, y = share_delta_iteration_01),
    shape = 1,
    size = 2
  ) + # Open circle It1
  geom_point(
    aes(x = pso_frac_of_base_iteration_02, y = share_delta_iteration_02),
    shape = 16,
    size = 2
  ) + # Filled circle It2
  labs(
    title = "Improvement of 'Best Found' Solution (Rank 0)",
    subtitle = "Arrow Direction: It1 (Open) -> It2 (Filled)",
    x = "Proxy Objective Cost",
    y = "Real Mode Share Gain"
  ) +
  theme_bw() +
  ggrepel::geom_text_repel(
    aes(
      x = pso_frac_of_base_iteration_02,
      y = share_delta_iteration_02,
      label = objective
    ),
    size = 3
  ) +
  theme(legend.position = "none")

ggsave(
  file.path(PLOT_DIR, "fig_iteration_best_arrows.png"),
  plot_arrows,
  width = 10,
  height = 8
)

message("Analysis Complete.")
