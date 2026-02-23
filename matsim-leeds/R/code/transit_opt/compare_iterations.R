library(tidyverse)
library(glue)

# Configuration
ITERATIONS <- c("iteration_01", "iteration_02", "iteration_03")
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
obj_pso_raw <- obj_pso_raw |> filter(rank <= 1023)

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
# res_best_view <- res_mode_share |>
#   filter(
#     level == "person",
#     access == "origin+destination",
#     zones == "pt+drt",
#     mode == "pt+drt"
#   )

res_best_view <- res_mode_share |>
  filter(
    level == "all",
    access == "all",
    zones == "all",
    mode == "pt+drt"
  )

# Table 4.2c (Dynamic Calculation: Consecutive Deltas)
rank0_summary <- res_best_view |>
  filter(solution_id == 0) |>
  select(objective, iteration, share_delta, pso_frac_of_base)

# Create "Wide" table with raw columns first
table_4_2c <- rank0_summary |>
  left_join(
    rank0_fleet_detailed |>
      select(objective, iteration, total_fleet = total_system_fleet),
    by = c("objective", "iteration")
  ) |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, total_fleet, pso_frac_of_base),
    names_glue = "{iteration}_{.value}"
  )

# Dynamically add consecutive difference columns
for (i in 1:(length(ITERATIONS) - 1)) {
  curr_it <- ITERATIONS[i]
  next_it <- ITERATIONS[i + 1]

  # Parse integers for concise column names (e.g. "delta_1_2")
  id_curr <- as.integer(str_extract(curr_it, "\\d+"))
  id_next <- as.integer(str_extract(next_it, "\\d+"))

  col_name <- glue::glue("calc_delta_{id_curr}_{id_next}")

  # Add column: Next - Current
  table_4_2c <- table_4_2c |>
    mutate(
      !!col_name := .data[[glue("{next_it}_share_delta")]] -
        .data[[glue("{curr_it}_share_delta")]]
    )
}

# Clean up column names and order
# Matches "iteration_01_share_delta" -> "it1_share"
table_4_2c <- table_4_2c |>
  rename_with(
    .fn = function(x) {
      x |>
        str_replace("iteration_0?(\\d+)_share_delta", "it\\1_share") |>
        str_replace("iteration_0?(\\d+)_total_fleet", "it\\1_fleet") |>
        str_replace("iteration_0?(\\d+)_pso_frac_of_base", "it\\1_pso")
    },
    .cols = matches("iteration")
  ) |>
  select(
    objective,
    matches("^it\\d+_share$"), # Share columns
    matches("^it\\d+_fleet$"), # Fleet columns
    matches("^it\\d+_pso$"), # PSO fraction columns
    starts_with("calc_delta_") # Compare columns
  ) |>
  # Sort by the final delta (latest improvement)
  arrange(desc(across(last_col())))

write_csv(
  table_4_2c,
  file.path(PLOT_DIR, "tables/table_comparison_rank0_summary.csv")
)


# Plot 1: Standard Cloud Shift (Naturally handles N iterations)
plot_cloud <- res_best_view |>
  filter(!is.na(pso_frac_of_base)) |>
  filter(pso_frac_of_base < 1.5) |> # Limit to reasonable range for visibility
  ggplot(aes(x = pso_frac_of_base, y = share_delta, color = iteration)) +
  geom_point(alpha = 0.5, size = 2) +
  facet_wrap(~objective, scales = "free_x") +
  #scale_x_continuous(limits = c(0.5, 1.5)) +
  labs(
    title = "Optimization Convergence",
    subtitle = "Analysis Filter: Trip | O+D | PT+DRT",
    x = "Proxy Objective Cost (Fraction of Base)",
    y = "PT+DRT Mode Share Gain (pp)",
    color = "Iteration"
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Set1")

plot_cloud

ggsave(
  file.path(PLOT_DIR, "fig_iteration_cloud_shift.png"),
  plot_cloud,
  width = 14,
  height = 10
)

# ------------------------------------------------------------------
# ANALYSIS 2: MULTI-CATCHMENT VIEW
# ------------------------------------------------------------------

# Define catchments of interest
target_catchments <- tibble::tribble(
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

plot_cloud_catchment

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

# Extended Table 4.2c (Same dynamic logic as above)
table_data_long <- rank0_catchment |>
  left_join(rank0_fleet_detailed, by = c("objective", "iteration"))

table_4_2c_extended <- table_data_long |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, total_system_fleet, drt_fleet_str),
    names_glue = "{iteration}_{.value}"
  )

# Add consecutive deltas loop
for (i in 1:(length(ITERATIONS) - 1)) {
  curr_it <- ITERATIONS[i]
  next_it <- ITERATIONS[i + 1]
  id_curr <- as.integer(str_extract(curr_it, "\\d+"))
  id_next <- as.integer(str_extract(next_it, "\\d+"))
  col_name <- glue::glue("delta_pp_{id_curr}_{id_next}")

  table_4_2c_extended <- table_4_2c_extended |>
    mutate(
      !!col_name := .data[[glue("{next_it}_share_delta")]] -
        .data[[glue("{curr_it}_share_delta")]]
    )
}

# Select and arrange (Simplified select to keep flexibility)
table_4_2c_extended <- table_4_2c_extended |>
  select(objective, catchment_label, everything()) |>
  arrange(objective, catchment_label)

write_csv(
  table_4_2c_extended,
  file.path(PLOT_DIR, "tables/table_comparison_rank0_catchment.csv")
)


# ------------------------------------------------------------------
# ANALYSIS 2b: BEST MODE SHARE COMPARISON (NEW)
# ------------------------------------------------------------------

# 1. Identify Best Share Solutions per Iteration & Objective
best_share_summary <- res_best_view |>
  group_by(objective, iteration) |>
  slice_max(share_delta, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(objective, iteration, solution_id, share_delta, pso_frac_of_base)

# 2. Get Fleet Strings for THESE specific solutions
# We need to filter drt_fleet again for these specific IDs
best_share_fleet_detailed <- drt_fleet |>
  # Join to keep only the best rows
  inner_join(
    best_share_summary |> select(objective, iteration, solution_id),
    by = c("objective", "iteration")
  ) |>
  # Ensure we match the solution string pattern logic
  filter(str_ends(solution, paste0("_", sprintf("%02d", solution_id)))) |>
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

# 3. Create Summary Table for Best Share
table_best_share <- best_share_summary |>
  left_join(
    best_share_fleet_detailed |>
      select(objective, iteration, total_fleet = total_system_fleet),
    by = c("objective", "iteration")
  ) |>
  pivot_wider(
    names_from = iteration,
    values_from = c(share_delta, total_fleet, pso_frac_of_base, solution_id),
    names_glue = "{iteration}_{.value}"
  )

# Add consecutive deltas (reuse logic)
for (i in 1:(length(ITERATIONS) - 1)) {
  curr_it <- ITERATIONS[i]
  next_it <- ITERATIONS[i + 1]
  id_curr <- as.integer(str_extract(curr_it, "\\d+"))
  id_next <- as.integer(str_extract(next_it, "\\d+"))
  col_name <- glue::glue("calc_delta_{id_curr}_{id_next}")

  table_best_share <- table_best_share |>
    mutate(
      !!col_name := .data[[glue("{next_it}_share_delta")]] -
        .data[[glue("{curr_it}_share_delta")]]
    )
}

# Clean names
table_best_share <- table_best_share |>
  rename_with(
    .fn = function(x) {
      x |>
        str_replace("iteration_0?(\\d+)_share_delta", "it\\1_share") |>
        str_replace("iteration_0?(\\d+)_total_fleet", "it\\1_fleet") |>
        str_replace("iteration_0?(\\d+)_pso_frac_of_base", "it\\1_pso") |>
        str_replace("iteration_0?(\\d+)_solution_id", "it\\1_id")
    },
    .cols = matches("iteration")
  ) |>
  select(
    objective,
    matches("^it\\d+_share$"),
    matches("^it\\d+_fleet$"),
    matches("^it\\d+_id$"),
    matches("^it\\d+_pso$"),
    starts_with("calc_delta_")
  ) |>
  arrange(desc(across(last_col())))

write_csv(
  table_best_share,
  file.path(PLOT_DIR, "tables/table_comparison_best_share_summary.csv")
)


# ------------------------------------------------------------------
# ANALYSIS 3: ARROWS (Rank 0 Trajectory)
# ------------------------------------------------------------------

# geom_path automatically connects points in order 1->2->3->...
arrow_data <- rank0_summary |>
  arrange(objective, iteration)

plot_arrows <- ggplot(
  arrow_data,
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = objective,
    group = objective
  )
) +
  geom_path(
    arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
    size = 1,
    alpha = 0.8
  ) +
  geom_point(aes(shape = iteration), size = 3) +
  labs(
    title = "Optimization Trajectory (Rank 0 Solutions)",
    subtitle = "Path of the best solution across iterations",
    x = "Proxy Objective Cost",
    y = "Real Mode Share Gain"
  ) +
  theme_bw() +
  ggrepel::geom_text_repel(
    data = arrow_data |> group_by(objective) |> slice_tail(n = 1), # Label last point
    aes(label = objective),
    size = 3
  ) +
  theme(legend.position = "bottom")

plot_arrows

ggsave(
  file.path(PLOT_DIR, "fig_iteration_best_arrows.png"),
  plot_arrows,
  width = 10,
  height = 8
)

# ------------------------------------------------------------------
# ANALYSIS 4: BENCHMARK WITH ITERATION 00
# ------------------------------------------------------------------

message("Loading Benchmark (Iteration 00)...")

# 1. Load Iteration 00 Data separately
# (Reusing existing helper functions)
res_mode_share_00 <- load_iter_data(
  "iteration_00",
  "mode_share_by_objective.csv"
)
obj_pso_00 <- load_iter_data("iteration_00", "pso_objective_values.csv")

# Initialize empty container for consistent plotting flow
bench_data <- tibble(
  objective = character(),
  iteration = character(),
  pso_frac_of_base = numeric(),
  share_delta = numeric()
)

# Process only if data exists
if (!is.null(res_mode_share_00) && !is.null(obj_pso_00)) {
  # Join PSO info (Proxy Cost)
  obj_pso_joined_00 <- obj_pso_00 |>
    left_join(
      obj_base |> select(config_name, baseline_objective_value_pen),
      by = c("objective_name" = "config_name")
    ) |>
    mutate(pso_frac_of_base = objective / baseline_objective_value_pen)

  # Join to results
  res_00_joined <- res_mode_share_00 |>
    left_join(
      obj_pso_joined_00,
      by = c(
        "objective" = "objective_name",
        "solution" = "solution_id",
        "iteration"
      )
    )

  # Calculate PT+DRT Combined for It00
  pt_drt_00 <- res_00_joined |>
    filter(mode == "pt" | str_detect(mode, "^drt")) |>
    group_by(
      objective,
      solution,
      solution_id,
      level,
      access,
      zones,
      iteration
    ) |>
    summarise(
      share_solution = sum(share_solution, na.rm = TRUE),
      share_base = sum(share_base, na.rm = TRUE),
      pso_frac_of_base = first(pso_frac_of_base),
      .groups = "drop"
    ) |>
    mutate(
      mode = "pt+drt",
      share_delta = share_solution - share_base
    )

  # Filter for the standard view (Trip | O+D | PT+DRT)
  # bench_data <- pt_drt_00 |>
  #   filter(
  #     level == "person",
  #     access == "origin+destination",
  #     zones == "pt+drt",
  #     !is.na(pso_frac_of_base)
  #   )
  bench_data <- pt_drt_00 |>
    filter(
      level == "all",
      access == "all",
      zones == "all",
      !is.na(pso_frac_of_base)
    )
} else {
  message("⚠️ Iteration 00 data not found. Benchmark layer will be empty.")
}

# 2. Combine with Main Data (res_best_view) for plotting
# We want Iteration 00 to be the "background"
# This works even if bench_data is empty
plot_data_bench <- bind_rows(
  res_best_view |> select(objective, iteration, pso_frac_of_base, share_delta),
  bench_data |> select(objective, iteration, pso_frac_of_base, share_delta)
)

# Calculate max share delta from Iteration 00 for reference lines
bench_ref_lines <- bench_data |>
  group_by(objective) |>
  summarise(max_share00 = max(share_delta, na.rm = TRUE), .groups = "drop")


# 3. Plot
plot_cloud_bench <- ggplot() +
  # Reference Line: Best unseeded result
  geom_hline(
    data = bench_ref_lines,
    aes(yintercept = max_share00),
    linetype = "dashed",
    color = "gray50",
    size = 0.4
  ) +
  # Layer 1: Benchmark (Grey, behind)
  geom_point(
    data = plot_data_bench |>
      filter(iteration == "iteration_00") |>
      # Limit to reasonable range for visibility
      filter(pso_frac_of_base < 1.5),
    aes(x = pso_frac_of_base, y = share_delta, color = "Benchmark (It00)"),
    alpha = 0.55,
    size = 2.0
  ) +
  # Layer 2: Main Experiment (Colored, on top)
  geom_point(
    data = plot_data_bench |>
      filter(iteration != "iteration_00") |>
      filter(pso_frac_of_base < 1.2),
    aes(x = pso_frac_of_base, y = share_delta, color = iteration),
    alpha = 0.75,
    size = 2.5
  ) +
  facet_wrap(~objective, scales = "free", nrow = 1) +
  #scale_x_continuous(limits = c(0.5, 1.5)) +
  scale_color_manual(
    values = c(
      "Benchmark (It00)" = "gray30",
      "iteration_01" = "#7570b3", # Dark2 purple
      "iteration_02" = "#d95f02", # Dark2 orange
      "iteration_03" = "#1b9e77" # Dark2 teal
    ),
    name = "Iteration"
  ) +
  labs(
    title = "Convergence Benchmark: Seeded vs Unseeded",
    subtitle = "Grey: Unseeded Baseline (It00) | Colored: MATSim-Seeded Iterations | Dashed line: Best It00 result",
    x = "Proxy Objective Cost",
    y = "PT+DRT Mode Share Gain (pp)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom", # Moves legend below the plot
    legend.box = "horizontal", # Ensures the items inside are side-by-side
    legend.direction = "horizontal", # Forces the direction of the scales
    # Increase Facet Title (Strip) size
    # 'strip.text' handles all facets; 'strip.text.x' targets just horizontal ones
    strip.text = element_text(size = 12, face = "bold"),
    # Increase Legend Title size
    legend.title = element_text(size = 12, face = "bold"),
    # Increase Legend Item (Label) size
    legend.text = element_text(size = 11),
  )

plot_cloud_bench

# Save
ggsave(
  file.path(PLOT_DIR, "fig_iteration_benchmark_cloud.png"),
  plot_cloud_bench,
  width = 14,
  height = 5
)

# 4. Save Full Benchmark Data Table (Request #3)
table_benchmark_full <- plot_data_bench |>
  # Join back solution metadata for context
  left_join(
    bind_rows(res_best_view, bench_data) |>
      select(
        objective,
        iteration,
        pso_frac_of_base,
        share_delta,
        solution_id
      ),
    by = c("objective", "iteration", "pso_frac_of_base", "share_delta")
  ) |>
  # Reorder columns
  select(
    objective,
    iteration,
    solution_id,
    pso_frac_of_base,
    share_delta
  ) |>
  arrange(objective, iteration, solution_id)

write_csv(
  table_benchmark_full,
  file.path(PLOT_DIR, "tables/table_comparison_full_cloud_data.csv")
)

message("✓ Benchmark plot and tables saved.")
