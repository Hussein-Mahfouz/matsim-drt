library(tidyverse)
library(glue)


##########
# SECTION 0a: Configuration - Data Loading
##########

# ===== USER CONFIGURATION =====

ITERATION_ID <- "iteration_01"
message(glue::glue("\nRunning evaluation for: {ITERATION_ID}"))

# Update Paths to read from the iteration folder
input_dir <- file.path("R/output", ITERATION_ID)
plot_dir <- file.path("R/plots/transit_opt_paper", ITERATION_ID)

dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(
  file.path(plot_dir, "tables"),
  showWarnings = FALSE,
  recursive = TRUE
)

message("\n==========================================")
message("LOADING DATA")
message("==========================================\n")

## PSO results
obj_base <- read_csv("../../transit_opt/output/base_objective_values.csv") |>
  rename(baseline_objective_value_pen = penalized_objective_value)

obj_pso <- read_csv(file.path(input_dir, "pso_objective_values.csv"))
res_mode_share <- read_csv(file.path(input_dir, "mode_share_by_objective.csv"))
res_vkm <- read_csv(file.path(input_dir, "vkm_by_objective.csv"))
all_drt_deployments <- read_csv(file.path(
  input_dir,
  "drt_fleet_deployments.csv"
))

# Replace 0 fleet size with 25
all_drt_deployments <- all_drt_deployments |>
  mutate(fleet_size = if_else(fleet_size == 0, 25, fleet_size))

##########
# SECTION 0b: Filters & Analysis Settings
##########

# 1. Objective filtering
OBJECTIVES_TO_INCLUDE <- NULL
OBJECTIVES_TO_EXCLUDE <- "^sc_|_var$|_sum_"

# 2. Catchment Filtering (NEW)
# Regex to exclude specific definitions from PLOTS (Heatmaps + Sensitivity Bars)
# Matches " | O | " which corresponds to "Access: Origin" (Trip | O | PT)
CATCHMENT_PLOT_EXCLUDE_REGEX <- "\\| O \\|"

# 3. Correlation Settings
MAX_SOLUTION_RANK_FOR_CORRELATION <- NULL # NULL = use all

# 4. Top-k Settings
TOPK_HEATMAP_K_CONFIG <- 5
TOPK_LINE_PLOT_MAX_K_CONFIG <- 5

# Apply Objective Filters
if (!is.null(OBJECTIVES_TO_INCLUDE)) {
  message(glue::glue(
    "Filtering to include only: {paste(OBJECTIVES_TO_INCLUDE, collapse = ', ')}"
  ))
  res_mode_share <- res_mode_share |>
    filter(objective %in% OBJECTIVES_TO_INCLUDE)
  res_vkm <- res_vkm |> filter(objective %in% OBJECTIVES_TO_INCLUDE)
  obj_pso <- obj_pso |> filter(objective_name %in% OBJECTIVES_TO_INCLUDE)
  obj_base <- obj_base |> filter(config_name %in% OBJECTIVES_TO_INCLUDE)
  all_drt_deployments <- all_drt_deployments |>
    filter(objective %in% OBJECTIVES_TO_INCLUDE)
} else if (!is.null(OBJECTIVES_TO_EXCLUDE)) {
  message(glue::glue("Filtering to exclude: {OBJECTIVES_TO_EXCLUDE}"))
  res_mode_share <- res_mode_share |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
  res_vkm <- res_vkm |> filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
  obj_pso <- obj_pso |>
    filter(!str_detect(objective_name, OBJECTIVES_TO_EXCLUDE))
  obj_base <- obj_base |>
    filter(!str_detect(config_name, OBJECTIVES_TO_EXCLUDE))
  all_drt_deployments <- all_drt_deployments |>
    filter(!str_detect(objective, OBJECTIVES_TO_EXCLUDE))
}

message(glue::glue(
  "Objectives in analysis: {paste(unique(res_mode_share$objective), collapse = ', ')}"
))


##########
# SECTION 0b: Prepare PSO columns and join to results
##########

message("Preparing PSO objective values...")

obj_pso_joined <- obj_pso |>
  left_join(
    obj_base |> select(config_name, baseline_objective_value_pen),
    by = c("objective_name" = "config_name")
  ) |>
  rename(objective_sol = objective) |>
  mutate(
    pso_pct_change_vs_base = 100 *
      (objective_sol - baseline_objective_value_pen) /
      baseline_objective_value_pen,
    pso_frac_of_base = objective_sol / baseline_objective_value_pen
  )

res_mode_share <- res_mode_share |>
  left_join(
    obj_pso_joined,
    by = c("objective" = "objective_name", "solution" = "solution_id")
  )

res_vkm <- res_vkm |>
  left_join(
    obj_pso_joined,
    by = c("objective" = "objective_name", "solution" = "solution_id")
  )

##########
# SECTION 0c: Create combined modes (PT+DRT, Car+Taxi)
##########

message("Creating combined mode categories...")

pt_drt_combined <- res_mode_share |>
  filter(mode == "pt" | str_detect(mode, "^drt")) |>
  group_by(objective, solution, solution_id, level, access, zones) |>
  summarise(
    n_solution = sum(n_solution, na.rm = TRUE),
    share_solution = sum(share_solution, na.rm = TRUE),
    n_base = sum(n_base, na.rm = TRUE),
    share_base = sum(share_base, na.rm = TRUE),
    n_pct_change = ((n_solution - n_base) / n_base) * 100,
    share_pct_change = ((share_solution - share_base) / share_base) * 100,
    # Keep metadata from first row
    across(
      c(
        rank,
        swarm_id,
        objective_sol,
        generation_found,
        violations,
        baseline_objective_value_pen,
        pso_pct_change_vs_base,
        pso_frac_of_base
      ),
      first
    ),
    .groups = "drop"
  ) |>
  mutate(mode = "pt+drt")

res_mode_share <- bind_rows(res_mode_share, pt_drt_combined) |>
  arrange(objective, solution, solution_id, level, access, zones, mode) |>
  mutate(share_delta = share_solution - share_base)

pt_drt_vkm_combined <- res_vkm |>
  filter(mode == "pt" | str_detect(mode, "^drt")) |>
  group_by(objective, solution, solution_id, level, access, zones) |>
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    across(
      c(
        rank,
        swarm_id,
        objective_sol,
        generation_found,
        violations,
        baseline_objective_value_pen,
        pso_pct_change_vs_base,
        pso_frac_of_base
      ),
      first
    ),
    .groups = "drop"
  ) |>
  mutate(
    mode = "pt+drt",
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = (delta_km / total_distance_km_base) * 100
  )

car_taxi_combined <- res_vkm |>
  filter(mode %in% c("car", "taxi")) |>
  group_by(objective, solution, solution_id, level, access, zones) |>
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    across(
      c(
        rank,
        swarm_id,
        objective_sol,
        generation_found,
        violations,
        baseline_objective_value_pen,
        pso_pct_change_vs_base,
        pso_frac_of_base
      ),
      first
    ),
    .groups = "drop"
  ) |>
  mutate(
    mode = "car+taxi",
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = (delta_km / total_distance_km_base) * 100
  )

res_vkm_extended <- bind_rows(
  res_vkm,
  pt_drt_vkm_combined,
  car_taxi_combined
) |>
  arrange(objective, solution, solution_id, level, access, zones, mode)

##########
# SECTION 0d: Labels for plots
##########

objective_labels <- c(
  "sc_avg_var" = "Service Coverage\nAvg Variance",
  "sc_int_var" = "Service Coverage\nInterval Variance",
  "sc_peak_var" = "Service Coverage\nPeak Variance",
  "sc_sum_var" = "Service Coverage\nSum Variance",
  "wt_avg_tot" = "Wait Time\nAvg Total",
  "wt_avg_var" = "Wait Time\nAvg Variance",
  "wt_avg_atk" = "Wait Time\nAvg Atkinson",
  "wt_int_tot" = "Wait Time\nInterval Total",
  "wt_int_var" = "Wait Time\nInterval Variance",
  "wt_int_atk" = "Wait Time\nInterval Atkinson",
  "wt_sum_tot" = "Wait Time\nSum Total",
  "wt_sum_var" = "Wait Time\nSum Variance",
  "wt_sum_atk" = "Wait Time\nSum Atkinson",
  "wt_peak_tot" = "Wait Time\nPeak Total",
  "wt_peak_var" = "Wait Time\nPeak Variance",
  "wt_peak_atk" = "Wait Time\nPeak Atkinson"
)

objective_labels_short <- c(
  "sc_avg_var" = "SC-Avg-Var",
  "sc_int_var" = "SC-Int-Var",
  "sc_peak_var" = "SC-Peak-Var",
  "sc_sum_var" = "SC-Sum-Var",
  "wt_avg_tot" = "WT-Avg-Tot",
  "wt_avg_var" = "WT-Avg-Var",
  "wt_avg_atk" = "WT-Avg-Atk",
  "wt_int_tot" = "WT-Int-Tot",
  "wt_int_var" = "WT-Int-Var",
  "wt_int_atk" = "WT-Int-Atk",
  "wt_sum_tot" = "WT-Sum-Tot",
  "wt_sum_var" = "WT-Sum-Var",
  "wt_sum_atk" = "WT-Sum-Atk",
  "wt_peak_tot" = "WT-Peak-Tot",
  "wt_peak_var" = "WT-Peak-Var",
  "wt_peak_atk" = "WT-Peak-Atk"
)

# Apply filters to labels
if (!is.null(OBJECTIVES_TO_INCLUDE)) {
  keep_idx <- names(objective_labels) %in% OBJECTIVES_TO_INCLUDE
  objective_labels <- objective_labels[keep_idx]
  objective_labels_short <- objective_labels_short[keep_idx]
} else if (!is.null(OBJECTIVES_TO_EXCLUDE)) {
  keep_idx <- !str_detect(names(objective_labels), OBJECTIVES_TO_EXCLUDE)
  objective_labels <- objective_labels[keep_idx]
  objective_labels_short <- objective_labels_short[keep_idx]
}

level_labels <- c("trip" = "Trip", "person" = "Person", "all" = "All")
access_labels <- c("origin" = "O", "origin+destination" = "O+D", "all" = "All")
zones_labels <- c("pt" = "PT", "pt+drt" = "PT+DRT", "all" = "All")


##########################################################################
# SECTION 4.1: VALIDATION - Correlation Analysis
##########################################################################

message("\n==========================================")
message("SECTION 4.1: VALIDATION - Correlation Analysis")
message("==========================================\n")

# Apply solution rank filter
if (!is.null(MAX_SOLUTION_RANK_FOR_CORRELATION)) {
  message(glue::glue(
    "Filtering to solutions with rank <= {MAX_SOLUTION_RANK_FOR_CORRELATION}"
  ))
  res_mode_share_corr <- res_mode_share |>
    filter(solution_id <= MAX_SOLUTION_RANK_FOR_CORRELATION)
  res_vkm_extended_corr <- res_vkm_extended |>
    filter(solution_id <= MAX_SOLUTION_RANK_FOR_CORRELATION)
} else {
  message("Using all solutions for correlation analysis")
  res_mode_share_corr <- res_mode_share
  res_vkm_extended_corr <- res_vkm_extended
}

# Define all filter combinations for iteration
filter_combinations <- bind_rows(
  expand_grid(
    level = c("trip", "person"),
    access = c("origin", "origin+destination"),
    zones = c("pt", "pt+drt")
  ),
  tibble(level = "all", access = "all", zones = "all")
) |>
  mutate(
    filter_label = paste(
      level_labels[level],
      access_labels[access],
      zones_labels[zones],
      sep = " | "
    )
  )

# --- APPLY CATCHMENT FILTER HERE FOR HEATMAPS ---
if (!is.null(CATCHMENT_PLOT_EXCLUDE_REGEX)) {
  message(glue::glue(
    "Filtering catchment combinations using regex: '{CATCHMENT_PLOT_EXCLUDE_REGEX}'"
  ))
  filter_combinations <- filter_combinations |>
    filter(!str_detect(filter_label, CATCHMENT_PLOT_EXCLUDE_REGEX))
}
# ------------------------------------------------

# Helper Function
calc_correlation <- function(data, x_var, y_var) {
  x <- data[[x_var]]
  y <- data[[y_var]]
  complete_idx <- !is.na(x) & !is.na(y)
  x <- x[complete_idx]
  y <- y[complete_idx]

  if (length(x) < 3) {
    return(tibble(
      n_solutions = length(x),
      cor_spearman = NA,
      cor_pearson = NA,
      p_value_spearman = NA,
      p_value_pearson = NA
    ))
  }

  spearman_test <- tryCatch(
    cor.test(x, y, method = "spearman", exact = FALSE),
    error = function(e) NULL
  )
  pearson_test <- tryCatch(
    cor.test(x, y, method = "pearson"),
    error = function(e) NULL
  )

  tibble(
    n_solutions = length(x),
    cor_spearman = if (!is.null(spearman_test)) spearman_test$estimate else NA,
    cor_pearson = if (!is.null(pearson_test)) pearson_test$estimate else NA,
    p_value_spearman = if (!is.null(spearman_test)) {
      spearman_test$p.value
    } else {
      NA
    },
    p_value_pearson = if (!is.null(pearson_test)) pearson_test$p.value else NA
  )
}

# Run Correlations
message("Calculating correlations...")

correlation_mode_share <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share_corr |>
      filter(
        mode == "pt+drt",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "share_delta")) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "PT+DRT Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

correlation_car <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share_corr |>
      filter(
        mode == "car",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "share_delta")) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "Car Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

correlation_vkm <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_vkm_extended_corr |>
      filter(
        mode == "car+taxi",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "delta_km")) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "Car+Taxi VKM",
        objective_clean = objective_labels_short[objective]
      )
  })

# Combine
all_correlations <- bind_rows(
  correlation_mode_share,
  correlation_car,
  correlation_vkm
) |>
  mutate(
    significance = case_when(
      p_value_spearman < 0.001 ~ "***",
      p_value_spearman < 0.01 ~ "**",
      p_value_spearman < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

write_csv(
  all_correlations,
  file.path(plot_dir, "tables/table_4_1_correlations_all.csv")
)

# Matrix Table
correlation_table_wide <- all_correlations |>
  filter(outcome == "PT+DRT Mode Share") |>
  mutate(cor_display = paste0(round(cor_spearman, 2), significance)) |>
  select(objective_clean, filter_label, cor_display) |>
  pivot_wider(names_from = filter_label, values_from = cor_display)

write_csv(
  correlation_table_wide,
  file.path(plot_dir, "tables/table_4_1_correlation_matrix.csv")
)

# Plot 1: Faceted Heatmap
message("Generating heatmap plots...")
faceted_heatmap_data <- all_correlations |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    ),
    filter_label = factor(filter_label)
  )

plot_heatmap_faceted <- ggplot(
  faceted_heatmap_data,
  aes(x = outcome, y = objective_clean, fill = cor_spearman)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0(round(cor_spearman, 2), significance)),
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~filter_label, ncol = 3) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "white",
    high = "#1a9850",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman\nCorrelation"
  ) +
  labs(
    title = "Correlation: Proxy Objective vs. MATSim Outcomes",
    subtitle = "By catchment definition",
    x = "Outcome",
    y = "Objective"
  ) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_heatmap_faceted

ggsave(
  file.path(plot_dir, "fig_4_1a_correlation_heatmap_faceted.png"),
  plot_heatmap_faceted,
  width = 14,
  height = 14,
  dpi = 300
)


# -------------------------
# Plot 2a: Multi-outcome Heatmap for main filter config
# -------------------------

main_filter_label <- "Person | O+D | PT+DRT"
no_filter_label <- "All | All | All"

main_filter_correlations <- all_correlations |>
  filter(filter_label == main_filter_label) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    )
  )

ggplot(
  main_filter_correlations,
  aes(x = outcome, y = objective_clean, fill = cor_spearman)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0(round(cor_spearman, 2), significance)),
    size = 3
  ) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "white",
    high = "#1a9850",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman\nCorrelation"
  ) +
  labs(
    title = "Correlation: Proxy Objective vs. MATSim Outcomes",
    subtitle = paste("Filter:", main_filter_label),
    x = "MATSim Outcome",
    y = "Proxy Objective",
    caption = paste(
      "Expected correlations if proxy is effective:\n",
      "• PT+DRT Mode Share: Negative (lower proxy → higher PT+DRT share)\n",
      "• Car Mode Share: Positive (higher proxy → higher car share)\n",
      "• Car+Taxi VKM: Positive (higher proxy → higher car VKM)\n",
      "* p<0.05, ** p<0.01, *** p<0.001 (Spearman)"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1b_correlation_heatmap_outcomes.png"),
  width = 7,
  height = 8,
  dpi = 300
)

# -------------------------
# Plot 2b: Multi-outcome Heatmap for NO filter (All | All | All)
# -------------------------

no_filter_correlations <- all_correlations |>
  filter(filter_label == no_filter_label) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    )
  )

ggplot(
  no_filter_correlations,
  aes(x = outcome, y = objective_clean, fill = cor_spearman)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = paste0(round(cor_spearman, 2), significance)),
    size = 6
  ) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "white",
    high = "#1a9850",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Spearman\nCorrelation"
  ) +
  labs(
    title = "Correlation: Proxy Objective vs. MATSim Outcomes",
    subtitle = "No spatial filtering applied",
    x = "MATSim Outcome",
    y = "Proxy Objective",
    caption = paste(
      "Expected correlations if proxy is effective:\n",
      "• PT+DRT Mode Share: Negative (lower proxy → higher PT+DRT share)\n",
      "• Car Mode Share: Positive (higher proxy → higher car share)\n",
      "• Car+Taxi VKM: Positive (higher proxy → higher car VKM)\n",
      "* p<0.05, ** p<0.01, *** p<0.001 (Spearman)"
    )
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 10, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1b_correlation_heatmap_outcomes_no_filter.png"),
  width = 10,
  height = 9,
  dpi = 300
)

# -------------------------
# Plot 3a: Scatter plots for main filter config
# -------------------------

scatter_data <- res_mode_share |>
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt",
    !is.na(pso_frac_of_base),
    !is.na(share_delta)
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  scatter_data,
  aes(x = pso_frac_of_base, y = share_delta)
) +
  geom_point(aes(color = solution_id), size = 2.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  facet_wrap(~objective_clean, scales = "free_x", ncol = 3) +
  labs(
    title = "Proxy Objective Value vs. PT+DRT Mode Share Change",
    subtitle = paste("Filter:", main_filter_label),
    x = "Proxy Objective (fraction of baseline)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Solution\nRank",
    caption = "Dashed line: linear trend. Lower proxy value = better optimization."
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Viridis",
    rev = TRUE
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1c_proxy_vs_mode_share.png"),
  width = 16,
  height = 10,
  dpi = 300
)
# -------------------------
# Plot 3b: Scatter plots for NO filter (All | All | All)
# -------------------------

scatter_data_no_filter <- res_mode_share |>
  filter(
    mode == "pt+drt",
    level == "all",
    access == "all",
    zones == "all",
    !is.na(pso_frac_of_base),
    !is.na(share_delta)
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  scatter_data_no_filter,
  aes(x = pso_frac_of_base, y = share_delta)
) +
  geom_point(aes(color = solution_id), size = 2.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  facet_wrap(~objective_clean, scales = "free_x", ncol = 3) +
  labs(
    title = "Proxy Objective Value vs. PT+DRT Mode Share Change",
    subtitle = "No spatial filtering applied",
    x = "Proxy Objective (fraction of baseline)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Solution\nRank",
    caption = "Dashed line: linear trend. Lower proxy value = better optimization."
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Viridis",
    rev = TRUE
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1c_proxy_vs_mode_share_no_filter.png"),
  width = 16,
  height = 10,
  dpi = 300
)

# -------------------------
# Top-k Recall Analysis
# -------------------------

message("Calculating Top-k Recall...")

# Determine actual number of solutions per objective (for validation)
n_solutions_per_objective <- res_mode_share |>
  filter(mode == "pt+drt", level == "all", access == "all", zones == "all") |>
  group_by(objective) |>
  summarise(n = n_distinct(solution_id), .groups = "drop") |>
  pull(n) |>
  min()

message(glue::glue("Solutions per objective: {n_solutions_per_objective}"))

# Validate and apply configuration
TOPK_HEATMAP_K <- min(TOPK_HEATMAP_K_CONFIG, n_solutions_per_objective)
TOPK_LINE_PLOT_K_VALUES <- 1:min(
  TOPK_LINE_PLOT_MAX_K_CONFIG,
  n_solutions_per_objective
)

message(glue::glue(
  "K values for line plot: {paste(TOPK_LINE_PLOT_K_VALUES, collapse = ', ')}"
))
message(glue::glue("K value for heatmap: {TOPK_HEATMAP_K}"))

# calc_topk_recall function (unchanged)
calc_topk_recall <- function(
  data,
  proxy_col,
  outcome_col,
  outcome_direction = "higher",
  k_values = c(1, 2, 3, 5)
) {
  data <- data |>
    filter(!is.na(.data[[proxy_col]]) & !is.na(.data[[outcome_col]]))

  n_total <- nrow(data)

  map_dfr(k_values, function(k) {
    n_top_k <- k

    if (n_top_k < 1 || n_top_k > n_total) {
      return(tibble(
        k = k,
        n_top_k = n_top_k,
        n_recalled = NA_integer_,
        recall_pct = NA_real_
      ))
    }

    top_k_proxy <- data |>
      slice_min(order_by = .data[[proxy_col]], n = n_top_k) |>
      pull(solution_id)

    if (outcome_direction == "higher") {
      top_k_outcome <- data |>
        slice_max(order_by = .data[[outcome_col]], n = n_top_k) |>
        pull(solution_id)
    } else {
      top_k_outcome <- data |>
        slice_min(order_by = .data[[outcome_col]], n = n_top_k) |>
        pull(solution_id)
    }

    n_recalled <- length(intersect(top_k_proxy, top_k_outcome))

    tibble(
      k = k,
      n_top_k = n_top_k,
      n_recalled = n_recalled,
      recall_pct = round(100 * n_recalled / n_top_k, 1)
    )
  })
}

# Calculate Top-k Recall for ALL outcomes using filter_combinations (which now includes "all")
# PT+DRT Mode Share (higher = better)
topk_pt_drt <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share |>
      filter(
        mode == "pt+drt",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(
        ~ {
          calc_topk_recall(
            .x,
            proxy_col = "pso_frac_of_base",
            outcome_col = "share_delta",
            outcome_direction = "higher",
            k_values = TOPK_LINE_PLOT_K_VALUES
          )
        }
      ) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "PT+DRT Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Car Mode Share (lower = better)
topk_car <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share |>
      filter(
        mode == "car",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(
        ~ {
          calc_topk_recall(
            .x,
            proxy_col = "pso_frac_of_base",
            outcome_col = "share_delta",
            outcome_direction = "lower",
            k_values = TOPK_LINE_PLOT_K_VALUES
          )
        }
      ) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "Car Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Car+Taxi VKM (lower = better)
topk_vkm <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_vkm_extended |>
      filter(
        mode == "car+taxi",
        level == !!level,
        access == !!access,
        zones == !!zones
      ) |>
      group_by(objective) |>
      group_modify(
        ~ {
          calc_topk_recall(
            .x,
            proxy_col = "pso_frac_of_base",
            outcome_col = "delta_km",
            outcome_direction = "lower",
            k_values = TOPK_LINE_PLOT_K_VALUES
          )
        }
      ) |>
      ungroup() |>
      mutate(
        filter_label = filter_label,
        outcome = "Car+Taxi VKM",
        objective_clean = objective_labels_short[objective]
      )
  })

# Combine all outcomes (no need to add "all" separately)
topk_recall_results <- bind_rows(topk_pt_drt, topk_car, topk_vkm)

# Save full table
write_csv(
  topk_recall_results,
  file.path(plot_dir, "tables/table_4_1_topk_recall.csv")
)
# -------------------------
# Plot: Top-k Recall by k value - Main filter
# -------------------------

topk_by_k_data <- topk_recall_results |>
  filter(filter_label == main_filter_label) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    )
  )

random_chance_line <- tibble(
  k = TOPK_LINE_PLOT_K_VALUES,
  random_pct = 100 * k / n_solutions_per_objective
)

ggplot(
  topk_by_k_data,
  aes(x = k, y = recall_pct, color = objective_clean, group = objective_clean)
) +
  geom_line(
    data = random_chance_line,
    aes(x = k, y = random_pct),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~outcome, ncol = 1) +
  scale_x_continuous(
    breaks = TOPK_LINE_PLOT_K_VALUES,
    labels = TOPK_LINE_PLOT_K_VALUES
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Top-k Recall by Number of Solutions Selected",
    subtitle = paste("Filter:", main_filter_label),
    x = "k (Number of Top Solutions)",
    y = "Recall (%)",
    color = "Proxy Objective",
    caption = paste0(
      "Recall = % of proxy's top-k that are also MATSim's top-k\n",
      "Dashed line = random chance (",
      n_solutions_per_objective,
      " solutions per objective)"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1e_topk_recall_by_k.png"),
  width = 12,
  height = 10,
  dpi = 300
)

# -------------------------
# Plot: Top-k Recall by k value - NO filter (All | All | All)
# -------------------------

topk_by_k_data_no_filter <- topk_recall_results |>
  filter(filter_label == no_filter_label) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    )
  )

ggplot(
  topk_by_k_data_no_filter,
  aes(x = k, y = recall_pct, color = objective_clean, group = objective_clean)
) +
  geom_line(
    data = random_chance_line,
    aes(x = k, y = random_pct),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~outcome, ncol = 1) +
  scale_x_continuous(
    breaks = TOPK_LINE_PLOT_K_VALUES,
    labels = TOPK_LINE_PLOT_K_VALUES
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Top-k Recall by Number of Solutions Selected",
    subtitle = "No spatial filtering applied",
    x = "k (Number of Top Solutions)",
    y = "Recall (%)",
    color = "Proxy Objective",
    caption = paste0(
      "Recall = % of proxy's top-k that are also MATSim's top-k\n",
      "Dashed line = random chance (",
      n_solutions_per_objective,
      " solutions per objective)"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_1e_topk_recall_by_k_no_filter.png"),
  width = 12,
  height = 10,
  dpi = 300
)

# -------------------------
# Plot: Simplified line plot - Main filter
# -------------------------

topk_by_k_simple <- topk_recall_results |>
  filter(
    filter_label == main_filter_label,
    outcome == "PT+DRT Mode Share"
  ) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short)
  )

ggplot(
  topk_by_k_simple,
  aes(x = k, y = recall_pct, color = objective_clean, group = objective_clean)
) +
  geom_line(
    data = random_chance_line,
    aes(x = k, y = random_pct),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = TOPK_LINE_PLOT_K_VALUES,
    labels = TOPK_LINE_PLOT_K_VALUES
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Top-k Recall: Proxy vs. PT+DRT Mode Share",
    subtitle = paste("Filter:", main_filter_label),
    x = "k (Number of Top Solutions)",
    y = "Recall (%)",
    color = "Proxy Objective",
    caption = "Recall = % of proxy's top-k solutions that are also MATSim's top-k\nDashed line = random chance"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(
  file.path(plot_dir, "fig_4_1f_topk_recall_simple.png"),
  width = 10,
  height = 8,
  dpi = 300
)

# -------------------------
# Plot: Simplified line plot - NO filter
# -------------------------

topk_by_k_simple_no_filter <- topk_recall_results |>
  filter(
    filter_label == no_filter_label,
    outcome == "PT+DRT Mode Share"
  ) |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short)
  )

ggplot(
  topk_by_k_simple_no_filter,
  aes(x = k, y = recall_pct, color = objective_clean, group = objective_clean)
) +
  geom_line(
    data = random_chance_line,
    aes(x = k, y = random_pct),
    inherit.aes = FALSE,
    linetype = "dashed",
    color = "gray50",
    linewidth = 1
  ) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = TOPK_LINE_PLOT_K_VALUES,
    labels = TOPK_LINE_PLOT_K_VALUES
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Top-k Recall: Proxy vs. PT+DRT Mode Share",
    subtitle = "No spatial filtering applied",
    x = "k (Number of Top Solutions)",
    y = "Recall (%)",
    color = "Proxy Objective",
    caption = "Recall = % of proxy's top-k solutions that are also MATSim's top-k\nDashed line = random chance"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  ) +
  guides(color = guide_legend(nrow = 2))

ggsave(
  file.path(plot_dir, "fig_4_1f_topk_recall_simple_no_filter.png"),
  width = 10,
  height = 8,
  dpi = 300
)


# -------------------------
# Table: Solution Rankings by Multiple Metrics
# -------------------------

message("Creating solution rankings table...")

# Create rankings for each outcome metric
solution_rankings <- res_mode_share |>
  filter(
    level == "all",
    access == "all",
    zones == "all"
  ) |>
  select(objective, solution_id, mode, share_delta, pso_frac_of_base) |>
  # Pivot to get PT+DRT and Car in columns
  pivot_wider(
    names_from = mode,
    values_from = share_delta,
    names_prefix = "share_delta_"
  ) |>
  # Join Car+Taxi VKM data
  left_join(
    res_vkm_extended |>
      filter(
        mode == "car+taxi",
        level == "all",
        access == "all",
        zones == "all"
      ) |>
      select(objective, solution_id, car_taxi_vkm = delta_km),
    by = c("objective", "solution_id")
  ) |>
  # Calculate ranks within each objective
  group_by(objective) |>
  mutate(
    # Rank by PT+DRT mode share (higher = better, so rank 1 = highest)
    rank_pt_drt_share = rank(-`share_delta_pt+drt`, ties.method = "min"),

    # Rank by Car mode share (lower = better, so rank 1 = lowest)
    rank_car_share = rank(`share_delta_car`, ties.method = "min"),

    # Rank by Car+Taxi VKM (lower = better, so rank 1 = lowest)
    rank_car_taxi_vkm = rank(car_taxi_vkm, ties.method = "min"),

    # Rank by proxy objective (lower = better)
    rank_proxy = rank(pso_frac_of_base, ties.method = "min")
  ) |>
  ungroup() |>
  # Add objective labels
  mutate(
    objective_clean = objective_labels_short[objective]
  ) |>
  # Reorder columns for readability
  select(
    objective_clean,
    solution_id,
    pso_frac_of_base,
    rank_proxy,
    `share_delta_pt+drt`,
    rank_pt_drt_share,
    share_delta_car,
    rank_car_share,
    car_taxi_vkm,
    rank_car_taxi_vkm,
    everything()
  ) |>
  arrange(objective_clean, rank_proxy)

# Save full rankings table
write_csv(
  solution_rankings,
  file.path(plot_dir, "tables/table_4_1_solution_rankings_all.csv")
)

# -------------------------
# Table: Top-N Solutions per Objective and Metric
# -------------------------

# Configuration: how many top solutions to show
TOP_N_SOLUTIONS <- 3

# Create a summary showing top-N for each metric
top_solutions_summary <- bind_rows(
  # Top by PT+DRT mode share
  solution_rankings |>
    filter(rank_pt_drt_share <= TOP_N_SOLUTIONS) |>
    mutate(ranking_metric = "PT+DRT Mode Share (Higher is Better)") |>
    select(
      objective_clean,
      ranking_metric,
      solution_id,
      rank = rank_pt_drt_share,
      metric_value = `share_delta_pt+drt`,
      proxy_value = pso_frac_of_base,
      proxy_rank = rank_proxy
    ),

  # Top by Car mode share reduction
  solution_rankings |>
    filter(rank_car_share <= TOP_N_SOLUTIONS) |>
    mutate(ranking_metric = "Car Mode Share (Lower is Better)") |>
    select(
      objective_clean,
      ranking_metric,
      solution_id,
      rank = rank_car_share,
      metric_value = share_delta_car,
      proxy_value = pso_frac_of_base,
      proxy_rank = rank_proxy
    ),

  # Top by Car+Taxi VKM reduction
  solution_rankings |>
    filter(rank_car_taxi_vkm <= TOP_N_SOLUTIONS) |>
    mutate(ranking_metric = "Car+Taxi VKM (Lower is Better)") |>
    select(
      objective_clean,
      ranking_metric,
      solution_id,
      rank = rank_car_taxi_vkm,
      metric_value = car_taxi_vkm,
      proxy_value = pso_frac_of_base,
      proxy_rank = rank_proxy
    )
) |>
  arrange(objective_clean, ranking_metric, rank)

write_csv(
  top_solutions_summary,
  file.path(plot_dir, "tables/table_4_1_top_solutions_summary.csv")
)


# -------------------------
# Table: Compare Top Solutions Across Metrics (Wide Format for Paper)
# -------------------------

# Create a paper-ready comparison showing if top proxy solutions match top outcome solutions
top_solutions_comparison <- solution_rankings |>
  group_by(objective_clean) |>
  summarise(
    # Top 3 solutions by proxy
    top3_proxy = paste(solution_id[rank_proxy <= 3], collapse = ", "),

    # Top 3 by PT+DRT share
    top3_pt_drt = paste(solution_id[rank_pt_drt_share <= 3], collapse = ", "),
    overlap_pt_drt = length(intersect(
      solution_id[rank_proxy <= 3],
      solution_id[rank_pt_drt_share <= 3]
    )),

    # Top 3 by Car share
    top3_car = paste(solution_id[rank_car_share <= 3], collapse = ", "),
    overlap_car = length(intersect(
      solution_id[rank_proxy <= 3],
      solution_id[rank_car_share <= 3]
    )),

    # Top 3 by Car+Taxi VKM
    top3_vkm = paste(solution_id[rank_car_taxi_vkm <= 3], collapse = ", "),
    overlap_vkm = length(intersect(
      solution_id[rank_proxy <= 3],
      solution_id[rank_car_taxi_vkm <= 3]
    )),

    .groups = "drop"
  )

write_csv(
  top_solutions_comparison,
  file.path(plot_dir, "tables/table_4_1_top_solutions_comparison.csv")
)


##########################################################################
# SECTION 4.2: SYSTEM PERFORMANCE COMPARISON
##########################################################################

message("\n==========================================")
message("SECTION 4.2: SYSTEM PERFORMANCE COMPARISON")
message("==========================================\n")

# Define labels for plot subtitles
main_filter_label <- "Trip | O+D | PT+DRT"

# -------------------------
# Table 4.2a: Best Solutions by PSO Rank (Rank 0)
# -------------------------

# Get best solution (rank 0) for each objective
best_solutions_summary <- res_mode_share |>
  filter(
    solution_id == 0,
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(objective, mode, share_solution, share_base, share_delta) |>
  pivot_wider(
    names_from = mode,
    values_from = c(share_solution, share_base, share_delta),
    names_glue = "{mode}_{.value}"
  )

# Add VKM data
best_vkm_summary <- res_vkm_extended |>
  filter(
    solution_id == 0,
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt",
    mode %in% c("pt+drt", "car+taxi")
  ) |>
  select(
    objective,
    mode,
    total_distance_km_solution,
    total_distance_km_base,
    delta_km,
    delta_km_pct
  ) |>
  pivot_wider(
    names_from = mode,
    values_from = c(
      total_distance_km_solution,
      total_distance_km_base,
      delta_km,
      delta_km_pct
    ),
    names_glue = "{mode}_{.value}"
  )

# Add DRT fleet data (ALL intervals, separated by zone)
best_drt_fleet <- all_drt_deployments |>
  filter(str_detect(solution, "_00$")) |>
  # First, create comma-separated lists per zone
  group_by(objective, scenario, interval_label) |>
  summarise(
    interval_fleet = first(fleet_size),
    .groups = "drop"
  ) |>
  group_by(objective, scenario) |>
  arrange(interval_label) |>
  summarise(
    fleet_by_interval = paste(interval_fleet, collapse = ", "),
    .groups = "drop"
  ) |>
  # Now pivot to get one row per objective with both zones
  pivot_wider(
    names_from = scenario,
    values_from = fleet_by_interval,
    names_prefix = "fleet_"
  ) |>
  mutate(
    # Combine both zones with " | " separator
    drt_fleet_by_interval = paste0(
      "(",
      fleet_drtNE,
      ") | (",
      fleet_drtNW,
      ")"
    )
  ) |>
  select(objective, drt_fleet_by_interval)


# Combine into Table 4.2a
table_4_2a_scenario_comparison <- best_solutions_summary |>
  left_join(best_vkm_summary, by = "objective") |>
  left_join(best_drt_fleet, by = "objective") |>
  mutate(
    objective_clean = objective_labels_short[objective]
  ) |>
  select(
    objective_clean,
    `pt+drt_share_delta`,
    car_share_delta,
    `pt+drt_delta_km`,
    `car+taxi_delta_km`,
    drt_fleet_by_interval
  ) |>
  rename(
    Objective = objective_clean,
    `PT+DRT Share Δ (pp)` = `pt+drt_share_delta`,
    `Car Share Δ (pp)` = car_share_delta,
    `PT+DRT VKM Δ (km)` = `pt+drt_delta_km`,
    `Car+Taxi VKM Δ (km)` = `car+taxi_delta_km`,
    `DRT Fleet by Interval (NE | NW)` = drt_fleet_by_interval
  )

write_csv(
  table_4_2a_scenario_comparison,
  file.path(plot_dir, "tables/table_4_2a_scenario_comparison_best_rank.csv")
)

message("✓ Table 4.2a saved (best solutions by PSO rank)")


# -------------------------
# Table 4.2b: Best Solutions by PT+DRT Mode Share
# -------------------------

# Find best solution ID per objective based on PT+DRT share delta
best_by_pt_drt <- res_mode_share |>
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  group_by(objective) |>
  slice_max(order_by = share_delta, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(
    objective,
    best_solution_id = solution_id,
    best_share_delta = share_delta
  )

# Get summary for best PT+DRT solutions
best_pt_drt_solutions_summary <- res_mode_share |>
  inner_join(
    best_by_pt_drt,
    by = c("objective", "solution_id" = "best_solution_id")
  ) |>
  filter(
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(
    objective,
    solution_id,
    mode,
    share_solution,
    share_base,
    share_delta
  ) |>
  pivot_wider(
    names_from = mode,
    values_from = c(share_solution, share_base, share_delta),
    names_glue = "{mode}_{.value}"
  )

# Add VKM data for best PT+DRT solutions
best_pt_drt_vkm_summary <- res_vkm_extended |>
  inner_join(
    best_by_pt_drt,
    by = c("objective", "solution_id" = "best_solution_id")
  ) |>
  filter(
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt",
    mode %in% c("pt+drt", "car+taxi")
  ) |>
  select(
    objective,
    solution_id,
    mode,
    total_distance_km_solution,
    total_distance_km_base,
    delta_km,
    delta_km_pct
  ) |>
  pivot_wider(
    names_from = mode,
    values_from = c(
      total_distance_km_solution,
      total_distance_km_base,
      delta_km,
      delta_km_pct
    ),
    names_glue = "{mode}_{.value}"
  )

# Add DRT fleet data for best PT+DRT solutions (ALL intervals, separated by zone)
best_pt_drt_fleet <- all_drt_deployments |>
  inner_join(
    best_by_pt_drt |>
      mutate(
        solution_pattern = paste0("_", sprintf("%02d", best_solution_id), "$")
      ),
    by = "objective"
  ) |>
  filter(str_detect(solution, solution_pattern)) |>
  # First, create comma-separated lists per zone
  group_by(objective, scenario, interval_label) |>
  summarise(
    interval_fleet = first(fleet_size),
    .groups = "drop"
  ) |>
  group_by(objective, scenario) |>
  arrange(interval_label) |>
  summarise(
    fleet_by_interval = paste(interval_fleet, collapse = ", "),
    .groups = "drop"
  ) |>
  # Now pivot to get one row per objective with both zones
  pivot_wider(
    names_from = scenario,
    values_from = fleet_by_interval,
    names_prefix = "fleet_"
  ) |>
  mutate(
    # Combine both zones with " | " separator
    drt_fleet_by_interval = paste0(
      "(",
      fleet_drtNE,
      ") | (",
      fleet_drtNW,
      ")"
    )
  ) |>
  select(objective, drt_fleet_by_interval)

# Combine into Table 4.2b
table_4_2b_scenario_comparison <- best_pt_drt_solutions_summary |>
  left_join(best_pt_drt_vkm_summary, by = c("objective", "solution_id")) |>
  left_join(best_pt_drt_fleet, by = "objective") |>
  left_join(
    best_by_pt_drt |> select(objective, best_solution_id),
    by = "objective"
  ) |>
  mutate(
    objective_clean = objective_labels_short[objective]
  ) |>
  select(
    objective_clean,
    best_solution_id,
    `pt+drt_share_delta`,
    car_share_delta,
    `pt+drt_delta_km`,
    `car+taxi_delta_km`,
    drt_fleet_by_interval
  ) |>
  rename(
    Objective = objective_clean,
    `Solution ID` = best_solution_id,
    `PT+DRT Share Δ (pp)` = `pt+drt_share_delta`,
    `Car Share Δ (pp)` = car_share_delta,
    `PT+DRT VKM Δ (km)` = `pt+drt_delta_km`,
    `Car+Taxi VKM Δ (km)` = `car+taxi_delta_km`,
    `DRT Fleet by Interval (NE | NW)` = drt_fleet_by_interval # Updated name
  )

write_csv(
  table_4_2b_scenario_comparison,
  file.path(plot_dir, "tables/table_4_2b_scenario_comparison_best_pt_drt.csv")
)


message("✓ Table 4.2b saved (best solutions by PT+DRT mode share)")


# -------------------------
# Plot: Trade-off between PT+DRT gain and Car reduction (Standard)
# -------------------------

tradeoff_data_old <- res_mode_share |>
  filter(
    mode %in% c("pt+drt", "car"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(objective, solution_id, mode, share_delta, pso_frac_of_base) |>
  pivot_wider(names_from = mode, values_from = share_delta) |>
  rename(pt_drt_change = `pt+drt`, car_change = car) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  tradeoff_data_old,
  aes(x = car_change, y = pt_drt_change, color = pso_frac_of_base)
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dotted",
    color = "gray30"
  ) +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 3) +
  labs(
    title = "PT+DRT Gain vs. Car Loss Trade-off",
    subtitle = paste("Filter:", main_filter_label),
    x = "Car Mode Share Change (pp)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Proxy Value\n(× baseline)",
    caption = "Diagonal: 1:1 substitution. Lower-left quadrant = desirable."
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Viridis",
    rev = TRUE
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_2a_pt_car_tradeoff.png"),
  width = 16,
  height = 10,
  dpi = 300
)

# -------------------------
# Plot: VKM efficiency (Standard)
# -------------------------

vkm_mode_tradeoff_old <- res_vkm_extended |>
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(objective, solution_id, pt_vkm_change = delta_km) |>
  left_join(
    res_mode_share |>
      filter(
        mode == "pt+drt",
        level == "trip",
        access == "origin+destination",
        zones == "pt+drt"
      ) |>
      select(
        objective,
        solution_id,
        pt_share_delta = share_delta,
        pso_frac_of_base
      ),
    by = c("objective", "solution_id")
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  vkm_mode_tradeoff_old,
  aes(x = pt_vkm_change / 1000, y = pt_share_delta, color = pso_frac_of_base)
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 3) +
  labs(
    title = "PT Service Investment vs. Ridership Gain",
    subtitle = paste("Filter:", main_filter_label),
    x = "PT+DRT VKM Change (1000s km)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Proxy Value\n(× baseline)",
    caption = "Upper-right = efficient (ridership gains with service increase)."
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Viridis",
    rev = TRUE
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_2b_vkm_vs_mode_share.png"),
  width = 16,
  height = 10,
  dpi = 300
)

# -------------------------
# Define Ids for later sections
# -------------------------

# 1. Best by Rank
ids_rank0 <- res_mode_share |>
  filter(solution_id == 0) |>
  distinct(objective, solution_id)

# 2. Best by Share (already calculated as best_by_pt_drt)
ids_best_share <- best_by_pt_drt |>
  select(objective, solution_id = best_solution_id)


##########################################################################
# SECTION 4.3: CATCHMENT SENSITIVITY ANALYSIS
##########################################################################

message("\n==========================================")
message("SECTION 4.3: CATCHMENT SENSITIVITY ANALYSIS")
message("==========================================\n")

# Prepare Data: Calculate Total VKM Delta (PT+DRT + Car+Taxi) Globally
vkm_overall_delta <- res_vkm_extended |>
  filter(mode %in% c("pt+drt", "car+taxi")) |>
  group_by(objective, solution_id, level, access, zones) |>
  summarise(total_delta_km = sum(delta_km, na.rm = TRUE), .groups = "drop")


# Updated Function with Dynamic Filter
generate_catchment_analysis <- function(target_solution_ids, suffix_label) {
  message(glue::glue("Generating catchment analysis for: {suffix_label}"))

  # 1. Join Base Frame
  base_frame <- target_solution_ids |>
    cross_join(res_mode_share |> distinct(level, access, zones)) |>
    mutate(
      objective_clean = objective_labels_short[objective],
      filter_label = paste(
        level_labels[level],
        access_labels[access],
        zones_labels[zones],
        sep = " | "
      )
    )

  # 2. Get Car+Taxi Share Delta
  car_taxi_share_data <- res_mode_share |>
    filter(mode %in% c("car", "taxi")) |>
    group_by(objective, solution_id, level, access, zones) |>
    summarise(
      share_delta_combined = sum(share_delta, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Join Metrics
  catchment_metrics <- base_frame |>
    # PT+DRT Share
    left_join(
      res_mode_share |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, share_delta),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_share_change = share_delta) |>
    # Car+Taxi Share
    left_join(
      car_taxi_share_data |>
        select(
          objective,
          solution_id,
          level,
          access,
          zones,
          share_delta_combined
        ),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_share_change = share_delta_combined) |>
    # PT+DRT VKM
    left_join(
      res_vkm_extended |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_vkm_change = delta_km) |>
    # Car+Taxi VKM
    left_join(
      res_vkm_extended |>
        filter(mode == "car+taxi") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_vkm_change = delta_km)

  # 4. Save Table (Full details)
  table_data <- catchment_metrics |>
    left_join(
      vkm_overall_delta,
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(overall_vkm_change = total_delta_km) |>
    select(objective_clean, filter_label, ends_with("change")) |>
    pivot_longer(
      ends_with("change"),
      names_to = "Metric",
      values_to = "Value"
    ) |>
    pivot_wider(names_from = filter_label, values_from = Value)

  write_csv(
    table_data,
    file.path(
      plot_dir,
      glue::glue("tables/table_4_3_{suffix_label}_catchment_sensitivity.csv")
    )
  )

  # 5. Prepare Plot Data
  plot_data <- catchment_metrics |>
    select(
      objective_clean,
      filter_label,
      pt_drt_share_change,
      car_taxi_share_change,
      pt_drt_vkm_change,
      car_taxi_vkm_change
    ) |>
    pivot_longer(
      cols = ends_with("change"),
      names_to = "raw_metric",
      values_to = "value"
    )

  # --- APPLY CATCHMENT FILTER HERE FOR PLOT ---
  if (!is.null(CATCHMENT_PLOT_EXCLUDE_REGEX)) {
    plot_data <- plot_data |>
      filter(!str_detect(filter_label, CATCHMENT_PLOT_EXCLUDE_REGEX))
  }
  # ---------------------------------------------

  plot_data <- plot_data |>
    mutate(
      Metric_Category = if_else(
        str_detect(raw_metric, "share"),
        "Mode Share Change (pp)",
        "VKM Change ('000 km)"
      ),
      Mode_Group = case_when(
        str_detect(raw_metric, "pt_drt") ~ "PT + DRT",
        str_detect(raw_metric, "car_taxi") ~ "Car + Taxi"
      ),
      value_plot = if_else(
        Metric_Category == "VKM Change ('000 km)",
        value / 1000,
        value
      )
    )

  # 6. Calc Net Change
  net_change_data <- plot_data |>
    group_by(objective_clean, filter_label, Metric_Category) |>
    summarise(net_value = sum(value_plot, na.rm = TRUE), .groups = "drop")

  # 7. Plot
  plot_grid <- ggplot() +
    geom_col(
      data = plot_data,
      aes(x = filter_label, y = value_plot, fill = Mode_Group),
      position = "stack",
      width = 0.7
    ) +
    geom_errorbar(
      data = net_change_data,
      aes(
        x = filter_label,
        ymin = net_value,
        ymax = net_value,
        color = "Net Change"
      ),
      width = 0.7,
      linewidth = 0.6
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_grid(
      Metric_Category ~ objective_clean,
      scales = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(
      values = c("PT + DRT" = "#7570b3", "Car + Taxi" = "#d95f02")
    ) +
    scale_color_manual(name = NULL, values = c("Net Change" = "black")) +
    labs(
      title = "Effect of Catchment Definition on Mode Share and VKT Changes",
      subtitle = glue::glue(
        "Analysis of {suffix_label} Solutions"
      ),
      x = "Catchment Definition",
      y = NULL,
      fill = "Mode Component"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      strip.placement = "outside"
    )

  ggsave(
    file.path(
      plot_dir,
      glue::glue("fig_4_3_{suffix_label}_catchment_comparison_grid.png")
    ),
    plot_grid,
    width = 16,
    height = 10,
    dpi = 300
  )
}

# Run 4.3 a & b
generate_catchment_analysis(ids_rank0, "Best_PSO_Rank")
generate_catchment_analysis(ids_best_share, "Best_Mode_Shift")


# -------------------------
# Combined Catchment Analysis (Rank vs Best Share)
# -------------------------

message("Generating combined comparative catchment analysis (Side-by-Side)...")

# 1. Reuse logic to get data for both sets
get_catchment_data_old <- function(target_ids, type_label) {
  base_frame <- target_ids |>
    cross_join(res_mode_share |> distinct(level, access, zones)) |>
    mutate(
      objective_clean = objective_labels_short[objective],
      filter_label = paste(
        level_labels[level],
        access_labels[access],
        zones_labels[zones],
        sep = " | "
      ),
      Type = type_label
    )

  # Calculate Car+Taxi Share Delta
  car_taxi_share <- res_mode_share |>
    filter(mode %in% c("car", "taxi")) |>
    group_by(objective, solution_id, level, access, zones) |>
    summarise(
      share_delta_combined = sum(share_delta, na.rm = TRUE),
      .groups = "drop"
    )

  base_frame |>
    # PT+DRT Share
    left_join(
      res_mode_share |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, val = share_delta),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    mutate(
      Metric_Category = "Mode Share Change (pp)",
      Mode_Group = "PT + DRT"
    ) |>
    bind_rows(
      # Car+Taxi Share
      base_frame |>
        left_join(
          car_taxi_share |>
            select(
              objective,
              solution_id,
              level,
              access,
              zones,
              val = share_delta_combined
            ),
          by = c("objective", "solution_id", "level", "access", "zones")
        ) |>
        mutate(
          Metric_Category = "Mode Share Change (pp)",
          Mode_Group = "Car + Taxi"
        )
    ) |>
    bind_rows(
      # PT+DRT VKM
      base_frame |>
        left_join(
          res_vkm_extended |>
            filter(mode == "pt+drt") |>
            select(
              objective,
              solution_id,
              level,
              access,
              zones,
              val = delta_km
            ),
          by = c("objective", "solution_id", "level", "access", "zones")
        ) |>
        mutate(
          Metric_Category = "VKM Change ('000 km)",
          Mode_Group = "PT + DRT",
          val = val / 1000
        )
    ) |>
    bind_rows(
      # Car+Taxi VKM
      base_frame |>
        left_join(
          res_vkm_extended |>
            filter(mode == "car+taxi") |>
            select(
              objective,
              solution_id,
              level,
              access,
              zones,
              val = delta_km
            ),
          by = c("objective", "solution_id", "level", "access", "zones")
        ) |>
        mutate(
          Metric_Category = "VKM Change ('000 km)",
          Mode_Group = "Car + Taxi",
          val = val / 1000
        )
    )
}

# 2. Bind Data (Using same labels as 4.3y for consistency)
combined_catchment_data <- bind_rows(
  get_catchment_data_old(ids_rank0, "Best PSO Rank"),
  get_catchment_data_old(ids_best_share, "Best Real Share")
) |>
  filter(
    !str_detect(filter_label, CATCHMENT_PLOT_EXCLUDE_REGEX)
  ) |>
  mutate(
    # Create unified fill group for legend matching 4.3y
    Fill_Group = paste(Mode_Group, Type, sep = " - ")
  )

# --- MANUAL DODGING LOGIC FOR X-AXIS ---
# This places the two bars side-by-side for each catchment definition

# Define X-axis order
catchment_labels_ordered <- sort(unique(combined_catchment_data$filter_label))

bar_width <- 0.35
offset <- 0.2

combined_catchment_pos <- combined_catchment_data |>
  mutate(
    # Convert categorical X to numeric position
    x_base = as.numeric(factor(
      filter_label,
      levels = catchment_labels_ordered
    )),
    # Shift position based on Type: "Best Share" right, "Rank" left
    x_pos = if_else(Type == "Best Real Share", x_base + offset, x_base - offset)
  )

# Calculate Net Change for Error Bars (Positioned at x_pos)
net_catchment_combined <- combined_catchment_pos |>
  group_by(objective_clean, x_pos, Type, Metric_Category) |>
  summarise(net_value = sum(val, na.rm = TRUE), .groups = "drop")

# Define Colors (Matching 4.3y)
catchment_colors <- c(
  "PT + DRT - Best Real Share" = "#7570b3", # Solid Purple
  "PT + DRT - Best PSO Rank" = "#bcbddc", # Light Purple
  "Car + Taxi - Best Real Share" = "#d95f02", # Solid Orange
  "Car + Taxi - Best PSO Rank" = "#fdbe85" # Light Orange
)

# 4. Plot
plot_combined_catchment <- ggplot() +
  # Stacked Bars (manually positioned on X)
  geom_col(
    data = combined_catchment_pos,
    aes(x = x_pos, y = val, fill = Fill_Group),
    width = bar_width,
    position = "stack"
  ) +

  # Net Change Marker (VSegment) - Only for VKM
  geom_segment(
    data = net_catchment_combined |> filter(str_detect(Metric_Category, "VKM")),
    aes(
      x = x_pos - 0.22, # Slightly wider than bar
      xend = x_pos + 0.22,
      y = net_value,
      yend = net_value,
      color = "Net Change"
    ),
    linewidth = 1
  ) +

  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +

  # Facet Grid: Rows = Metric, Cols = Objective
  facet_grid(
    Metric_Category ~ objective_clean,
    scales = "free_y",
    switch = "y"
  ) +

  scale_fill_manual(
    values = catchment_colors,
    name = "",
    labels = c(
      "PT + DRT - Best Real Share" = "PT + DRT - Best Mode Shift",
      "PT + DRT - Best PSO Rank" = "PT + DRT - Best PSO Rank",
      "Car + Taxi - Best Real Share" = "Car + Taxi - Best Mode Shift",
      "Car + Taxi - Best PSO Rank" = "Car + Taxi - Best PSO Rank"
    )
  ) +
  scale_color_manual(name = NULL, values = c("Net Change" = "black")) +

  # Custom X Axis: Map numeric positions back to labels
  scale_x_continuous(
    breaks = seq_along(catchment_labels_ordered),
    labels = catchment_labels_ordered
  ) +

  labs(
    title = "Effect of catchment definition on Mode Share and VKT changes",
    subtitle = "Analysis of Best PSO Rank & Best Mode Shift Solutions",
    x = "Catchment Definition",
    y = NULL
  ) +

  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.placement = "outside",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() # Hide vertical grid lines to emphasize grouping
  ) +
  guides(fill = guide_legend(ncol = 2))

ggsave(
  file.path(plot_dir, "fig_4_3_combined_catchment_comparison.png"),
  plot_combined_catchment,
  width = 16,
  height = 12,
  dpi = 300
)

message("✓ Figure 4.3 combined saved (Side-by-Side)")

##########################################################################
# SECTION 4.4: SUMMARY COMPARISON (Table 4.2c & Simple Global Plot)
##########################################################################

message("\n==========================================")
message("SECTION 4.4: SUMMARY COMPARISON")
message("==========================================\n")

# -------------------------
# Table 4.2c: Combined "Best Rank" vs "Best Share" (Expanded Metrics)
# -------------------------

# Helper to extract all 4 metrics for specific solution IDs
get_solution_metrics_expanded <- function(ids_df, label_suffix) {
  # 1. Get Car+Taxi Share Delta specific to this subset
  car_taxi_share_data <- res_mode_share |>
    filter(
      mode %in% c("car", "taxi"),
      level == "all",
      access == "all",
      zones == "all"
    ) |>
    group_by(objective, solution_id) |>
    summarise(
      share_delta_combined = sum(share_delta, na.rm = TRUE),
      .groups = "drop"
    )

  ids_df |>
    # Join PT+DRT Share
    left_join(
      res_mode_share |>
        filter(
          mode == "pt+drt",
          level == "all",
          access == "all",
          zones == "all"
        ) |>
        select(objective, solution_id, share_delta),
      by = c("objective", "solution_id")
    ) |>
    rename(pt_drt_share = share_delta) |>

    # Join Car+Taxi Share
    left_join(
      car_taxi_share_data,
      by = c("objective", "solution_id")
    ) |>
    rename(car_taxi_share = share_delta_combined) |>

    # Join PT+DRT VKM
    left_join(
      res_vkm_extended |>
        filter(
          mode == "pt+drt",
          level == "all",
          access == "all",
          zones == "all"
        ) |>
        select(objective, solution_id, delta_km),
      by = c("objective", "solution_id")
    ) |>
    rename(pt_drt_vkm = delta_km) |>

    # Join Car+Taxi VKM
    left_join(
      res_vkm_extended |>
        filter(
          mode == "car+taxi",
          level == "all",
          access == "all",
          zones == "all"
        ) |>
        select(objective, solution_id, delta_km),
      by = c("objective", "solution_id")
    ) |>
    rename(car_taxi_vkm = delta_km) |>

    # Rename all value columns with suffix
    rename_with(
      ~ paste0(., "_", label_suffix),
      c(pt_drt_share, car_taxi_share, pt_drt_vkm, car_taxi_vkm, solution_id)
    )
}

# Create combined table
table_4_2c <- distinct(res_mode_share, objective) |>
  inner_join(
    get_solution_metrics_expanded(ids_rank0, "rank0"),
    by = "objective"
  ) |>
  inner_join(
    get_solution_metrics_expanded(ids_best_share, "best"),
    by = "objective"
  ) |>
  mutate(objective_clean = objective_labels_short[objective]) |>
  select(
    objective_clean,
    # Best Rank Columns
    id_rank0 = solution_id_rank0,
    pt_drt_share_rank0,
    car_taxi_share_rank0,
    pt_drt_vkm_rank0,
    car_taxi_vkm_rank0,
    # Best Share Columns
    id_best = solution_id_best,
    pt_drt_share_best,
    car_taxi_share_best,
    pt_drt_vkm_best,
    car_taxi_vkm_best
  ) |>
  arrange(objective_clean)

write_csv(
  table_4_2c,
  file.path(plot_dir, "tables/table_4_2c_combined_summary.csv")
)

message("✓ Table 4.2c saved (Expanded Metrics)")


# -------------------------
# Figure 4.3x: Global Catchment Summary Plot (Stacked & Horizontal)
# -------------------------

# Prepare data in long format, mirroring the structure used in catchment plots
plot_data_global <- bind_rows(
  # --- Rank 0 Data ---
  table_4_2c |>
    select(objective_clean, val = pt_drt_share_rank0) |>
    mutate(
      Type = "Best PSO Rank",
      Metric_Category = "Mode Share Change (pp)",
      Mode_Group = "PT + DRT"
    ),
  table_4_2c |>
    select(objective_clean, val = car_taxi_share_rank0) |>
    mutate(
      Type = "Best PSO Rank",
      Metric_Category = "Mode Share Change (pp)",
      Mode_Group = "Car + Taxi"
    ),
  table_4_2c |>
    select(objective_clean, val = pt_drt_vkm_rank0) |>
    mutate(
      Type = "Best PSO Rank",
      Metric_Category = "VKM Change ('000 km)",
      Mode_Group = "PT + DRT",
      val = val / 1000
    ),
  table_4_2c |>
    select(objective_clean, val = car_taxi_vkm_rank0) |>
    mutate(
      Type = "Best PSO Rank",
      Metric_Category = "VKM Change ('000 km)",
      Mode_Group = "Car + Taxi",
      val = val / 1000
    ),

  # --- Best Share Data ---
  table_4_2c |>
    select(objective_clean, val = pt_drt_share_best) |>
    mutate(
      Type = "Best Real Share",
      Metric_Category = "Mode Share Change (pp)",
      Mode_Group = "PT + DRT"
    ),
  table_4_2c |>
    select(objective_clean, val = car_taxi_share_best) |>
    mutate(
      Type = "Best Real Share",
      Metric_Category = "Mode Share Change (pp)",
      Mode_Group = "Car + Taxi"
    ),
  table_4_2c |>
    select(objective_clean, val = pt_drt_vkm_best) |>
    mutate(
      Type = "Best Real Share",
      Metric_Category = "VKM Change ('000 km)",
      Mode_Group = "PT + DRT",
      val = val / 1000
    ),
  table_4_2c |>
    select(objective_clean, val = car_taxi_vkm_best) |>
    mutate(
      Type = "Best Real Share",
      Metric_Category = "VKM Change ('000 km)",
      Mode_Group = "Car + Taxi",
      val = val / 1000
    )
)

# Calculate Net Change for Error Bars
net_change_global <- plot_data_global |>
  group_by(objective_clean, Type, Metric_Category) |>
  summarise(net_value = sum(val, na.rm = TRUE), .groups = "drop")

# Plot
plot_global <- ggplot() +
  # Stacked bars
  geom_col(
    data = plot_data_global,
    aes(x = val, y = fct_rev(objective_clean), fill = Mode_Group),
    position = "stack",
    width = 0.7
  ) +
  # Net Change Dash
  geom_errorbar(
    data = net_change_global,
    aes(
      xmin = net_value,
      xmax = net_value,
      y = fct_rev(objective_clean),
      color = "Net Change"
    ),
    width = 0.7,
    linewidth = 0.6
  ) +

  geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +

  # Facet Grid: Cols = Metric, Rows = Selection Type (Rank vs Best)
  facet_grid(Type ~ Metric_Category, scales = "free_x") +

  scale_fill_manual(
    values = c("PT + DRT" = "#7570b3", "Car + Taxi" = "#d95f02")
  ) +
  scale_color_manual(name = NULL, values = c("Net Change" = "black")) +

  labs(
    title = "System-Wide Performance Summary (Global Catchment)",
    subtitle = "Analysis of All | All | All Filter - Comparing Selection Methods",
    x = "Change vs Baseline",
    y = NULL,
    fill = "Mode Component"
  ) +

  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave(
  file.path(plot_dir, "fig_4_3x_global_summary_horizontal.png"),
  plot_global,
  width = 12,
  height = 10,
  dpi = 300
)

message("✓ Figure 4.3x saved (Global horizontal summary with components)")

# -------------------------
# Figure 4.3y: Grouped Global Summary (Manual Layout)
# -------------------------

message("Generating manual grouped summary plot...")

# 1. Prepare Data for Manual Geometry
# We use geom_rect to avoid position_dodge/stack conflicts with continuous axes

# Ensure objective factor levels are ordered correctly for the axis
objectives_ordered <- levels(factor(plot_data_global$objective_clean))

# Define geometry parameters
bar_height <- 0.35
offset <- 0.21 # Distance from center of objective tick

# FIX: Renamed variable to plot_data_grouped to match ggplot call
# FIX: Renamed column y_center to y_pos to match aes()
plot_data_grouped <- plot_data_global |>
  mutate(
    # Create combined grouping for Fill (4 colors)
    Fill_Group = paste(Mode_Group, Type, sep = " - "),

    # Convert Objective to numeric Y-center
    obj_num = as.numeric(factor(objective_clean, levels = objectives_ordered)),

    # Determine Y-center for this bar type
    y_pos = if_else(
      Type == "Best Real Share",
      obj_num + offset,
      obj_num - offset
    ),

    # Calculate Rect coordinates
    # (Since PT is + and Car is -, we stack from 0 outwards)
    ymin = y_pos - (bar_height / 2),
    ymax = y_pos + (bar_height / 2),
    xmin = 0,
    xmax = val
  )

# FIX: Renamed variable to net_change_grouped to match ggplot call
# FIX: Renamed column y_center to y_pos
net_change_grouped <- net_change_global |>
  mutate(
    obj_num = as.numeric(factor(objective_clean, levels = objectives_ordered)),
    y_pos = if_else(
      Type == "Best Real Share",
      obj_num + offset,
      obj_num - offset
    ),
    ymin = y_pos - (bar_height / 2),
    ymax = y_pos + (bar_height / 2)
  )

# 2. Define 4-Color Palette (Solid vs Light)
custom_fill_colors <- c(
  "PT + DRT - Best Real Share" = "#7570b3", # Solid Purple
  "PT + DRT - Best PSO Rank" = "#bcbddc", # Light Purple
  "Car + Taxi - Best Real Share" = "#d95f02", # Solid Orange
  "Car + Taxi - Best PSO Rank" = "#fdbe85" # Light Orange
)

# 3. Create Plot (Hybrid Approach)
plot_global_grouped <- ggplot() +

  # 1. BARS: Use geom_col for safe, automatic stacking
  geom_col(
    data = plot_data_grouped,
    aes(
      x = val,
      y = y_pos,
      fill = Fill_Group
    ),
    width = 0.35, # Controls bar thickness easily
    position = "stack", # Ensures PT and Car stack, not overlap
    orientation = "y" # Fixes the "vertical vs horizontal" confusion
  ) +

  # 2. NET CHANGE: Use geom_segment (Your manual approach is prettier here)
  # We use y_pos from the grouped data, but calculate the segment height manually
  geom_segment(
    data = net_change_grouped |> filter(str_detect(Metric_Category, "VKM")),
    aes(
      x = net_value,
      xend = net_value,
      y = y_pos - 0.22, # (0.35 width / 2) + small padding
      yend = y_pos + 0.22,
      color = "Net Change"
    ),
    linewidth = 1
  ) +

  geom_vline(xintercept = 0, color = "black", linewidth = 0.3) +
  facet_wrap(~Metric_Category, scales = "free_x") +

  # SCALES
  scale_fill_manual(
    values = custom_fill_colors,
    name = "",
    labels = c(
      "PT + DRT - Best Real Share" = "PT + DRT - Best Mode Shift",
      "PT + DRT - Best PSO Rank" = "PT + DRT - Best PSO Rank",
      "Car + Taxi - Best Real Share" = "Car + Taxi - Best Mode Shift",
      "Car + Taxi - Best PSO Rank" = "Car + Taxi - Best PSO Rank"
    )
  ) +
  scale_color_manual(name = NULL, values = c("Net Change" = "black")) +

  # Y AXIS (Map numeric positions back to labels)
  scale_y_continuous(
    breaks = seq_along(objectives_ordered),
    labels = objectives_ordered,
    expand = expansion(mult = 0.05)
  ) +

  labs(
    title = "Mode Share and VKM Change Per Objective",
    subtitle = "Comparing Best Ranked PSO Solution (Light) vs PSO Solution with Highest PT+DRT Mode Shift (Dark)",
    x = "Change vs Baseline",
    y = NULL
  ) +

  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    axis.text.y = element_text(size = 10, face = "bold", color = "black"),
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(fill = guide_legend(ncol = 2, reverse = TRUE))

ggsave(
  file.path(plot_dir, "fig_4_3y_global_summary_grouped.png"),
  plot_global_grouped,
  width = 12,
  height = 8,
  dpi = 300
)

message("✓ Figure 4.3y saved (Grouped summary)")

# =========================================================================
# SECTION 5: FLEET SIZE ANALYSIS (combined_fleet_sizes.csv)
# =========================================================================

message("\n==========================================")
message("SECTION 5: FLEET SIZE ANALYSIS")
message("==========================================\n")

# --- 5.0 Load Combined Fleet Data ---
combined_fleet_path <- file.path(input_dir, "combined_fleet_sizes.csv")
if (!file.exists(combined_fleet_path)) {
  stop(glue::glue("Combined fleet file not found: {combined_fleet_path}"))
}

combined_fleet <- read_csv(combined_fleet_path, show_col_types = FALSE)
message(glue::glue("✓ Loaded combined fleet data: {nrow(combined_fleet)} rows"))

# Add solution_id (numeric) from solution name
combined_fleet <- combined_fleet |>
  mutate(
    solution_id = as.integer(str_extract(solution, "\\d+"))
  )

# --- 5.0b Identify Peak Interval ---
# Peak = interval with highest bus_fleet_base
peak_row <- combined_fleet |>
  distinct(interval_label, bus_fleet_base) |>
  slice_max(order_by = bus_fleet_base, n = 1, with_ties = FALSE)

peak_interval <- peak_row$interval_label
base_fleet_peak <- peak_row$bus_fleet_base

message(glue::glue(
  "Peak interval: {peak_interval}h (Base bus fleet: {base_fleet_peak})"
))

# --- 5.0c fleet_peak: Combined fleet data filtered to peak interval ---
fleet_peak <- combined_fleet |>
  filter(interval_label == peak_interval)

# --- 5.0d Best Solution IDs ---
# ids_rank0: Best PSO rank (solution_id == 0) per objective
ids_rank0 <- res_mode_share |>
  filter(
    solution_id == 0,
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  distinct(objective) |>
  mutate(solution_id = 0L)

# ids_best_share: Best PT+DRT mode share per objective
ids_best_share <- res_mode_share |>
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  group_by(objective) |>
  slice_max(order_by = share_delta, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(objective, solution_id)

# --- 5.0e Catchment Helper Function & Plot Constants ---

# Reusable function to build catchment comparison data for a set of solution IDs
get_catchment_data <- function(target_ids, type_label) {
  # Get Car+Taxi Share Delta
  car_taxi_share_data <- res_mode_share |>
    filter(mode %in% c("car", "taxi")) |>
    group_by(objective, solution_id, level, access, zones) |>
    summarise(
      share_delta_combined = sum(share_delta, na.rm = TRUE),
      .groups = "drop"
    )

  base_frame <- target_ids |>
    cross_join(res_mode_share |> distinct(level, access, zones)) |>
    mutate(
      objective_clean = objective_labels_short[objective],
      filter_label = paste(
        level_labels[level],
        access_labels[access],
        zones_labels[zones],
        sep = " | "
      )
    )

  catchment_metrics <- base_frame |>
    left_join(
      res_mode_share |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, share_delta),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_share_change = share_delta) |>
    left_join(
      car_taxi_share_data,
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_share_change = share_delta_combined) |>
    left_join(
      res_vkm_extended |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_vkm_change = delta_km) |>
    left_join(
      res_vkm_extended |>
        filter(mode == "car+taxi") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_vkm_change = delta_km)

  catchment_metrics |>
    select(
      objective_clean,
      filter_label,
      pt_drt_share_change,
      car_taxi_share_change,
      pt_drt_vkm_change,
      car_taxi_vkm_change
    ) |>
    pivot_longer(
      cols = ends_with("change"),
      names_to = "raw_metric",
      values_to = "value"
    ) |>
    mutate(
      Metric_Category = if_else(
        str_detect(raw_metric, "share"),
        "Mode Share Change (pp)",
        "VKM Change ('000 km)"
      ),
      Mode_Group = case_when(
        str_detect(raw_metric, "pt_drt") ~ "PT + DRT",
        str_detect(raw_metric, "car_taxi") ~ "Car + Taxi"
      ),
      val = if_else(
        Metric_Category == "VKM Change ('000 km)",
        value / 1000,
        value
      ),
      Type = type_label
    )
}

# Plot constants for combined catchment
offset <- 0.22
bar_width <- 0.4
catchment_colors <- c(
  "PT + DRT - Best PSO Rank" = "#7570b3",
  "Car + Taxi - Best PSO Rank" = "#d95f02",
  "PT + DRT - Best Real Share" = "#a6bddb",
  "Car + Taxi - Best Real Share" = "#fdae6b"
)

message("✓ Fleet analysis setup complete")


# -------------------------
# 5.1 Fleet-Colored Trade-off Plot
# -------------------------

# Plot: Trade-off between PT+DRT gain and Car reduction
# -------------------------

tradeoff_data <- res_mode_share |>
  filter(
    mode %in% c("pt+drt", "car"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(objective, solution_id, mode, share_delta, pso_frac_of_base) |>
  pivot_wider(names_from = mode, values_from = share_delta) |>
  rename(pt_drt_change = `pt+drt`, car_change = car) |>
  # Join fleet data at peak
  left_join(
    fleet_peak |> select(objective, solution_id, total_fleet_pct_change),
    by = c("objective", "solution_id")
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  tradeoff_data,
  aes(x = car_change, y = pt_drt_change, color = total_fleet_pct_change)
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dotted",
    color = "gray30"
  ) +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 3) +
  scale_color_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    name = glue::glue("Total Fleet\n% Change\n(Peak: {peak_interval}h)")
  ) +
  labs(
    title = "PT+DRT Gain vs. Car Loss — Colored by Fleet Size Change",
    subtitle = glue::glue(
      "Filter: Trip | O+D | PT+DRT | Peak interval: {peak_interval}h (Base fleet: {base_fleet_peak})"
    ),
    x = "Car Mode Share Change (pp)",
    y = "PT+DRT Mode Share Change (pp)",
    caption = "Diagonal: 1:1 substitution. Blue = fleet reduction, Red = fleet increase."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_2a_pt_car_tradeoff_fleet.png"),
  width = 16,
  height = 10,
  dpi = 300
)
message("✓ fig_4_2a fleet-colored tradeoff saved")


# -------------------------
# 5.2 Fleet-Colored VKM Plot (New fig_4_2b variant)
# -------------------------

message("Creating fleet-colored VKM plot...")

vkm_fleet_data <- res_vkm_extended |>
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) |>
  select(objective, solution_id, pt_vkm_change = delta_km) |>
  left_join(
    res_mode_share |>
      filter(
        mode == "pt+drt",
        level == "trip",
        access == "origin+destination",
        zones == "pt+drt"
      ) |>
      select(objective, solution_id, pt_share_delta = share_delta),
    by = c("objective", "solution_id")
  ) |>
  left_join(
    fleet_peak |> select(objective, solution_id, total_fleet_pct_change),
    by = c("objective", "solution_id")
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  vkm_fleet_data,
  aes(
    x = pt_vkm_change / 1000,
    y = pt_share_delta,
    color = total_fleet_pct_change
  )
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 3) +
  scale_color_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    name = glue::glue("Total Fleet\n% Change\n(Peak: {peak_interval}h)")
  ) +
  labs(
    title = "PT Service Investment vs. Ridership — Colored by Fleet Size Change",
    subtitle = glue::glue(
      "Filter: Trip | O+D | PT+DRT | Peak interval: {peak_interval}h (Base fleet: {base_fleet_peak})"
    ),
    x = "PT+DRT VKM Change (1000s km)",
    y = "PT+DRT Mode Share Change (pp)",
    caption = "Blue = fleet reduction, Red = fleet increase. Upper-right = efficient."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_4_2b_vkm_vs_mode_share_fleet.png"),
  width = 16,
  height = 10,
  dpi = 300
)
message("✓ fig_4_2b fleet-colored VKM saved")


# -------------------------
# 5.3 Catchment Grids with Fleet Labels (New variants)
# -------------------------

message("Creating catchment grids with fleet labels...")

# Helper: Get fleet label string for a given objective + solution_id at peak
get_fleet_label <- function(obj, sol_id) {
  row <- fleet_peak |>
    filter(objective == obj, solution_id == sol_id)
  if (nrow(row) == 0) {
    return("")
  }
  glue::glue(
    "Bus: {row$bus_fleet_diff} | DRT: {row$drt_fleet_solution} ({sprintf('%+.1f', row$total_fleet_pct_change)}%)"
  )
}

# Updated catchment analysis with fleet-annotated facet labels
generate_catchment_analysis_fleet <- function(
  target_solution_ids,
  suffix_label
) {
  message(glue::glue(
    "Generating fleet-annotated catchment analysis for: {suffix_label}"
  ))

  # Build fleet labels per objective
  fleet_labels <- target_solution_ids |>
    rowwise() |>
    mutate(fleet_info = get_fleet_label(objective, solution_id)) |>
    ungroup() |>
    mutate(
      objective_fleet_label = paste0(
        objective_labels_short[objective],
        "\n",
        fleet_info
      )
    )

  # 1. Join Base Frame
  base_frame <- target_solution_ids |>
    cross_join(res_mode_share |> distinct(level, access, zones)) |>
    mutate(
      objective_clean = objective_labels_short[objective],
      filter_label = paste(
        level_labels[level],
        access_labels[access],
        zones_labels[zones],
        sep = " | "
      )
    ) |>
    left_join(
      fleet_labels |> select(objective, objective_fleet_label),
      by = "objective"
    )

  # 2. Get Car+Taxi Share Delta
  car_taxi_share_data <- res_mode_share |>
    filter(mode %in% c("car", "taxi")) |>
    group_by(objective, solution_id, level, access, zones) |>
    summarise(
      share_delta_combined = sum(share_delta, na.rm = TRUE),
      .groups = "drop"
    )

  # 3. Join Metrics
  catchment_metrics <- base_frame |>
    left_join(
      res_mode_share |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, share_delta),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_share_change = share_delta) |>
    left_join(
      car_taxi_share_data |>
        select(
          objective,
          solution_id,
          level,
          access,
          zones,
          share_delta_combined
        ),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_share_change = share_delta_combined) |>
    left_join(
      res_vkm_extended |>
        filter(mode == "pt+drt") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(pt_drt_vkm_change = delta_km) |>
    left_join(
      res_vkm_extended |>
        filter(mode == "car+taxi") |>
        select(objective, solution_id, level, access, zones, delta_km),
      by = c("objective", "solution_id", "level", "access", "zones")
    ) |>
    rename(car_taxi_vkm_change = delta_km)

  # 4. Prepare Plot Data
  plot_data <- catchment_metrics |>
    select(
      objective_fleet_label,
      filter_label,
      pt_drt_share_change,
      car_taxi_share_change,
      pt_drt_vkm_change,
      car_taxi_vkm_change
    ) |>
    pivot_longer(
      cols = ends_with("change"),
      names_to = "raw_metric",
      values_to = "value"
    )

  if (!is.null(CATCHMENT_PLOT_EXCLUDE_REGEX)) {
    plot_data <- plot_data |>
      filter(!str_detect(filter_label, CATCHMENT_PLOT_EXCLUDE_REGEX))
  }

  plot_data <- plot_data |>
    mutate(
      Metric_Category = if_else(
        str_detect(raw_metric, "share"),
        "Mode Share Change (pp)",
        "VKM Change ('000 km)"
      ),
      Mode_Group = case_when(
        str_detect(raw_metric, "pt_drt") ~ "PT + DRT",
        str_detect(raw_metric, "car_taxi") ~ "Car + Taxi"
      ),
      value_plot = if_else(
        Metric_Category == "VKM Change ('000 km)",
        value / 1000,
        value
      )
    )

  net_change_data <- plot_data |>
    group_by(objective_fleet_label, filter_label, Metric_Category) |>
    summarise(net_value = sum(value_plot, na.rm = TRUE), .groups = "drop")

  plot_grid <- ggplot() +
    geom_col(
      data = plot_data,
      aes(x = filter_label, y = value_plot, fill = Mode_Group),
      position = "stack",
      width = 0.7
    ) +
    geom_errorbar(
      data = net_change_data,
      aes(
        x = filter_label,
        ymin = net_value,
        ymax = net_value,
        color = "Net Change"
      ),
      width = 0.7,
      linewidth = 0.6
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    facet_grid(
      Metric_Category ~ objective_fleet_label,
      scales = "free_y",
      switch = "y"
    ) +
    scale_fill_manual(
      values = c("PT + DRT" = "#7570b3", "Car + Taxi" = "#d95f02")
    ) +
    scale_color_manual(name = NULL, values = c("Net Change" = "black")) +
    labs(
      title = "Effect of Catchment Definition on Mode Share and VKT Changes",
      subtitle = glue::glue(
        "{suffix_label} Solutions | Peak fleet label: Bus Δ | DRT fleet (Total Δ%)"
      ),
      x = "Catchment Definition",
      y = NULL,
      fill = "Mode Component",
      caption = glue::glue(
        "Fleet stats shown for peak interval ({peak_interval}h). Base bus fleet: {base_fleet_peak}."
      )
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.placement = "outside",
      strip.text.x = element_text(size = 8),
      strip.text.y = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
    )

  ggsave(
    file.path(
      plot_dir,
      glue::glue("fig_4_3_{suffix_label}_catchment_fleet.png")
    ),
    plot_grid,
    width = 18,
    height = 10,
    dpi = 300
  )
}

generate_catchment_analysis_fleet(ids_rank0, "Best_PSO_Rank")
generate_catchment_analysis_fleet(ids_best_share, "Best_Mode_Shift")
message("✓ Fleet-annotated catchment grids saved")


# -------------------------
# 5.4 Combined Catchment with Fleet Footnotes
# -------------------------

message("Creating combined catchment with fleet footnotes...")

# Build fleet info for both selection types
fleet_rank0_labels <- ids_rank0 |>
  rowwise() |>
  mutate(fleet_info = get_fleet_label(objective, solution_id)) |>
  ungroup() |>
  mutate(objective_clean = objective_labels_short[objective])

fleet_best_labels <- ids_best_share |>
  rowwise() |>
  mutate(fleet_info = get_fleet_label(objective, solution_id)) |>
  ungroup() |>
  mutate(objective_clean = objective_labels_short[objective])

# Build composite facet label: objective + fleet info for both
composite_labels <- fleet_rank0_labels |>
  select(objective_clean, rank_fleet = fleet_info) |>
  left_join(
    fleet_best_labels |> select(objective_clean, best_fleet = fleet_info),
    by = "objective_clean"
  ) |>
  mutate(
    composite_label = paste0(
      objective_clean,
      "\nRank: ",
      rank_fleet,
      "\nBest: ",
      best_fleet
    )
  )

# Re-create combined catchment data with composite labels
combined_catchment_fleet <- bind_rows(
  get_catchment_data(ids_rank0, "Best PSO Rank"),
  get_catchment_data(ids_best_share, "Best Real Share")
) |>
  filter(!str_detect(filter_label, CATCHMENT_PLOT_EXCLUDE_REGEX)) |>
  mutate(Fill_Group = paste(Mode_Group, Type, sep = " - ")) |>
  left_join(
    composite_labels |> select(objective_clean, composite_label),
    by = "objective_clean"
  )

# Manual dodging
catchment_labels_ordered_fleet <- sort(unique(
  combined_catchment_fleet$filter_label
))

combined_catchment_fleet_pos <- combined_catchment_fleet |>
  mutate(
    x_base = as.numeric(factor(
      filter_label,
      levels = catchment_labels_ordered_fleet
    )),
    x_pos = if_else(Type == "Best Real Share", x_base + offset, x_base - offset)
  )

net_catchment_fleet <- combined_catchment_fleet_pos |>
  group_by(composite_label, x_pos, Type, Metric_Category) |>
  summarise(net_value = sum(val, na.rm = TRUE), .groups = "drop")

plot_combined_fleet <- ggplot() +
  geom_col(
    data = combined_catchment_fleet_pos,
    aes(x = x_pos, y = val, fill = Fill_Group),
    width = bar_width,
    position = "stack"
  ) +
  geom_segment(
    data = net_catchment_fleet |> filter(str_detect(Metric_Category, "VKM")),
    aes(
      x = x_pos - 0.22,
      xend = x_pos + 0.22,
      y = net_value,
      yend = net_value,
      color = "Net Change"
    ),
    linewidth = 1
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(
    Metric_Category ~ composite_label,
    scales = "free_y",
    switch = "y"
  ) +
  scale_fill_manual(
    values = catchment_colors,
    name = "",
    labels = c(
      "PT + DRT - Best Real Share" = "PT + DRT - Best Mode Shift",
      "PT + DRT - Best PSO Rank" = "PT + DRT - Best PSO Rank",
      "Car + Taxi - Best Real Share" = "Car + Taxi - Best Mode Shift",
      "Car + Taxi - Best PSO Rank" = "Car + Taxi - Best PSO Rank"
    )
  ) +
  scale_color_manual(name = NULL, values = c("Net Change" = "black")) +
  scale_x_continuous(
    breaks = seq_along(catchment_labels_ordered_fleet),
    labels = catchment_labels_ordered_fleet
  ) +
  labs(
    title = "Effect of Catchment Definition on Mode Share and VKT Changes",
    subtitle = "Best PSO Rank vs Best Mode Shift",
    x = "Catchment Definition",
    y = NULL,
    caption = glue::glue(
      "Fleet labels: Bus Δ | DRT fleet (Total Δ%). Peak interval: {peak_interval}h. Base bus fleet: {base_fleet_peak}.\n",
      "'Rank' = Best PSO Rank solution. 'Best' = Best PT+DRT Mode Shift solution."
    )
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.placement = "outside",
    strip.text.x = element_text(size = 7),
    strip.text.y = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  ) +
  guides(fill = guide_legend(ncol = 2))

ggsave(
  file.path(plot_dir, "fig_4_3_combined_catchment_fleet.png"),
  plot_combined_fleet,
  width = 20,
  height = 12,
  dpi = 300
)
message("✓ Combined catchment with fleet footnotes saved")


# -------------------------
# 5.5 Fleet Bar Plots by Interval (NEW PROPOSED PLOT)
# -------------------------

message("Creating fleet bar plots by interval...")

# Helper: Create fleet interval plot for a set of solution IDs
create_fleet_interval_plot <- function(target_ids, suffix_label) {
  # Join to combined fleet data (all intervals)
  fleet_data <- target_ids |>
    inner_join(
      combined_fleet,
      by = c("objective", "solution_id" = "solution_id")
    )

  # If solution_id column name differs, try matching via solution name
  if (nrow(fleet_data) == 0) {
    # Match via solution name pattern
    fleet_data <- target_ids |>
      mutate(
        solution_pattern = paste0(
          "combined_solution_",
          sprintf("%02d", solution_id)
        )
      ) |>
      inner_join(
        combined_fleet |> rename(solution_name = solution),
        by = c("objective", "solution_pattern" = "solution_name")
      )
  }

  fleet_data <- fleet_data |>
    mutate(
      objective_clean = objective_labels_short[objective],
      # Sort intervals correctly
      start_hour = as.numeric(str_extract(interval_label, "^\\d+")),
      interval_label = fct_reorder(interval_label, start_hour)
    )

  # Create stacked bar: bus_fleet_solution + drt_fleet_solution stacked
  fleet_long <- fleet_data |>
    select(
      objective_clean,
      interval_label,
      bus_fleet_base,
      bus_fleet_solution,
      drt_fleet_solution,
      total_fleet_pct_change
    ) |>
    pivot_longer(
      cols = c(bus_fleet_solution, drt_fleet_solution),
      names_to = "fleet_type",
      values_to = "fleet_size"
    ) |>
    mutate(
      fleet_type = case_when(
        fleet_type == "bus_fleet_solution" ~ "Bus",
        fleet_type == "drt_fleet_solution" ~ "DRT"
      ),
      fleet_type = factor(fleet_type, levels = c("DRT", "Bus"))
    )

  # Base line data (one value per interval per objective)
  base_line_data <- fleet_data |>
    distinct(objective_clean, interval_label, bus_fleet_base)

  # Label: total fleet % change at top of stacked bar
  label_data <- fleet_data |>
    mutate(
      total_fleet = bus_fleet_solution + drt_fleet_solution,
      label = paste0(sprintf("%+.0f", total_fleet_pct_change), "%")
    ) |>
    select(objective_clean, interval_label, total_fleet, label)

  ggplot() +
    # Stacked bars: Bus (bottom) + DRT (top)
    geom_col(
      data = fleet_long,
      aes(x = interval_label, y = fleet_size, fill = fleet_type),
      position = "stack",
      width = 0.7
    ) +
    # Base fleet line
    geom_segment(
      data = base_line_data,
      aes(
        x = as.numeric(interval_label) - 0.45,
        xend = as.numeric(interval_label) + 0.45,
        y = bus_fleet_base,
        yend = bus_fleet_base
      ),
      color = "black",
      linewidth = 0.8,
      linetype = "solid"
    ) +
    # Total fleet % change label
    geom_text(
      data = label_data,
      aes(x = interval_label, y = total_fleet + 20, label = label),
      size = 2.8,
      fontface = "bold",
      color = "gray30"
    ) +
    facet_wrap(~objective_clean, nrow = 2, scales = "free_y") +
    scale_fill_manual(
      values = c("Bus" = "#4393c3", "DRT" = "#d6604d"),
      name = "Fleet Component"
    ) +
    labs(
      title = glue::glue("Fleet Composition by Time Interval — {suffix_label}"),
      subtitle = "Black line = baseline bus fleet. Labels = total fleet % change vs baseline.",
      x = "Time Interval",
      y = "Fleet Size (vehicles)",
      caption = glue::glue(
        "Base bus fleet at peak ({peak_interval}h): {base_fleet_peak} vehicles.\n",
        "Total fleet = Bus + DRT. % change relative to bus-only baseline."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
    )
}

# Plot for Best PSO Rank solutions
plot_fleet_rank <- create_fleet_interval_plot(ids_rank0, "Best PSO Rank")
ggsave(
  file.path(plot_dir, "fig_5_1a_fleet_by_interval_rank.png"),
  plot_fleet_rank,
  width = 16,
  height = 10,
  dpi = 300
)

# Plot for Best Mode Shift solutions
plot_fleet_best <- create_fleet_interval_plot(ids_best_share, "Best Mode Shift")
ggsave(
  file.path(plot_dir, "fig_5_1a_fleet_by_interval_best_share.png"),
  plot_fleet_best,
  width = 16,
  height = 10,
  dpi = 300
)

message("✓ Fleet interval bar plots saved")


# -------------------------
# 5.5b Combined Fleet Interval Plot (Side-by-Side)
# -------------------------

message("Creating combined fleet interval plot...")

# 1. Helper to get data for a specific type
get_fleet_plot_data <- function(target_ids, type_label) {
  # Join to combined fleet data (all intervals)
  fleet_data <- target_ids |>
    inner_join(
      combined_fleet,
      by = c("objective", "solution_id" = "solution_id")
    )

  if (nrow(fleet_data) == 0) {
    return(NULL)
  }

  fleet_data |>
    mutate(
      objective_clean = objective_labels_short[objective],
      start_hour = as.numeric(str_extract(interval_label, "^\\d+")),
      interval_label = fct_reorder(interval_label, start_hour),
      Type = type_label
    )
}

# 2. Combine Data
combined_fleet_plot_data <- bind_rows(
  get_fleet_plot_data(ids_rank0, "Best PSO Rank"),
  get_fleet_plot_data(ids_best_share, "Best Real Share")
)

write_csv(
  combined_fleet_plot_data,
  file.path(plot_dir, "tables/table_4_4_combined_fleet_data.csv")
)

# 3. Pivot Longer for Stacking (Bus vs DRT)
combined_fleet_long <- combined_fleet_plot_data |>
  select(
    objective_clean,
    interval_label,
    Type,
    bus_fleet_base,
    bus_fleet_solution,
    drt_fleet_solution,
    total_fleet_pct_change
  ) |>
  pivot_longer(
    cols = c(bus_fleet_solution, drt_fleet_solution),
    names_to = "fleet_type",
    values_to = "fleet_size"
  ) |>
  mutate(
    fleet_type = case_when(
      fleet_type == "bus_fleet_solution" ~ "Bus",
      fleet_type == "drt_fleet_solution" ~ "DRT"
    ),
    # Stack order: Bus bottom, DRT top
    fleet_type = factor(fleet_type, levels = c("Bus", "DRT"))
  )

# 4. Manual Dodging Logic (Interval on X, shift left/right by Type)
interval_levels <- levels(combined_fleet_long$interval_label)
bar_width <- 0.35
offset <- 0.2

combined_fleet_pos <- combined_fleet_long |>
  mutate(
    x_base = as.numeric(factor(interval_label, levels = interval_levels)),
    x_pos = if_else(
      Type == "Best Real Share",
      x_base + offset,
      x_base - offset
    ),
    Fill_Group = paste(fleet_type, Type, sep = " - "),

    # 1. Set Factor Levels (Controls Legend Order)
    #    "Bus" first implies it is the "first" category
    Fill_Group = factor(
      Fill_Group,
      levels = c(
        "Bus - Best PSO Rank",
        "Bus - Best Real Share",
        "DRT - Best PSO Rank",
        "DRT - Best Real Share"
      )
    ),

    # 2. Helper column for sorting (Bus=1, DRT=2)
    sort_order = if_else(fleet_type == "Bus", 1, 2)
  ) |>
  # 3. CRITICAL: Sort the data so Bus rows are processed first (Bottom of stack)
  arrange(objective_clean, interval_label, Type, sort_order)

# 5. Base Line Data (Needs same manual dodging logic if we want lines to split)
# Alternatively, since base fleet is identical for both, we can just center it.
# Let's simple center it on the interval tick.
base_line_data <- combined_fleet_plot_data |>
  distinct(objective_clean, interval_label, bus_fleet_base) |>
  mutate(
    x_base = as.numeric(factor(interval_label, levels = interval_levels))
  )

# 6. Labels (Total change per bar)
label_data_combined <- combined_fleet_plot_data |>
  mutate(
    total_fleet = bus_fleet_solution + drt_fleet_solution,
    label = paste0(sprintf("%+.0f", total_fleet_pct_change), "%"),
    x_base = as.numeric(factor(interval_label, levels = interval_levels)),
    x_pos = if_else(
      Type == "Best Real Share",
      x_base + offset,
      x_base - offset
    )
  )

# 7. Colors (Light for Rank, Solid for Share)
# Bus = Blueish, DRT = Reddish
fleet_colors_combined <- c(
  "Bus - Best Real Share" = "#4393c3", # Solid Blue
  "Bus - Best PSO Rank" = "#92c5de", # Light Blue
  "DRT - Best Real Share" = "#d6604d", # Solid Red
  "DRT - Best PSO Rank" = "#f4a582" # Light Red
)

# 8. Plot
plot_combined_fleet_interval <- ggplot() +
  # Stacked columns
  geom_col(
    data = combined_fleet_pos,
    aes(x = x_pos, y = fleet_size, fill = Fill_Group),
    width = bar_width,
    position = position_stack(reverse = TRUE)
  ) +
  # Base fleet line (centered, covering both bars)
  geom_segment(
    data = base_line_data,
    aes(
      x = x_base - 0.45,
      xend = x_base + 0.45,
      y = bus_fleet_base,
      yend = bus_fleet_base
    ),
    color = "black",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  # Labels
  geom_text(
    data = label_data_combined,
    aes(x = x_pos, y = total_fleet + 25, label = label),
    size = 2.2,
    original = FALSE,
    color = "gray20"
  ) +
  facet_wrap(~objective_clean, nrow = 2, scales = "free_y") +
  scale_fill_manual(
    values = fleet_colors_combined,
    name = "",
    labels = c(
      "Bus - Best Real Share" = "Bus - Best Mode Shift",
      "Bus - Best PSO Rank" = "Bus - Best PSO Rank",
      "DRT - Best Real Share" = "DRT - Best Mode Shift",
      "DRT - Best PSO Rank" = "DRT - Best PSO Rank"
    )
  ) +
  scale_x_continuous(
    breaks = seq_along(interval_levels),
    labels = interval_levels
  ) +
  labs(
    title = "Fleet Composition by Time Interval (Side-by-Side Comparison)",
    subtitle = "Light bars = Best PSO Rank | Solid bars = Best Mode Shift",
    x = "Time Interval",
    y = "Fleet Size (vehicles)",
    caption = "Dashed line = baseline bus fleet. Labels = Total fleet % change vs baseline bus."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  ) +
  guides(fill = guide_legend(ncol = 2))

ggsave(
  file.path(plot_dir, "fig_5_1_combined_fleet_interval.png"),
  plot_combined_fleet_interval,
  width = 16,
  height = 10,
  dpi = 300
)

message("✓ Combined fleet interval plot saved")


# -------------------------
# 5.6 Fleet Efficiency Summary (NEW PROPOSED PLOT)
# -------------------------

message("Creating fleet efficiency summary...")

# For each objective, show the mode share gain per unit of fleet change
# This shows "bang for buck" in terms of fleet resources
efficiency_data <- fleet_peak |>
  left_join(
    res_mode_share |>
      filter(
        mode == "pt+drt",
        level == "trip",
        access == "origin+destination",
        zones == "pt+drt"
      ) |>
      select(objective, solution_id, share_delta),
    by = c("objective", "solution_id")
  ) |>
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  efficiency_data,
  aes(x = total_fleet_pct_change, y = share_delta, color = objective_clean)
) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~objective_clean, ncol = 3) +
  labs(
    title = "Fleet Efficiency: Mode Share Gain vs Fleet Size Change",
    subtitle = glue::glue(
      "Peak interval ({peak_interval}h). Base fleet: {base_fleet_peak}"
    ),
    x = "Total Fleet % Change (Bus + DRT vs Baseline Bus)",
    y = "PT+DRT Mode Share Change (pp)",
    caption = "Upper-left quadrant = ideal (mode share gain with fleet reduction)."
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  file.path(plot_dir, "fig_5_2_fleet_efficiency.png"),
  width = 16,
  height = 10,
  dpi = 300
)
message("✓ Fleet efficiency summary saved")


message("\nAnalysis Complete.")

##########################################################################
# SUMMARY OUTPUT
##########################################################################

message("\n==========================================")
message("ANALYSIS COMPLETE")
message("==========================================\n")

message("Tables saved to: R/plots/transit_opt_paper/tables/")
message("  - table_4_1_correlations_all.csv (full correlation data)")
message("  - table_4_1_correlation_matrix.csv (objectives x filters)")
message("  - table_4_1_topk_recall.csv (top-k recall data)")
message("  - table_4_1_solution_rankings_all.csv (all solutions with ranks)")
message("  - table_4_1_top_solutions_summary.csv (top N per metric)")
message("  - table_4_1_top_solutions_comparison.csv (overlap analysis)")
message("  - table_4_2a_scenario_comparison_best_rank.csv (best by PSO rank)")
message(
  "  - table_4_2b_scenario_comparison_best_pt_drt.csv (best by PT+DRT share)"
)
message("  - table_4_3_catchment_sensitivity.csv")

message("\nPlots saved to: R/plots/transit_opt_paper/")
message("  - fig_4_1a_correlation_heatmap_faceted.png")
message("  - fig_4_1b_correlation_heatmap_outcomes.png")
message("  - fig_4_1b_correlation_heatmap_outcomes_no_filter.png")
message("  - fig_4_1c_proxy_vs_mode_share.png")
message("  - fig_4_1c_proxy_vs_mode_share_no_filter.png")
message("  - fig_4_1e_topk_recall_by_k.png")
message("  - fig_4_1e_topk_recall_by_k_no_filter.png")
message("  - fig_4_1f_topk_recall_simple.png")
message("  - fig_4_1f_topk_recall_simple_no_filter.png")
message("  - fig_4_2a_pt_car_tradeoff.png")
message("  - fig_4_2b_vkm_vs_mode_share.png")
message("  - fig_4_3_catchment_comparison_bar.png")
message("  - fig_4_3x_global_summary_horizontal.png")
message("  - fig_4_3x_global_summary_grouped.png")
message("  - fig_4_2a_pt_car_tradeoff_fleet.png")
message("  - fig_4_2b_vkm_vs_mode_share_fleet.png")
message("  - fig_4_3_combined_catchment_fleet.png")
message("  - fig_5_1a_fleet_by_interval_rank.png")
message("  - fig_5_1a_fleet_by_interval_best_share.png")
message("  - fig_5_2_fleet_efficiency.png")

message("\nNote: Spatial/temporal plots are in plot_maps.R")
