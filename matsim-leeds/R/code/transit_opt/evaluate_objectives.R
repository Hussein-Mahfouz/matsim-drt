library(tidyverse)

##########
# SECTION 0: Reading in data
##########

message("\n==========================================")
message("LOADING DATA")
message("==========================================\n")

## PSO results
obj_base <- read_csv("../../transit_opt/output/base_objective_values.csv") |>
  rename(baseline_objective_value_pen = penalized_objective_value)

obj_pso <- read_csv("R/output/pso_objective_values.csv")

## MATSim results
res_mode_share <- read_csv("R/output/mode_share_by_objective.csv")
res_vkm <- read_csv("R/output/vkm_by_objective.csv")

## DRT fleet deployments
all_drt_deployments <- read_csv("R/output/drt_fleet_deployments.csv")


##########
# SECTION 0a: Configuration - Filter objectives
##########

# ===== USER CONFIGURATION =====
# Objective filtering
OBJECTIVES_TO_INCLUDE <- NULL
OBJECTIVES_TO_EXCLUDE <- "^sc_"

# Correlation analysis: Maximum solution rank to include (NULL = use all)
# Solutions with rank > this value will be excluded from correlation analysis
# Motivation: High-rank solutions (e.g., 1023, 2047) are essentially random
MAX_SOLUTION_RANK_FOR_CORRELATION <- NULL # Only use top 127 solutions (or set to NULL for all)

# Top-k Recall configuration
TOPK_HEATMAP_K_CONFIG <- 5
TOPK_LINE_PLOT_MAX_K_CONFIG <- 5
# ==============================

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
    rank = first(rank),
    swarm_id = first(swarm_id),
    objective_sol = first(objective_sol),
    generation_found = first(generation_found),
    violations = first(violations),
    baseline_objective_value_pen = first(baseline_objective_value_pen),
    pso_pct_change_vs_base = first(pso_pct_change_vs_base),
    pso_frac_of_base = first(pso_frac_of_base),
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
    rank = first(rank),
    swarm_id = first(swarm_id),
    objective_sol = first(objective_sol),
    generation_found = first(generation_found),
    violations = first(violations),
    baseline_objective_value_pen = first(baseline_objective_value_pen),
    pso_pct_change_vs_base = first(pso_pct_change_vs_base),
    pso_frac_of_base = first(pso_frac_of_base),
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
    rank = first(rank),
    swarm_id = first(swarm_id),
    objective_sol = first(objective_sol),
    generation_found = first(generation_found),
    violations = first(violations),
    baseline_objective_value_pen = first(baseline_objective_value_pen),
    pso_pct_change_vs_base = first(pso_pct_change_vs_base),
    pso_frac_of_base = first(pso_frac_of_base),
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
  "wt_int_tot" = "Wait Time\nInterval Total",
  "wt_int_var" = "Wait Time\nInterval Variance",
  "wt_sum_tot" = "Wait Time\nSum Total",
  "wt_sum_var" = "Wait Time\nSum Variance",
  "wt_peak_tot" = "Wait Time\nPeak Total",
  "wt_peak_var" = "Wait Time\nPeak Variance"
)

objective_labels_short <- c(
  "sc_avg_var" = "SC-Avg-Var",
  "sc_int_var" = "SC-Int-Var",
  "sc_peak_var" = "SC-Peak-Var",
  "sc_sum_var" = "SC-Sum-Var",
  "wt_avg_tot" = "WT-Avg-Tot",
  "wt_avg_var" = "WT-Avg-Var",
  "wt_int_tot" = "WT-Int-Tot",
  "wt_int_var" = "WT-Int-Var",
  "wt_sum_tot" = "WT-Sum-Tot",
  "wt_sum_var" = "WT-Sum-Var",
  "wt_peak_tot" = "WT-Peak-Tot",
  "wt_peak_var" = "WT-Peak-Var"
)

if (!is.null(OBJECTIVES_TO_INCLUDE)) {
  objective_labels <- objective_labels[
    names(objective_labels) %in% OBJECTIVES_TO_INCLUDE
  ]
  objective_labels_short <- objective_labels_short[
    names(objective_labels_short) %in% OBJECTIVES_TO_INCLUDE
  ]
} else if (!is.null(OBJECTIVES_TO_EXCLUDE)) {
  objective_labels <- objective_labels[
    !str_detect(names(objective_labels), OBJECTIVES_TO_EXCLUDE)
  ]
  objective_labels_short <- objective_labels_short[
    !str_detect(names(objective_labels_short), OBJECTIVES_TO_EXCLUDE)
  ]
}

level_labels <- c("trip" = "Trip", "person" = "Person", "all" = "All")
access_labels <- c("origin" = "O", "origin+destination" = "O+D", "all" = "All")
zones_labels <- c("pt" = "PT", "pt+drt" = "PT+DRT", "all" = "All")

main_modes <- c("pt+drt", "car", "walk", "bike", "taxi")

dir.create("R/plots/transit_opt_paper", showWarnings = FALSE, recursive = TRUE)
dir.create(
  "R/plots/transit_opt_paper/tables",
  showWarnings = FALSE,
  recursive = TRUE
)

##########################################################################
# SECTION 4.1: VALIDATION - Correlation Analysis (with Filter Sensitivity)
##########################################################################

message("\n==========================================")
message("SECTION 4.1: VALIDATION - Correlation Analysis")
message("==========================================\n")

# Apply solution rank filter for correlation analysis
if (!is.null(MAX_SOLUTION_RANK_FOR_CORRELATION)) {
  message(glue::glue(
    "Filtering to solutions with rank ≤ {MAX_SOLUTION_RANK_FOR_CORRELATION} for correlation analysis"
  ))

  res_mode_share_corr <- res_mode_share |>
    filter(solution_id <= MAX_SOLUTION_RANK_FOR_CORRELATION)

  res_vkm_extended_corr <- res_vkm_extended |>
    filter(solution_id <= MAX_SOLUTION_RANK_FOR_CORRELATION)

  n_solutions_used <- n_distinct(res_mode_share_corr$solution_id)
  message(glue::glue(
    "Using {n_solutions_used} solutions for correlation analysis"
  ))
} else {
  message("Using all solutions for correlation analysis")
  res_mode_share_corr <- res_mode_share
  res_vkm_extended_corr <- res_vkm_extended
}

# Define all filter combinations
# Note: "all" only appears as "All | All | All" (not mixed with other filters)
filter_combinations <- bind_rows(
  # All combinations of trip/person × origin/origin+destination × pt/pt+drt
  expand_grid(
    level = c("trip", "person"),
    access = c("origin", "origin+destination"),
    zones = c("pt", "pt+drt")
  ),
  # Add the single "all" combination
  tibble(
    level = "all",
    access = "all",
    zones = "all"
  )
) |>
  mutate(
    filter_label = paste(
      level_labels[level],
      access_labels[access],
      zones_labels[zones],
      sep = " | "
    )
  )

# Function to calculate correlation with proper Spearman p-value
calc_correlation <- function(data, x_var, y_var) {
  x <- data[[x_var]]
  y <- data[[y_var]]

  # Remove NAs
  complete_idx <- !is.na(x) & !is.na(y)
  x <- x[complete_idx]
  y <- y[complete_idx]

  n <- length(x)

  if (n < 3) {
    return(tibble(
      n_solutions = n,
      cor_spearman = NA_real_,
      cor_pearson = NA_real_,
      p_value_spearman = NA_real_,
      p_value_pearson = NA_real_
    ))
  }

  # Spearman correlation and p-value
  spearman_test <- tryCatch(
    cor.test(x, y, method = "spearman", exact = FALSE),
    error = function(e) NULL
  )

  # Pearson correlation and p-value
  pearson_test <- tryCatch(
    cor.test(x, y, method = "pearson"),
    error = function(e) NULL
  )

  tibble(
    n_solutions = n,
    cor_spearman = if (!is.null(spearman_test)) {
      spearman_test$estimate
    } else {
      NA_real_
    },
    cor_pearson = if (!is.null(pearson_test)) {
      pearson_test$estimate
    } else {
      NA_real_
    },
    p_value_spearman = if (!is.null(spearman_test)) {
      spearman_test$p.value
    } else {
      NA_real_
    },
    p_value_pearson = if (!is.null(pearson_test)) {
      pearson_test$p.value
    } else {
      NA_real_
    }
  )
}

# Correlation: PSO objective vs PT+DRT mode share change (all combinations)
correlation_mode_share <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share_corr |>
      filter(
        mode == "pt+drt",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(share_delta)
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "share_delta")) |>
      ungroup() |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        filter_label = filter_label,
        outcome = "PT+DRT Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Correlation: PSO objective vs Car mode share change (all combinations)
correlation_car <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_mode_share_corr |>
      filter(
        mode == "car",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(share_delta)
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "share_delta")) |>
      ungroup() |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        filter_label = filter_label,
        outcome = "Car Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Correlation: PSO objective vs Car+Taxi VKM change (all combinations)
correlation_vkm <- filter_combinations |>
  pmap_dfr(function(level, access, zones, filter_label) {
    res_vkm_extended_corr |>
      filter(
        mode == "car+taxi",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(delta_km)
      ) |>
      group_by(objective) |>
      group_modify(~ calc_correlation(.x, "pso_frac_of_base", "delta_km")) |>
      ungroup() |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        filter_label = filter_label,
        outcome = "Car+Taxi VKM",
        objective_clean = objective_labels_short[objective]
      )
  })

# Combine all correlations (no need to add "all" separately - it's already included)
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

# Save full correlation table
write_csv(
  all_correlations,
  "R/plots/transit_opt_paper/tables/table_4_1_correlations_all.csv"
)

# -------------------------
# Table 1: Correlation Matrix (objectives x filter combinations)
# -------------------------

correlation_table_wide <- all_correlations |>
  filter(outcome == "PT+DRT Mode Share") |>
  select(objective_clean, filter_label, cor_spearman, significance) |>
  mutate(
    cor_display = paste0(round(cor_spearman, 2), significance)
  ) |>
  select(objective_clean, filter_label, cor_display) |>
  pivot_wider(
    names_from = filter_label,
    values_from = cor_display
  )

write_csv(
  correlation_table_wide,
  "R/plots/transit_opt_paper/tables/table_4_1_correlation_matrix.csv"
)

# -------------------------
# Plot 1: Faceted Multi-outcome Heatmap (by filter combination)
# -------------------------

faceted_heatmap_data <- all_correlations |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    ),
    filter_label = factor(filter_label)
  )

ggplot(
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
    subtitle = "By catchment definition (Aggregation | Access | Zones)",
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
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30"),
    legend.position = "right",
    panel.grid = element_blank()
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_1a_correlation_heatmap_faceted.png",
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
  "R/plots/transit_opt_paper/fig_4_1b_correlation_heatmap_outcomes.png",
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
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_1b_correlation_heatmap_outcomes_no_filter.png",
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
  facet_wrap(~objective_clean, scales = "free_x", ncol = 4) +
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
  "R/plots/transit_opt_paper/fig_4_1c_proxy_vs_mode_share.png",
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
  facet_wrap(~objective_clean, scales = "free_x", ncol = 4) +
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
  "R/plots/transit_opt_paper/fig_4_1c_proxy_vs_mode_share_no_filter.png",
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
  "R/plots/transit_opt_paper/tables/table_4_1_topk_recall.csv"
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
  "R/plots/transit_opt_paper/fig_4_1e_topk_recall_by_k.png",
  width = 12,
  height = 14,
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
  "R/plots/transit_opt_paper/fig_4_1e_topk_recall_by_k_no_filter.png",
  width = 12,
  height = 14,
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
  "R/plots/transit_opt_paper/fig_4_1f_topk_recall_simple.png",
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
  "R/plots/transit_opt_paper/fig_4_1f_topk_recall_simple_no_filter.png",
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
  "R/plots/transit_opt_paper/tables/table_4_1_solution_rankings_all.csv"
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
  "R/plots/transit_opt_paper/tables/table_4_1_top_solutions_summary.csv"
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
  "R/plots/transit_opt_paper/tables/table_4_1_top_solutions_comparison.csv"
)


##########################################################################
# SECTION 4.2: SYSTEM PERFORMANCE COMPARISON
##########################################################################

message("\n==========================================")
message("SECTION 4.2: SYSTEM PERFORMANCE COMPARISON")
message("==========================================\n")

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

# Add DRT fleet data (peak interval: 8-12h)
best_drt_fleet <- all_drt_deployments |>
  filter(
    str_detect(solution, "_00$"),
    interval_label == "8-12"
  ) |>
  group_by(objective) |>
  summarise(
    drt_fleet_total = sum(fleet_size),
    drt_fleet_ne = sum(fleet_size[scenario == "drtNE"]),
    drt_fleet_nw = sum(fleet_size[scenario == "drtNW"]),
    .groups = "drop"
  )

# Combine into Table 2
table_4_2_scenario_comparison <- best_solutions_summary |>
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
    drt_fleet_total
  ) |>
  rename(
    Objective = objective_clean,
    `PT+DRT Share Δ (pp)` = `pt+drt_share_delta`,
    `Car Share Δ (pp)` = car_share_delta,
    `PT+DRT VKM Δ (km)` = `pt+drt_delta_km`,
    `Car+Taxi VKM Δ (km)` = `car+taxi_delta_km`,
    `DRT Fleet (8-12h)` = drt_fleet_total
  )

write_csv(
  table_4_2_scenario_comparison,
  "R/plots/transit_opt_paper/tables/table_4_2_scenario_comparison.csv"
)

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
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  tradeoff_data,
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
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
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
  "R/plots/transit_opt_paper/fig_4_2a_pt_car_tradeoff.png",
  width = 16,
  height = 10,
  dpi = 300
)

# -------------------------
# Plot: VKM efficiency
# -------------------------

vkm_mode_tradeoff <- res_vkm_extended |>
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
  vkm_mode_tradeoff,
  aes(x = pt_vkm_change / 1000, y = pt_share_delta, color = pso_frac_of_base)
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
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
  "R/plots/transit_opt_paper/fig_4_2b_vkm_vs_mode_share.png",
  width = 16,
  height = 10,
  dpi = 300
)

##########################################################################
# SECTION 4.3: CATCHMENT SENSITIVITY ANALYSIS
##########################################################################

message("\n==========================================")
message("SECTION 4.3: CATCHMENT SENSITIVITY ANALYSIS")
message("==========================================\n")

# Table 3: Mode share change by catchment type (best solutions only)
catchment_sensitivity_summary <- res_mode_share |>
  filter(
    mode == "pt+drt",
    solution_id == 0
  ) |>
  mutate(
    objective_clean = objective_labels_short[objective],
    filter_label = paste(
      level_labels[level],
      access_labels[access],
      zones_labels[zones],
      sep = " | "
    )
  ) |>
  select(objective_clean, filter_label, share_delta)

table_4_3_catchment <- catchment_sensitivity_summary |>
  pivot_wider(
    names_from = filter_label,
    values_from = share_delta
  )

write_csv(
  table_4_3_catchment,
  "R/plots/transit_opt_paper/tables/table_4_3_catchment_sensitivity.csv"
)

# -------------------------
# Plot: Bar chart comparing filter configurations
# -------------------------

best_catchment_comparison <- res_mode_share |>
  filter(
    mode == "pt+drt",
    solution_id == 0
  ) |>
  mutate(
    filter_label = paste(
      level_labels[level],
      access_labels[access],
      zones_labels[zones],
      sep = " | "
    ),
    objective_clean = factor(
      objective,
      levels = names(objective_labels_short),
      labels = objective_labels_short
    )
  )

ggplot(
  best_catchment_comparison,
  aes(x = filter_label, y = share_delta, fill = zones)
) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
  labs(
    title = "PT+DRT Mode Share Change by Catchment Definition",
    subtitle = "Best solutions (rank 0) only",
    x = "Filter Configuration (Aggregation | Access | Zones)",
    y = "Mode Share Change (pp)",
    fill = "Catchment"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_3_catchment_comparison_bar.png",
  width = 16,
  height = 10,
  dpi = 300
)


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
message("  - table_4_2_scenario_comparison.csv")
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

message("\nNote: Spatial/temporal plots are in plot_maps.R")
