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

# Set to NULL to include all objectives, or specify a vector of objectives to KEEP
# Example: c("wt_avg_tot", "wt_avg_var") to keep only these
# Example: NULL to keep all objectives
OBJECTIVES_TO_INCLUDE <- NULL

# Alternatively, set objectives to EXCLUDE (ignored if OBJECTIVES_TO_INCLUDE is not NULL)
# Example: c("sc_avg_var", "sc_int_var") to exclude these
# Example: "^sc_" to exclude all objectives starting with "sc_"
OBJECTIVES_TO_EXCLUDE <- "^sc_" # Exclude all Service Coverage objectives

# Apply the filter
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
    pso_frac_of_base = round(objective_sol / baseline_objective_value_pen, 2)
  )

# Join to mode share and VKM
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

# PT+DRT combined for mode share
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

# PT+DRT and Car+Taxi combined for VKM

# PT+DRT combined for VKM - FIXED: carry over PSO columns
pt_drt_vkm_combined <- res_vkm |>
  filter(mode == "pt" | str_detect(mode, "^drt")) |>
  group_by(objective, solution, solution_id, level, access, zones) |>
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    # Carry over PSO columns
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

# Car+Taxi combined for VKM - FIXED: carry over PSO columns
car_taxi_combined <- res_vkm |>
  filter(mode %in% c("car", "taxi")) |>
  group_by(objective, solution, solution_id, level, access, zones) |>
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    # Carry over PSO columns
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

# Also filter the labels to match
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

# Create output directory
dir.create("R/plots/transit_opt_paper", showWarnings = FALSE, recursive = TRUE)
dir.create(
  "R/tables/transit_opt_paper/tables/",
  showWarnings = FALSE,
  recursive = TRUE
)
##########################################################################
# SECTION 4.1: VALIDATION - Correlation Analysis
##########################################################################

message("\n==========================================")
message("SECTION 4.1: VALIDATION - Correlation Analysis")
message("==========================================\n")

# Calculate correlation for ALL filter combinations
# This shows how correlation varies by catchment definition

# Define all filter combinations
filter_combinations <- expand_grid(
  level = c("trip", "person"),
  access = c("origin", "origin+destination"),
  zones = c("pt", "pt+drt")
)

# Correlation: PSO objective vs PT+DRT mode share change (all combinations)
correlation_mode_share <- filter_combinations |>
  pmap_dfr(function(level, access, zones) {
    res_mode_share |>
      filter(
        mode == "pt+drt",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(share_delta)
      ) |>
      group_by(objective) |>
      summarise(
        n_solutions = n(),
        cor_spearman = cor(
          pso_frac_of_base,
          share_delta,
          method = "spearman",
          use = "complete.obs"
        ),
        cor_pearson = cor(
          pso_frac_of_base,
          share_delta,
          method = "pearson",
          use = "complete.obs"
        ),
        p_value = tryCatch(
          cor.test(pso_frac_of_base, share_delta, method = "spearman")$p.value,
          error = function(e) NA_real_
        ),
        .groups = "drop"
      ) |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        outcome = "PT+DRT Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Correlation: PSO objective vs Car mode share change (all combinations)
correlation_car <- filter_combinations |>
  pmap_dfr(function(level, access, zones) {
    res_mode_share |>
      filter(
        mode == "car",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(share_delta)
      ) |>
      group_by(objective) |>
      summarise(
        n_solutions = n(),
        cor_spearman = cor(
          pso_frac_of_base,
          share_delta,
          method = "spearman",
          use = "complete.obs"
        ),
        cor_pearson = cor(
          pso_frac_of_base,
          share_delta,
          method = "pearson",
          use = "complete.obs"
        ),
        p_value = tryCatch(
          cor.test(pso_frac_of_base, share_delta, method = "spearman")$p.value,
          error = function(e) NA_real_
        ),
        .groups = "drop"
      ) |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        outcome = "Car Mode Share",
        objective_clean = objective_labels_short[objective]
      )
  })

# Correlation: PSO objective vs Car+Taxi VKM change (all combinations)
correlation_vkm <- filter_combinations |>
  pmap_dfr(function(level, access, zones) {
    res_vkm_extended |>
      filter(
        mode == "car+taxi",
        level == !!level,
        access == !!access,
        zones == !!zones,
        !is.na(pso_frac_of_base),
        !is.na(delta_km)
      ) |>
      group_by(objective) |>
      summarise(
        n_solutions = n(),
        cor_spearman = cor(
          pso_frac_of_base,
          delta_km,
          method = "spearman",
          use = "complete.obs"
        ),
        cor_pearson = cor(
          pso_frac_of_base,
          delta_km,
          method = "pearson",
          use = "complete.obs"
        ),
        p_value = tryCatch(
          cor.test(pso_frac_of_base, delta_km, method = "spearman")$p.value,
          error = function(e) NA_real_
        ),
        .groups = "drop"
      ) |>
      mutate(
        level = level,
        access = access,
        zones = zones,
        outcome = "Car+Taxi VKM",
        objective_clean = objective_labels_short[objective]
      )
  })

# Combine all correlations
all_correlations <- bind_rows(
  correlation_mode_share,
  correlation_car,
  correlation_vkm
) |>
  mutate(
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    filter_config = paste(level, access, zones, sep = " | ")
  )

# Save full correlation table
write_csv(
  all_correlations,
  "R/tables/transit_opt_paper/tables/table_4_1_correlations_all.csv"
)

# Plot 1: Heatmap for ONE filter config (for paper figure)
# Use trip | origin+destination | pt+drt as the main result
main_filter_correlations <- all_correlations |>
  filter(
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  )

correlation_plot_data <- main_filter_correlations |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    outcome = factor(
      outcome,
      levels = c("PT+DRT Mode Share", "Car Mode Share", "Car+Taxi VKM")
    )
  )

ggplot(
  correlation_plot_data,
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
    subtitle = "Filter: Trip-level | Origin+Destination | PT+DRT catchment",
    x = "MATSim Outcome",
    y = "Proxy Objective",
    caption = "Negative correlation expected: lower proxy value should improve outcomes\n* p<0.05, ** p<0.01, *** p<0.001"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_1_correlation_heatmap.png",
  width = 10,
  height = 8,
  dpi = 300
)

# Plot 2: Correlation by filter configuration (shows sensitivity)
correlation_sensitivity <- all_correlations |>
  filter(outcome == "PT+DRT Mode Share") |>
  mutate(
    objective_clean = factor(objective_clean, levels = objective_labels_short),
    filter_config = factor(filter_config)
  )

ggplot(
  correlation_sensitivity,
  aes(x = filter_config, y = cor_spearman, fill = cor_spearman)
) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~objective_clean, ncol = 4) +
  scale_fill_gradient2(
    low = "#d73027",
    mid = "white",
    high = "#1a9850",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Correlation Sensitivity to Catchment Definition",
    subtitle = "Spearman correlation: Proxy Objective vs. PT+DRT Mode Share Change",
    x = "Filter Configuration (Level | Access | Zones)",
    y = "Spearman Correlation",
    caption = "Negative correlation = better proxy (lower proxy → higher mode share)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_1_correlation_sensitivity.png",
  width = 16,
  height = 10,
  dpi = 300
)

# Plot 3: Scatter plots for main filter config
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
  geom_point(aes(color = solution_id), size = 2, alpha = 0.7) +
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
    subtitle = "Each point is a solution; lower proxy value = better optimization",
    x = "Proxy Objective (fraction of baseline)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Solution\nRank",
    caption = "Dashed line shows linear trend. Good proxies should show negative correlation."
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
  "R/plots/transit_opt_paper/fig_4_1_proxy_vs_mode_share.png",
  width = 16,
  height = 10,
  dpi = 300
)

##########################################################################
# SECTION 4.2: SYSTEM PERFORMANCE COMPARISON
##########################################################################

message("\n==========================================")
message("SECTION 4.2: SYSTEM PERFORMANCE COMPARISON")
message("==========================================\n")

# Table 2: Compare Baseline vs Best Solution per Objective

# Get best solution (rank 0) for each objective
best_solutions_summary <- res_mode_share |>
  filter(
    solution_id == 0, # Best solution
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
    str_detect(solution, "_00$"), # Best solution
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
    # Mode share changes
    `pt+drt_share_delta`,
    car_share_delta,
    # VKM changes
    `pt+drt_delta_km`,
    `car+taxi_delta_km`,
    # DRT fleet
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
  "R/tables/transit_opt_paper/tables/table_4_2_scenario_comparison.csv"
)

# Plot: Trade-off between PT+DRT gain and Car reduction
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
    subtitle = "Trip-level | Origin+Destination | PT+DRT catchment",
    x = "Car Mode Share Change (pp)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Proxy Value\n(× baseline)",
    caption = "Diagonal: 1:1 substitution. Lower-left quadrant = desirable (PT gain, car loss).\nDarker colors = better proxy optimization."
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
  "R/plots/transit_opt_paper/fig_4_2_pt_car_tradeoff.png",
  width = 16,
  height = 10,
  dpi = 300
)

# Plot: VKM efficiency (PT service vs ridership gain)
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
    subtitle = "Trip-level | Origin+Destination | PT+DRT catchment",
    x = "PT+DRT VKM Change (1000s km)",
    y = "PT+DRT Mode Share Change (pp)",
    color = "Proxy Value\n(× baseline)",
    caption = "Upper-right = efficient (ridership gains with service increase).\nDarker colors = better proxy optimization."
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
  "R/plots/transit_opt_paper/fig_4_2_vkm_vs_mode_share.png",
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

# Table 3: Impact by Catchment Type
# Show how results vary by filter configuration

catchment_sensitivity_summary <- res_mode_share |>
  filter(
    mode == "pt+drt",
    solution_id == 0 # Best solution
  ) |>
  group_by(objective, level, access, zones) |>
  summarise(
    share_delta_mean = mean(share_delta, na.rm = TRUE),
    share_delta_sd = sd(share_delta, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    objective_clean = objective_labels_short[objective],
    level_clean = level_labels[level],
    access_clean = access_labels[access],
    zones_clean = zones_labels[zones],
    filter_config = paste(level_clean, access_clean, zones_clean, sep = " | ")
  )

# Pivot for table format
table_4_3_catchment <- catchment_sensitivity_summary |>
  select(objective_clean, filter_config, share_delta_mean) |>
  pivot_wider(
    names_from = filter_config,
    values_from = share_delta_mean
  )

write_csv(
  table_4_3_catchment,
  "R/tables/transit_opt_paper/tables/table_4_3_catchment_sensitivity.csv"
)

# Plot: Filter sensitivity for PT+DRT mode share
sensitivity_plot_data <- res_mode_share |>
  filter(mode == "pt+drt", !is.na(share_delta)) |>
  mutate(
    level_clean = factor(
      level,
      levels = names(level_labels),
      labels = level_labels
    ),
    access_clean = factor(
      access,
      levels = names(access_labels),
      labels = access_labels
    ),
    zones_clean = factor(
      zones,
      levels = names(zones_labels),
      labels = zones_labels
    ),
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  sensitivity_plot_data,
  aes(
    x = solution_id,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
  labs(
    title = "PT+DRT Mode Share Change by Solution and Filter Configuration",
    subtitle = "Sensitivity to catchment definition and aggregation level",
    x = "Solution Rank",
    y = "Mode Share Change (pp)",
    color = "Aggregation",
    linetype = "Catchment",
    shape = "Access",
    caption = paste(
      "Aggregation: Trip = individual trips; Person = persons with ALL trips in catchment\n",
      "Catchment: PT = 400m buffer; PT+DRT = buffer + DRT zones\n",
      "Access: O = origin only; O+D = both trip ends in catchment"
    )
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(values = c("O" = 16, "O+D" = 17, "All" = 15)) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    strip.text = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray30")
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1, title.position = "left"),
    linetype = guide_legend(order = 2, nrow = 1, title.position = "left"),
    shape = guide_legend(order = 3, nrow = 1, title.position = "left")
  )

ggsave(
  "R/plots/transit_opt_paper/fig_4_3_catchment_sensitivity.png",
  width = 16,
  height = 12,
  dpi = 300
)

# Plot: Comparison of catchment impact for best solutions only
best_catchment_comparison <- res_mode_share |>
  filter(
    mode == "pt+drt",
    solution_id == 0
  ) |>
  mutate(
    filter_label = paste(level, access, zones, sep = " | "),
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
    title = "PT+DRT Mode Share Change by Filter Configuration (Best Solutions)",
    subtitle = "Comparing different catchment and aggregation definitions",
    x = "Filter Configuration (Level | Access | Zones)",
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

message("Tables saved to: R/tables/transit_opt_paper/tables/")
message("  - table_4_1_correlations.csv")
message("  - table_4_2_scenario_comparison.csv")
message("  - table_4_3_catchment_sensitivity.csv")

message("\nPlots saved to: R/plots/transit_opt_paper/")
message("  - fig_4_1_correlation_heatmap.png")
message("  - fig_4_1_proxy_vs_mode_share.png")
message("  - fig_4_2_pt_car_tradeoff.png")
message("  - fig_4_2_vkm_vs_mode_share.png")
message("  - fig_4_3_catchment_sensitivity.png")
message("  - fig_4_3_catchment_comparison_bar.png")

message("\nNote: Spatial/temporal plots are in plot_maps.R")
