library(tidyverse)


##########
# Reading in data
##########

## PSO results

# These results are for each objective function (e.g. sc_avg_var, wt_sum_tot ...)
# objective values for the baseline GTFS feed
obj_base <- read_csv("../../transit_opt/output/base_objective_values.csv")

# Objective values for the top n schedules based on PSO
obj_pso <- read_csv("R/output/pso_objective_values.csv")

## MATSim results
res_mode_share <- read_csv("R/output/mode_share_by_objective.csv")
res_vkm <- read_csv("R/output/vkm_by_objective.csv")

obj_base = obj_base |>
  rename(c("baseline_objective_value_pen" = "penalized_objective_value"))
##########
# Prepare PSO columns
##########

# Join baseline value onto each PSO solution
obj_pso_joined <- obj_pso |>
  left_join(
    obj_base |> select(config_name, baseline_objective_value_pen),
    by = c("objective_name" = "config_name")
  ) |>
  # rename to avoid clash
  rename(c("objective_sol" = "objective"))

# Calculate change relative to baseline
obj_pso_joined <- obj_pso_joined |>
  mutate(
    pso_pct_change_vs_base = 100 *
      (objective_sol - baseline_objective_value_pen) /
      baseline_objective_value_pen,
    pso_frac_of_base = round(objective_sol / baseline_objective_value_pen, 2)
  )

# Join base and solution objectives onto mode share and vkm
res_mode_share = res_mode_share |>
  left_join(
    obj_pso_joined,
    by = c("objective" = "objective_name", "solution" = "solution_id")
  )

res_vkm = res_vkm |>
  left_join(
    obj_pso_joined,
    by = c("objective" = "objective_name", "solution" = "solution_id")
  )

##########
# Mode Share Analysis
##########

##########
# Add PT+DRT combined mode and percentage point delta
##########

pt_drt_combined <- res_mode_share %>%
  filter(mode == "pt" | str_detect(mode, "^drt")) %>%
  group_by(objective, solution, solution_id, level, access, zones) %>%
  summarise(
    n_solution = sum(n_solution, na.rm = TRUE),
    share_solution = sum(share_solution, na.rm = TRUE),
    n_base = sum(n_base, na.rm = TRUE),
    share_base = sum(share_base, na.rm = TRUE),
    n_pct_change = ((n_solution - n_base) / n_base) * 100,
    share_pct_change = ((share_solution - share_base) / share_base) * 100,
    # Keep the PSO columns (they're the same for all pt/drt rows in a group)
    rank = first(rank),
    swarm_id = first(swarm_id),
    objective_sol = first(objective_sol),
    generation_found = first(generation_found),
    violations = first(violations),
    baseline_objective_value_pen = first(baseline_objective_value_pen),
    pso_pct_change_vs_base = first(pso_pct_change_vs_base),
    pso_frac_of_base = first(pso_frac_of_base),
    .groups = "drop"
  ) %>%
  mutate(mode = "pt+drt")

res_mode_share <- bind_rows(res_mode_share, pt_drt_combined) %>%
  arrange(objective, solution, solution_id, level, access, zones, mode)

res_mode_share <- res_mode_share %>%
  mutate(share_delta = share_solution - share_base)

##########
# Improved Labels
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

# Better, more concise labels
level_labels <- c(
  "trip" = "Trip",
  "person" = "Person",
  "all" = "All"
)

access_labels <- c(
  "origin" = "O",
  "origin+destination" = "O+D",
  "all" = "All"
)

zones_labels <- c(
  "pt" = "PT",
  "pt+drt" = "PT+DRT",
  "all" = "All"
)

main_modes <- c("pt+drt", "car", "walk", "bike", "taxi")

##########
# Plot 1: PT+DRT Mode Share Change - IMPROVED
##########

pt_drt_plot_data <- res_mode_share %>%
  filter(mode == "pt+drt", !is.na(share_delta)) %>%
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
  pt_drt_plot_data,
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
  labs(
    title = "Change in PT+DRT Mode Share by Solution",
    subtitle = "Absolute change in percentage points relative to baseline",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = paste(
      "Aggregation: Trip-level = individual trips; Person-level = persons with all trips in catchment\n",
      "Catchment: PT only = 400m buffer around stops; PT+DRT = stops + DRT operating zones\n",
      "Access: Origin only vs. both trip ends within catchment"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1.5),
    breaks = seq(0, 1.5, 0.1)
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    # 1. Aligns the stacked legends to the left of their box
    legend.box.just = "left",
    # 2. Anchors the whole legend block to the left of the plot area
    legend.justification = "left",
    # 3. Reduces whitespace between the 3 stacked legends
    legend.spacing.y = unit(0, "pt"),
    # Optional: Fine-tune margins to pull it tighter to the plot
    legend.margin = margin(t = 5, r = 0, b = 0, l = 0),
    legend.box.margin = margin(0, 0, 0, 0),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    axis.ticks.x = element_line(color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left", # Title above legend items
      title.hjust = 0, # Left-align title
      label.position = "right" # Labels to right of symbols
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_drt_share_change_by_solution_facet_objective.png",
  width = 16,
  height = 9,
  dpi = 300
)


# Plot waiting time objective only
ggplot(
  pt_drt_plot_data %>%
    filter(str_detect(objective, 'wt_')),
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "free_x", ncol = 4) +
  labs(
    title = "Change in PT+DRT Mode Share by Solution",
    subtitle = "Absolute change in percentage points relative to baseline",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = paste(
      "Aggregation: Trip-level = individual trips; Person-level = persons with all trips in catchment\n",
      "Catchment: PT only = 400m buffer around stops; PT+DRT = stops + DRT operating zones\n",
      "Access: Origin only vs. both trip ends within catchment"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    # 1. Aligns the stacked legends to the left of their box
    legend.box.just = "left",
    # 2. Anchors the whole legend block to the left of the plot area
    legend.justification = "left",
    # 3. Reduces whitespace between the 3 stacked legends
    legend.spacing.y = unit(0, "pt"),
    # Optional: Fine-tune margins to pull it tighter to the plot
    legend.margin = margin(t = 5, r = 0, b = 0, l = 0),
    legend.box.margin = margin(0, 0, 0, 0),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 45, hjust = 0.5),
    axis.ticks.x = element_line(color = "gray30"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left", # Title above legend items
      title.hjust = 0, # Left-align title
      label.position = "right" # Labels to right of symbols
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_drt_share_change_by_solution_facet_objective_wt_only.png",
  width = 16,
  height = 9,
  dpi = 300
)

# Facet by Objective AND Zone definition
ggplot(
  pt_drt_plot_data,
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = level_clean,
    # You can keep linetype if you want the legend,
    # but strictly speaking it's no longer needed for distinction
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(zones_clean ~ objective_clean, scales = "free_x") +
  labs(
    title = "Change in PT+DRT Mode Share by Solution",
    subtitle = "Absolute change in percentage points relative to baseline",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter"
  ) +
  scale_x_continuous(
    limits = c(0, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left", # Keeps your previous alignment fix
    legend.justification = "left",
    legend.spacing.y = unit(0, "pt"),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 60, hjust = 0.5),
    axis.ticks.x = element_line(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_drt_share_change_by_solution_facet_objective_catchment.png",
  width = 16,
  height = 9,
  dpi = 300
)


# Facet by Objective AND Zone definition - Wait time objectives only
ggplot(
  pt_drt_plot_data %>%
    filter(str_detect(objective, 'wt_')),
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = level_clean,
    # You can keep linetype if you want the legend,
    # but strictly speaking it's no longer needed for distinction
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.5, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(zones_clean ~ objective_clean, scales = "free_x") +
  labs(
    title = "Change in PT+DRT Mode Share by Solution",
    subtitle = "Absolute change in percentage points relative to baseline",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter"
  ) +
  scale_x_continuous(
    limits = c(0, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left", # Keeps your previous alignment fix
    legend.justification = "left",
    legend.spacing.y = unit(0, "pt"),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 60, hjust = 0.5),
    axis.ticks.x = element_line(color = "gray30"),
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_drt_share_change_by_solution_facet_objective_catchment_wt_only.png",
  width = 16,
  height = 9,
  dpi = 300
)

##########
# Plot 2: All Modes - Same filter config, vary mode
##########

all_modes_plot_data <- res_mode_share %>%
  filter(
    mode %in% main_modes,
    !is.na(share_delta),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    ),
    mode_clean = factor(
      mode,
      levels = main_modes,
      labels = c("PT+DRT", "Car", "Walk", "Bike", "Taxi")
    )
  )

ggplot(
  all_modes_plot_data,
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = mode_clean,
    group = mode_clean
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  facet_wrap(~objective_clean, scales = "free_x", nrow = 2) +
  labs(
    title = "Mode Share Changes Across All Transport Modes",
    subtitle = "Trip-level | Origin+Destination filter | PT+DRT catchment zones",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Transport Mode",
    caption = "Trips filtered to those with both origin and destination within 400m of PT stops or inside DRT zones"
  ) +
  scale_x_continuous(
    limits = c(0, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_manual(
    values = c(
      "PT+DRT" = "#E41A1C",
      "Car" = "#377EB8",
      "Walk" = "#4DAF4A",
      "Bike" = "#984EA3",
      "Taxi" = "#FF7F00"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_all_modes_change_trip_origin-dest_pt-drt.png",
  width = 14,
  height = 8,
  dpi = 300
)

# Same plot - Wait time objectives only

ggplot(
  all_modes_plot_data %>%
    filter(str_detect(objective, 'wt_')),
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = mode_clean,
    group = mode_clean
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  facet_wrap(~objective_clean, scales = "free_x", nrow = 2) +
  labs(
    title = "Mode Share Changes Across All Transport Modes",
    subtitle = "Trip-level | Origin+Destination filter | PT+DRT catchment zones",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Transport Mode",
    caption = "Trips filtered to those with both origin and destination within 400m of PT stops or inside DRT zones"
  ) +
  scale_x_continuous(
    limits = c(0.5, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_manual(
    values = c(
      "PT+DRT" = "#E41A1C",
      "Car" = "#377EB8",
      "Walk" = "#4DAF4A",
      "Bike" = "#984EA3",
      "Taxi" = "#FF7F00"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_all_modes_change_trip_origin-dest_pt-drt_wt_only.png",
  width = 14,
  height = 8,
  dpi = 300
)


##########
# Plot 3: Filter Sensitivity
##########

sensitivity_data <- res_mode_share %>%
  filter(
    objective == "wt_peak_tot",
    mode %in% main_modes,
    !is.na(share_delta)
  ) %>%
  filter(
    !(mode %in% c("walk", "bike"))
  ) %>%
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
    mode_clean = factor(
      mode,
      levels = main_modes,
      labels = c("PT+DRT", "Car", "Walk", "Bike", "Taxi")
    )
  )

# Facet by mode
ggplot(
  sensitivity_data,
  aes(
    x = pso_frac_of_base,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~mode_clean, scales = "free_y", nrow = 1) +
  labs(
    title = "Mode Share Sensitivity to Filter Definition",
    subtitle = "Objective = Wait Time Peak Total",
    x = "Objective Value (fraction of baseline)",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = paste(
      "Shows how different filtering assumptions affect measured mode share changes\n",
      "Trip-level = all trips; Person-level = only persons with ALL trips in catchment\n",
      "Origin only = relaxed filter; Origin+Destination = strict filter requiring both trip ends in catchment"
    )
  ) +
  scale_x_continuous(
    limits = c(0, 1.5), # Set explicit limits for all facets
    breaks = seq(0, 1.5, 0.25) # Cleaner breaks
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 3, nrow = 1)
  )

ggsave(
  "R/plots/transit_opt_paper/ms_filter_sensitivity_wt_peak_tot_facet_mode.png",
  width = 14,
  height = 9,
  dpi = 300
)

# Facet by mode and catchment

ggplot(
  sensitivity_data,
  aes(
    x = solution_id,
    y = share_delta,
    color = level_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_grid(zones_clean ~ mode_clean, scales = "free_x") +
  labs(
    title = "Mode Share Sensitivity to Filter Definition",
    subtitle = "Objective = Wait Time Peak Total - Faceted by Catchment Definition and Mode",
    x = "Solution Rank",
    y = "Mode Share Change (percentage points)",
    color = "Aggregation Level",
    shape = "Access Filter",
    caption = paste(
      "Rows show different catchment definitions (PT only vs PT+DRT vs All)\n",
      "Trip-level = all trips; Person-level = only persons with ALL trips in catchment\n",
      "Origin only = relaxed filter; Origin+Destination = strict filter requiring both trip ends in catchment"
    )
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.box.just = "left",
    legend.spacing.y = unit(0.1, "cm"),
    strip.text = element_text(face = "bold", size = 9),
    strip.text.y = element_text(angle = 0), # Make row labels horizontal
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "top",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "top",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_filter_sensitivity_wt_peak_tot_facet_mode_catchment.png",
  width = 14,
  height = 10,
  dpi = 300
)

# Create data for all objectives (not just wt_peak_tot)
sensitivity_all_objectives <- res_mode_share %>%
  filter(
    mode %in% main_modes,
    !(mode %in% c("walk", "bike")),
    !is.na(share_delta)
  ) %>%
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
    mode_clean = factor(
      mode,
      levels = main_modes,
      labels = c("PT+DRT", "Car", "Walk", "Bike", "Taxi")
    ),
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  sensitivity_all_objectives,
  aes(
    x = solution_id,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.3
  ) +
  facet_grid(mode_clean ~ objective_clean) +
  labs(
    title = "Mode Share Sensitivity Across Objectives and Modes",
    subtitle = "Faceted by Objective (rows) and Transport Mode (columns)",
    x = "Solution Rank",
    y = "Mode Share Change (pp)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = paste(
      "Shows how filter definitions affect mode share changes across all objectives\n",
      "Trip-level = all trips; Person-level = only persons with ALL trips in catchment\n",
      "PT only = 400m buffer; PT+DRT = stops + DRT zones\n",
      "Origin only vs. Origin+Destination = one vs. both trip ends in catchment"
    )
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing.y = unit(0, "pt"),
    strip.text = element_text(face = "bold", size = 8),
    strip.text.y = element_text(angle = 0, hjust = 0), # Horizontal row labels
    axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(
      hjust = 0,
      size = 7,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_filter_sensitivity_grid_facet_objectives_mode.png",
  width = 20,
  height = 12,
  dpi = 300
)

# Same plot but waiting time objectives only

ggplot(
  sensitivity_all_objectives %>%
    filter(str_detect(objective, 'wt_')),
  aes(
    x = solution_id,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.6, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.3
  ) +
  facet_grid(mode_clean ~ objective_clean) +
  labs(
    title = "Mode Share Sensitivity Across Objectives and Modes",
    subtitle = "Faceted by Objective (rows) and Transport Mode (columns)",
    x = "Solution Rank",
    y = "Mode Share Change (pp)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = paste(
      "Shows how filter definitions affect mode share changes across all objectives\n",
      "Trip-level = all trips; Person-level = only persons with ALL trips in catchment\n",
      "PT only = 400m buffer; PT+DRT = stops + DRT zones\n",
      "Origin only vs. Origin+Destination = one vs. both trip ends in catchment"
    )
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.spacing.y = unit(0, "pt"),
    strip.text = element_text(face = "bold", size = 8),
    strip.text.y = element_text(angle = 0, hjust = 0), # Horizontal row labels
    axis.text.x = element_text(size = 7, angle = 45, hjust = 0.5),
    axis.text.y = element_text(size = 7),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.3, "lines"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    plot.caption = element_text(
      hjust = 0,
      size = 7,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(
      order = 1,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    ),
    shape = guide_legend(
      order = 3,
      nrow = 1,
      title.position = "left",
      title.hjust = 0
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_filter_sensitivity_grid_facet_objectives_mode_wt_only.png",
  width = 20,
  height = 12,
  dpi = 300
)


##########
# Plot 4: Trade-offs
##########

tradeoff_data <- res_mode_share %>%
  filter(
    mode %in% c("pt+drt", "car"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  select(objective, solution_id, mode, share_delta) %>%
  pivot_wider(names_from = mode, values_from = share_delta) %>%
  rename(pt_drt_change = `pt+drt`, car_change = car) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  tradeoff_data,
  aes(x = car_change, y = pt_drt_change, color = solution_id)
) +
  geom_point(size = 2.5, alpha = 0.7) +
  facet_wrap(~objective_clean, scales = "fixed", nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dotted",
    color = "gray30"
  ) +
  labs(
    title = "PT+DRT Gain vs. Car Loss Trade-off",
    subtitle = "Trip-level | Origin+Destination | PT+DRT catchment",
    x = "Car Mode Share Change (percentage points)",
    y = "PT+DRT Mode Share Change (percentage points)",
    color = "Solution Rank",
    caption = "Diagonal line shows perfect 1:1 substitution. Points above line indicate PT gains exceed car losses (potential walk/bike substitution)."
  ) +
  # scale_color_viridis_c(
  #   breaks = seq(0, 100, 20),  # Show breaks at actual data points
  #   limits = c(0, 100)          # Set scale limits to match data range
  # ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Lajolla",
    rev = TRUE,
    breaks = seq(0, 100, 20), # Show breaks at actual data points
    limits = c(0, 100) # Set scale limits to match data range)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_car_tradeoff.png",
  width = 12,
  height = 8,
  dpi = 300
)

# Same plot but waiting time only

ggplot(
  tradeoff_data %>%
    filter(str_detect(objective, 'wt_')),
  aes(x = car_change, y = pt_drt_change, color = solution_id)
) +
  geom_point(size = 2.5, alpha = 0.7) +
  facet_wrap(~objective_clean, scales = "fixed", nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dotted",
    color = "gray30"
  ) +
  labs(
    title = "PT+DRT Gain vs. Car Loss Trade-off",
    subtitle = "Trip-level | Origin+Destination | PT+DRT catchment",
    x = "Car Mode Share Change (percentage points)",
    y = "PT+DRT Mode Share Change (percentage points)",
    color = "Solution Rank",
    caption = "Diagonal line shows perfect 1:1 substitution. Points above line indicate PT gains exceed car losses (potential walk/bike substitution)."
  ) +
  # scale_color_viridis_c(
  #   breaks = seq(0, 100, 20),  # Show breaks at actual data points
  #   limits = c(0, 100)          # Set scale limits to match data range
  # ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Lajolla",
    rev = TRUE,
    breaks = seq(0, 100, 20), # Show breaks at actual data points
    limits = c(0, 100) # Set scale limits to match data range)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/ms_pt_car_tradeoff_wt_only.png",
  width = 12,
  height = 8,
  dpi = 300
)

##########
# Best solutions table
##########

best_solutions <- res_mode_share %>%
  filter(
    mode %in% c("pt", "pt+drt"),
    # level == "trip",
    # access == "origin+destination",
    # zones == "pt+drt"
  ) %>%
  group_by(objective, mode, level, access, zones) %>%
  slice_max(share_delta, n = 3) %>%
  arrange(objective, mode, level, access, zones, desc(share_delta)) %>%
  mutate(rank_matsim = row_number()) %>%
  ungroup() %>%
  select(
    objective,
    solution_id,
    rank_matsim,
    mode,
    level,
    access,
    zones,
    share_delta,
    share_solution,
    share_base
  )

write_csv(
  best_solutions,
  "R/plots/transit_opt_paper/ms_best_solutions_pt_drt.csv"
)


##########
# VKM Analysis
##########

# Combine PT and DRT VKM (sum their distances)
pt_drt_vkm_combined <- res_vkm %>%
  filter(mode == "pt" | str_detect(mode, "^drt")) %>%
  group_by(objective, solution, solution_id, level, access, zones) %>%
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mode = "pt+drt",
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = (delta_km / total_distance_km_base) * 100,
    share_solution = NA_real_,
    share_base = NA_real_,
    share_pct_change = NA_real_
  )

# Add pt+drt to original data
res_vkm <- bind_rows(res_vkm, pt_drt_vkm_combined) %>%
  arrange(objective, solution, solution_id, level, access, zones, mode)

# Add share_delta
res_vkm <- res_vkm %>%
  mutate(share_delta = share_solution - share_base)

# Create car+taxi combined mode
car_taxi_combined <- res_vkm %>%
  filter(mode %in% c("car", "taxi")) %>%
  group_by(objective, solution, solution_id, level, access, zones) %>%
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mode = "car+taxi",
    share_solution = NA_real_,
    share_base = NA_real_,
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = (delta_km / total_distance_km_base) * 100,
    share_pct_change = NA_real_
  )

# Combine with extended data
res_vkm_extended <- bind_rows(res_vkm, car_taxi_combined) %>%
  arrange(objective, solution, solution_id, level, access, zones, mode)

# Recalculate shares for BOTH car+taxi AND pt+drt
res_vkm_extended <- res_vkm_extended %>%
  group_by(objective, solution, solution_id, level, access, zones) %>%
  mutate(
    # Total motorized includes car, taxi, pt, AND all drt modes
    total_motorized_solution = sum(
      total_distance_km_solution[
        mode %in% c("car", "taxi", "pt") | str_detect(mode, "^drt")
      ],
      na.rm = TRUE
    ),
    total_motorized_base = sum(
      total_distance_km_base[
        mode %in% c("car", "taxi", "pt") | str_detect(mode, "^drt")
      ],
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    share_solution = if_else(
      mode %in% c("car+taxi", "pt+drt"),
      (total_distance_km_solution / total_motorized_solution) * 100,
      share_solution
    ),
    share_base = if_else(
      mode %in% c("car+taxi", "pt+drt"),
      (total_distance_km_base / total_motorized_base) * 100,
      share_base
    ),
    share_delta = share_solution - share_base,
    share_pct_change = if_else(
      mode %in% c("car+taxi", "pt+drt"),
      ((share_solution - share_base) / share_base) * 100,
      share_pct_change
    )
  ) %>%
  select(-total_motorized_solution, -total_motorized_base)


##########
# VKM Plot 1 (IMPROVED): Motorized VKM Change by Mode Group
##########

motorized_contributions_data <- res_vkm_extended %>%
  filter(
    mode %in% c("car+taxi", "pt+drt"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    ),
    mode_clean = factor(
      mode,
      levels = c("pt+drt", "car+taxi"),
      labels = c("PT+DRT", "Car+Taxi")
    )
  )

ggplot(
  motorized_contributions_data,
  aes(
    x = solution_id,
    y = delta_km / 1000,
    color = mode_clean,
    group = mode_clean
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "free_x", nrow = 2) +
  labs(
    title = "Change in Vehicle Kilometers by Mode Group",
    subtitle = "PT+DRT and Car+Taxi contributions | Trip-level | Origin+Destination | PT+DRT catchment",
    x = "Solution Rank",
    y = "VKM Change (thousands of km)",
    color = "Mode Group",
    caption = "Positive values indicate increased vehicle travel. PT increases from service expansion; Car+Taxi changes from modal shift."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(values = c("PT+DRT" = "#E41A1C", "Car+Taxi" = "#377EB8")) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_change_mode_group_km.png",
  width = 14,
  height = 8,
  dpi = 300
)

##########
# VKM Plot 2: VKM Share Change by Mode Group (using share_delta)
##########

vkm_share_group_data <- res_vkm_extended %>%
  filter(
    mode %in% c("car+taxi", "pt+drt"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    ),
    mode_clean = factor(
      mode,
      levels = c("pt+drt", "car+taxi"),
      labels = c("PT+DRT", "Car+Taxi")
    )
  )

ggplot(
  vkm_share_group_data,
  aes(x = solution_id, y = share_delta, color = mode_clean, group = mode_clean)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  facet_wrap(~objective_clean, scales = "free_x", nrow = 2) +
  labs(
    title = "VKM Share Changes by Mode Group",
    subtitle = "Percentage point change in share of total motorized VKM | Trip-level | Origin+Destination | PT+DRT",
    x = "Solution Rank",
    y = "VKM Share Change (percentage points)",
    color = "Mode Group",
    caption = "Share of total vehicle kilometers traveled. PT+DRT and Car+Taxi shares must sum to 100%, so changes are inversely related."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(values = c("PT+DRT" = "#E41A1C", "Car+Taxi" = "#377EB8")) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_change_mode_group_perc.png",
  width = 14,
  height = 8,
  dpi = 300
)

##########
# VKM Plot 2b: Total Motorized VKM Change by Filter Configuration
##########

# Calculate total motorized VKM (use only individual modes to avoid double counting)
total_motorized_vkm_data <- res_vkm_extended %>%
  filter(
    mode %in% c("car", "taxi", "pt") | str_detect(mode, "^drt") # Individual modes only
  ) %>%
  group_by(objective, solution, solution_id, level, access, zones) %>%
  summarise(
    total_distance_km_solution = sum(total_distance_km_solution, na.rm = TRUE),
    total_distance_km_base = sum(total_distance_km_base, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    delta_km = total_distance_km_solution - total_distance_km_base,
    delta_km_pct = (delta_km / total_distance_km_base) * 100,
    combo = paste(level, access, zones, sep = " | ")
  ) %>%
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
    combo_clean = paste(level_clean, access_clean, zones_clean, sep = " | "),
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  total_motorized_vkm_data,
  aes(x = solution_id, y = delta_km / 1000, color = combo_clean, group = combo)
) +
  geom_line(linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "fixed", nrow = 2) +
  labs(
    title = "Total Motorized VKM Change by Filter Configuration",
    subtitle = "Sum of car, taxi, PT, and DRT vehicle kilometers",
    x = "Solution Rank",
    y = "Total Motorized VKM Change (thousands of km)",
    color = "Filter Configuration",
    caption = "Level | Access | Catchment. Shows how different filtering assumptions affect total measured vehicle travel."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE))

ggsave(
  "R/plots/transit_opt_paper/vkm_total_motorized_vkm_change_by_filter_km.png",
  width = 14,
  height = 8,
  dpi = 300
)

##########
# VKM Plot 2c: Mode Group VKM Change by Filter Configuration (Grid with two lines)
##########

# Prepare data with car+taxi and pt+drt separately
mode_group_vkm_grid_data <- res_vkm_extended %>%
  filter(
    mode %in% c("car+taxi", "pt+drt") # Use the combined modes
  ) %>%
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
    combo_clean = paste(level_clean, access_clean, zones_clean, sep = " | "),
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    ),
    mode_clean = factor(
      mode,
      levels = c("pt+drt", "car+taxi"),
      labels = c("PT+DRT", "Car+Taxi")
    )
  )

ggplot(
  mode_group_vkm_grid_data,
  aes(
    x = solution_id,
    y = delta_km / 1000,
    color = mode_clean,
    group = mode_clean
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.3
  ) +
  facet_grid(combo_clean ~ objective_clean, scales = "fixed") +
  labs(
    title = "VKM Change by Mode Group, Filter Configuration, and Objective",
    subtitle = "Rows = Filter combinations (Level | Access | Catchment) | Columns = Objectives",
    x = "Solution Rank",
    y = "VKM Change (1000s km)",
    color = "Mode Group",
    caption = "PT+DRT increases from service expansion; Car+Taxi changes from modal shift and catchment filtering."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  scale_color_manual(values = c("PT+DRT" = "#E41A1C", "Car+Taxi" = "#377EB8")) +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    strip.text.y = element_text(angle = 0, hjust = 0),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 7),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 7,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_change_grid_by_group_facet_filter_combination.png",
  width = 16,
  height = 12,
  dpi = 300
)


##########
# VKM Plot 2: VKM Share Change by Mode (Car vs Taxi vs PT)
##########

vkm_share_plot_data <- res_vkm_extended %>%
  filter(
    mode %in% c("car", "taxi", "pt"),
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    ),
    mode_clean = factor(
      mode,
      levels = c("pt", "car", "taxi"),
      labels = c("PT", "Car", "Taxi")
    )
  )

ggplot(
  vkm_share_plot_data,
  aes(x = solution_id, y = share_delta, color = mode_clean, group = mode_clean)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray40",
    alpha = 0.5
  ) +
  facet_wrap(~objective_clean, scales = "fixed", ncol = 4) +
  labs(
    title = "VKM Share Changes by Mode",
    subtitle = "Percentage point change in share of total motorized VKM | Trip-level | Origin+Destination | PT+DRT",
    x = "Solution Rank",
    y = "VKM Share Change (percentage points)",
    color = "Mode",
    caption = "Share of total vehicle kilometers traveled. PT gains typically come from increased service, not modal shift."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_color_manual(
    values = c("PT" = "#E41A1C", "Car" = "#377EB8", "Taxi" = "#FF7F00")
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_change_by_mode_perc.png",
  width = 14,
  height = 8,
  dpi = 300
)

##########
# VKM Plot 3: PT VKM Change (Absolute km)
##########

pt_vkm_plot_data <- res_vkm_extended %>%
  filter(
    mode == "pt",
    level == "trip", # PT VKM is same across levels, so just pick one
    access == "origin", # Same across access filters
    zones == "pt" # Same across zones
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  pt_vkm_plot_data,
  aes(x = solution_id, y = delta_km / 1000)
) +
  geom_line(linewidth = 0.8, color = "#E41A1C") +
  geom_point(size = 2, alpha = 0.7, color = "#E41A1C") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~objective_clean, scales = "free_x", nrow = 2) +
  labs(
    title = "Change in PT Service Kilometers by Solution",
    subtitle = "Total scheduled transit vehicle kilometers from GTFS",
    x = "Solution Rank",
    y = "PT VKM Change (thousands of km)",
    caption = "Positive values = more PT service. Based on GTFS schedule, not actual ridership."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  theme_bw(base_size = 11) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_change_pt_only_km.png",
  width = 14,
  height = 8,
  dpi = 300
)

##########
# VKM Plot 5: Filter Sensitivity (like mode share)
##########

vkm_sensitivity_data <- res_vkm_extended %>%
  filter(
    objective == "wt_peak_tot",
    mode %in% c("car", "taxi", "pt")
  ) %>%
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
    mode_clean = factor(
      mode,
      levels = c("pt", "car", "taxi"),
      labels = c("PT", "Car", "Taxi")
    )
  )

ggplot(
  vkm_sensitivity_data,
  aes(
    x = solution_id,
    y = share_delta,
    color = level_clean,
    linetype = zones_clean,
    shape = access_clean,
    group = interaction(level, access, zones)
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  facet_wrap(~mode_clean, scales = "free_y", nrow = 1) +
  labs(
    title = "VKM Share Sensitivity to Filter Definition",
    subtitle = "Objective = Wait Time Peak Total",
    x = "Solution Rank",
    y = "VKM Share Change (percentage points)",
    color = "Aggregation Level",
    linetype = "Catchment Definition",
    shape = "Access Filter",
    caption = "PT share is constant across filters (from GTFS). Car/Taxi vary based on which trips are included."
  ) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(
    values = c("PT" = "dashed", "PT+DRT" = "solid", "All" = "dotted")
  ) +
  scale_shape_manual(
    values = c("O" = 16, "O+D" = 17, "All" = 15)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  ) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 3, nrow = 1)
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_filter_sensitivity_wt_peak_tot_facet_mode.png",
  width = 14,
  height = 6,
  dpi = 300
)


##########
# Combined Analysis (VKM + Mode Share)
##########

##########
# Plot 1: VKM vs Mode Share Trade-off (PT service vs PT ridership)
##########

# Combine VKM and mode share data
vkm_mode_tradeoff <- res_vkm_extended %>%
  filter(
    mode == "pt+drt",
    level == "trip",
    access == "origin+destination",
    zones == "pt+drt"
  ) %>%
  select(objective, solution_id, pt_vkm_change = delta_km) %>%
  left_join(
    res_mode_share %>%
      filter(
        mode == "pt+drt",
        level == "trip",
        access == "origin+destination",
        zones == "pt+drt"
      ) %>%
      select(objective, solution_id, pt_share_delta = share_delta),
    by = c("objective", "solution_id")
  ) %>%
  mutate(
    objective_clean = factor(
      objective,
      levels = names(objective_labels),
      labels = objective_labels
    )
  )

ggplot(
  vkm_mode_tradeoff,
  aes(x = pt_vkm_change / 1000, y = pt_share_delta, color = solution_id)
) +
  geom_point(size = 2.5, alpha = 0.7) +
  facet_wrap(~objective_clean, scales = "fixed", nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "PT Service Investment vs. Ridership Gain Trade-off",
    subtitle = "Trip-level | Origin+Destination | PT+DRT catchment",
    x = "PT VKM Change (thousands of km)",
    y = "PT+DRT Mode Share Change (percentage points)",
    color = "Solution Rank",
    caption = "Upper-right quadrant shows solutions with both increased service and ridership. Efficiency = vertical distance from horizontal axis per unit horizontal distance."
  ) +
  colorspace::scale_color_continuous_sequential(
    palette = "Lajolla",
    rev = TRUE,
    breaks = seq(0, 100, 20), # Show breaks at actual data points
    limits = c(0, 100) # Set scale limits to match data range)
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption = element_text(
      hjust = 0,
      size = 8,
      color = "gray30",
      margin = margin(t = 10)
    )
  )

ggsave(
  "R/plots/transit_opt_paper/vkm_pt_vkm_vs_pt_mode_share_tradeoff-mode_pt_drt-catchment_o_d-level_trip.png",
  width = 12,
  height = 8,
  dpi = 300
)
