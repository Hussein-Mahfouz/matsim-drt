# This script plots the calibration process (How ASCs and Mode shares changed through the process)

library(tidyverse)

# ================
# LOAD IN THE DATA
# ================

# mode shares
mode_share = read_csv("../scenarios/calibration_0.25/mode_shares.csv")
# asc values
asc_values = read_csv("../scenarios/calibration_0.25/asc_values.csv")

# ================
# DEFINE REFERENCE MODE SHARES
# ================

# Reference mode shares (# car-passenger is fixed at 23%)
mode_share_ref = tibble(
  mode = c("Car", "PT", "Bike", "Walk", "Taxi"),
  mode_share_ref = c(0.38, 0.10, 0.01, 0.25, 0.03)) %>%
  mutate(mode_share_ref = mode_share_ref * 100)


# ================
# PLOTS
# ================


# --- pivot wider for ggplot
mode_share_plot = mode_share %>%
  pivot_longer(!Iteration, names_to = "mode", values_to = "mode_share") %>%
  # turn from fraction to %
  mutate(mode_share = mode_share * 100)

asc_values_plot = asc_values %>%
  pivot_longer(!Iteration, names_to = "mode", values_to = "asc_value")


# --- add reference mode shares
mode_share_plot = mode_share_plot %>%
  left_join(mode_share_ref, by = "mode")



# ----- plot MODE SHARES

# All in one plot
ggplot(mode_share_plot, aes(x = Iteration, y = mode_share, color = mode)) +
  geom_line(size = 1) +
  geom_rect(
    data = mode_share_ref,
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf,
        ymin = mode_share_ref - 1, ymax = mode_share_ref + 1,
        fill = mode),
    alpha = 0.2
  ) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  theme_light() +
  theme(legend.position="bottom") +
  labs(title = "Mode share calibration",
       x = "Calibration step",
       y = "Mode share (%)",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_mode_share_all.png")


# Facet plot
ggplot(mode_share_plot, aes(x = Iteration, y = mode_share, color = mode)) +
  geom_line(size = 1) +
  geom_rect(
    data = mode_share_ref,
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf,
        ymin = mode_share_ref - 1, ymax = mode_share_ref + 1,
        fill = mode),
    alpha = 0.2
  ) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  facet_wrap(~mode) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title = "Mode share calibration",
       x = "Calibration step",
       y = "Mode share (%)",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_mode_share_facet.png")

# ----- plot MODE SHARES (add car_passenger)

mode_share_plot_car_passenger = mode_share_plot %>%
         # add car_passenger to mode share of CAR
  mutate(mode_share = case_when(mode == "Car" ~ mode_share + 25,
                          .default = mode_share),
         # add car_passenger to refernce mode share of CAR
         mode_share_ref = case_when(mode == "Car" ~ mode_share_ref + 25,
                                .default = mode_share_ref))

# update the reference table
mode_share_ref_car_passenger = mode_share_ref %>%
  mutate(mode_share_ref = case_when(mode == "Car" ~ mode_share_ref + 25,
                                .default = mode_share_ref))




# All in one plot
ggplot(mode_share_plot_car_passenger, aes(x = Iteration, y = mode_share, color = mode)) +
  geom_line(size = 1) +
  geom_rect(
    data = mode_share_ref_car_passenger,
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf,
        ymin = mode_share_ref - 1, ymax = mode_share_ref + 1,
        fill = mode),
    alpha = 0.2
  ) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title = "Mode share calibration",
       x = "Calibration step",
       y = "Mode share (%)",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_mode_share_all_car_passenger.png")


# Facet plot
ggplot(mode_share_plot_car_passenger, aes(x = Iteration, y = mode_share, color = mode)) +
  geom_line(size = 1) +
  geom_rect(
    data = mode_share_ref_car_passenger,
    inherit.aes = FALSE,
    aes(xmin = -Inf, xmax = Inf,
        ymin = mode_share_ref - 1, ymax = mode_share_ref + 1,
        fill = mode),
    alpha = 0.3
  ) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  facet_wrap(~mode) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)
  ) +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title = "Mode share calibration",
       x = "Calibration step",
       y = "Mode share (%)",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_mode_share_facet_car_passenger.png")


# ----- plot ASC VALUES


# clean names
asc_values_plot = asc_values_plot %>%
  mutate(mode = str_remove(mode, "ASC_"),
         mode = str_to_title(mode))

ggplot(asc_values_plot, aes(x = Iteration, y = asc_value, color = mode)) +
  geom_line(size = 1) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(title = "Change in ASC values as mode share is calibrated",
       x = "Calibration step",
       y = "ASC value",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_asc_values_all.png")


# ----- plot ASC VALUES (without Rail)
asc_values_plot_no_rail = asc_values_plot %>%
  filter(mode != "Rail") %>%
  mutate(mode = case_when(mode == "Bus" ~ "PT (Bus)",
                          .default = mode))

# Plot
ggplot(asc_values_plot_no_rail, aes(x = Iteration, y = asc_value, color = mode)) +
  geom_line(size = 1) +
  # geom_hline(aes(yintercept = mode_share_ref + 1, color = mode), linetype = "dotted", size = 0.6) +
  # geom_hline(aes(yintercept = mode_share_ref - 1, color = mode), linetype = "dotted", size = 0.6) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(title = "Change in ASC values as mode share is calibrated",
       x = "Calibration step",
       y = "ASC value",
       color = "",
       fill = "")

ggsave("plots/calibration/calibration_asc_values_all_no_rail.png")



