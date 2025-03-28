library(tidyverse)
library(arrow)
library(ggalluvial) # sankey

# This script looks at transitions in mode share. We compare all trip modes before and after
# the simulation, and plot a sankey diagram of the results

# ---------- Read in the data

# Input demand
# demand_original = arrow::read_parquet("../data/demand/legs_with_locations.parquet")
demand_original = read_delim("../scenarios/basic/sample_1.00/eqasim_trips.csv", delim =";")

# Output demand
scenario = "zones"
# scenario = "all"
fleet_size = 1000 # 20, 100, 200, 500, 1000

demand_matsim = read_delim(paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/eqasim_trips.csv"), delim =";")

# Add Trip distance
demand_matsim <- demand_matsim %>%
  mutate(euclidean_distance_bucket = cut(euclidean_distance,
                           breaks = seq(0, ceiling(max(euclidean_distance, na.rm = TRUE) / 2500) * 2500, by = 2500),
                           include.lowest = TRUE, right = FALSE))

# ---------- Join input and output trips

# Prepare data for joining (we join on person id and the equivalent of the trip sequence)
demand_original = demand_original %>%
  # mutate(person_trip_id = seq - 1) %>%      # eqasim output is a person_trip_id which is seq starting from 0
  rename(pid = person_id) %>%
  select(pid, person_trip_id, mode) %>%
  # rename_with(~ paste0("input_", .))
  rename_with(~ paste0("input_", .), .cols = c("mode"))

demand_matsim = demand_matsim %>%
  rename(pid = person_id) %>%
  select(pid, person_trip_id, mode) %>%
  rename_with(~ paste0("output_", .), .cols = c("mode"))

# join
demand_compare = demand_original %>%
  left_join(demand_matsim, by = c("pid", "person_trip_id"))

# ---------- Calculate mode summary statistics
demand_compare_summary = demand_compare %>%
  group_by(input_mode, output_mode) %>%
  summarise(trips = n())


# ---------- Plots

# Create columns for labels
demand_compare_summary = demand_compare_summary %>%
  group_by(input_mode) %>%
  mutate(input_mode_trips = sum(trips)) %>% # total number of trips that started with mode = input_mode
  ungroup() %>%
  mutate(input_mode_trips_frac = round((input_mode_trips / sum(trips)) * 100, 1)) %>%  # initial mode share of input_mode (%)
  mutate(input_mode_trips_with_frac = glue::glue("{input_mode} ({input_mode_trips_frac} {'%'})")) # initial mode + initial mode share (%)


# Create a static Sankey-like diagram using ggplot and ggalluvial
ggplot(data = demand_compare_summary, aes(axis1 = input_mode, axis2 = output_mode, y = trips)) +
  geom_alluvium(aes(fill = input_mode_trips_with_frac)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = input_mode), size = 3) +
  # ggrepel::geom_text_repel(
  #   aes(label = input_mode),
  #   stat = "stratum", size = 4, direction = "y", nudge_x = -.5
  # ) +
  ggrepel::geom_text_repel(
    aes(label = output_mode),
    stat = "stratum", size = 3, direction = "y", nudge_x = .35
  ) +
  scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
  scale_y_continuous(labels = scales::label_comma()) +  # Format y-axis with commas
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw()  +
  theme(panel.border = element_blank()) +
  labs(
    title = "Shifts in Mode Share after introduction of DRT",
    subtitle = "What modes did all trips transition from?",
    y = "Number of trips",  # Change the y-axis label here
    fill = "Initial mode \n(Initial mode share)"  # Change the legend title here
  )

ggsave(paste0("plots/mode_share/mode_share_sankey_", scenario, "_", fleet_size, ".png"))

# ----- Same plot but combine all DRT (no distinction with feeder or specific fleets)
demand_compare_summary_all <- demand_compare_summary %>%
  mutate(output_mode_drt = if_else(str_detect(output_mode, regex("drt", ignore_case = TRUE)),
                               "drt",
                               output_mode)) %>%
  # get total trips by DRT (we need one row per unique OD pair)
  group_by(input_mode, output_mode_drt, input_mode_trips_with_frac) %>%
  summarise(trips = sum(trips)) %>%
  ungroup() %>%
  filter(!is.na(output_mode_drt))


ggplot(data = demand_compare_summary_all, aes(axis1 = input_mode, axis2 = output_mode_drt, y = trips)) +
  geom_alluvium(aes(fill = input_mode_trips_with_frac)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = input_mode), size = 3) +
  # ggrepel::geom_text_repel(
  #   aes(label = input_mode),
  #   stat = "stratum", size = 4, direction = "y", nudge_x = -.5
  # ) +
  ggrepel::geom_text_repel(
    aes(label = output_mode_drt),
    stat = "stratum", size = 3, direction = "y", nudge_x = .35
  ) +
  scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
  scale_y_continuous(labels = scales::label_comma()) +  # Format y-axis with commas
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw()  +
  theme(panel.border = element_blank()) +
  labs(
    title = "Shifts in Mode Share after introduction of DRT",
    subtitle = "What modes did all trips transition from?",
    y = "Number of trips",  # Change the y-axis label here
    fill = "Initial mode \n(Initial mode share)"  # Change the legend title here
  )

ggsave(paste0("plots/mode_share/mode_share_sankey_", scenario, "_", fleet_size, "_sum_drt.png"))




# Let's focus only on trips that came out as DRT
demand_compare_summary_drt = demand_compare_summary %>%
  filter(str_detect(output_mode, "drt")) %>%
  mutate(trips_moved_drt_frac = round((trips / input_mode_trips) * 100, 2)) %>% # % of Trips for a particular mode that shifted to DRT
  group_by(input_mode) %>%
  mutate(total_drt_frac = sum(trips_moved_drt_frac), # total % of trips that transition to DRT (regardless of standalone or feeder)
         mode_with_trips_moved_drt_frac = glue::glue("{input_mode} ({total_drt_frac} {'%'})")) %>% # initial mode + initial mode share (%)
  ungroup()



ggplot(data = demand_compare_summary_drt,
       aes(axis1 = input_mode, axis2 = output_mode, y = trips)) +
  geom_alluvium(aes(fill = mode_with_trips_moved_drt_frac)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = input_mode)) +
  # geom_text(stat = "stratum", aes(label = glue::glue("{input_mode} ({people})")))  +
  geom_text(stat = "stratum", aes(label = output_mode)) +
  scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_y_continuous(labels = scales::label_comma()) +  # Format y-axis with commas
  theme_bw() +
  theme(panel.border = element_blank()) +
  labs(
    title = "Shifts in Mode Share after introduction of DRT",
    subtitle = "What modes did DRT user trips transition from?",
    y = "Number of trips",  # Change the y-axis label here
    fill = "Initial mode (% \nof trips that \nshifted to DRT)"  # Change the legend title here
  )

ggsave(paste0("plots/mode_share/mode_share_sankey_", scenario, "_", fleet_size, "_drt.png"))


