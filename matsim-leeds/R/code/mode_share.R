library(tidyverse)
library(arrow)
library(ggalluvial) # sankey

# This script looks at transitions in mode share. We compare all trip modes before and after
# the simulation, and plot a sankey diagram of the results

# ---------- Read in the data

# Input demand
# demand_original = arrow::read_parquet("../data/demand/legs_with_locations.parquet")
demand_original = read_delim("../scenarios/basic/sample_1.00/eqasim_trips.csv", delim =";")


# Set up a list of scenarios and fleet sizes to read in (file directories should exist)
scenarios <- c("zones",
               "all",
               "innerBUA")
fleet_sizes <- c(100, 200, 500, 1000)



# Define scenario names for plot subtitles
scenario_labels <- c(
  "zones" = "Zone-based DRT",
  "all" = "Citywide DRT",
  "innerBUA" = "Zone-based DRT (inner)"
)

# Function to read and process a file and add identifier column
read_and_process <- function(scenario, fleet_size, file_name) {
  # Read the data
  file_path <- paste0("../scenarios/fleet_sizing/", scenario, "/", fleet_size, "/sample_1.00/", file_name, ".csv")
  # Check if file exists
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)  # Safe fail
  }
  # Print status
  print(paste("Reading file:", file_path))
  # Read
  data <- read_delim(file_path, delim = ";")

  # Add the scenario and fleet size columns
  data <- data %>%
    mutate(scenario = scenario, fleet_size = fleet_size)

  return(data)
}


# Create a data frame of all combinations of scenarios and fleet sizes to read in
combinations <- expand.grid(scenario = scenarios, fleet_size = fleet_sizes)

# ----------  All DRT trips ---------- #

# Use purrr::pmap_dfr to read and process each combination. All dfs are binded together
demand_matsim <- purrr::pmap_dfr(combinations, function(scenario, fleet_size) {
  read_and_process(scenario, fleet_size, "eqasim_trips")
})

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

# Create columns for labels
demand_original = demand_original %>%
  group_by(input_mode) %>%
  mutate(input_mode_trips = n()) %>% # total number of trips that started with mode = input_mode
  ungroup() %>%
  mutate(input_mode_trips_frac = round((input_mode_trips / n()) * 100, 1)) %>%  # initial mode share of input_mode (%)
  mutate(input_mode_trips_with_frac = glue::glue("{input_mode} ({input_mode_trips_frac} {'%'})")) # initial mode + initial mode share (%)



demand_matsim = demand_matsim %>%
  rename(pid = person_id) %>%
  select(pid, person_trip_id, mode, scenario, fleet_size) %>%
  rename_with(~ paste0("output_", .), .cols = c("mode"))

# join
demand_compare = demand_original %>%
  left_join(demand_matsim, by = c("pid", "person_trip_id"))

# ---------- Calculate mode summary statistics
demand_compare_summary = demand_compare %>%
  group_by(input_mode, input_mode_trips, input_mode_trips_frac, input_mode_trips_with_frac, output_mode, scenario, fleet_size) %>%
  summarise(trips = n()) %>%
  ungroup()

# save
write_csv(demand_compare_summary, "plots/mode_share/mode_shift_all.csv")



# ---------- Plots

# Loop over combinations and print + save each one
for (scenario_plot in scenarios) {
  for (fleet_size_plot in fleet_sizes) {

    # Subset data
    plot_data <- demand_compare_summary %>%
      filter(fleet_size == fleet_size_plot, scenario == scenario_plot)

    # Create plot
    p <- ggplot(data = plot_data,
                aes(axis1 = input_mode, axis2 = output_mode, y = trips)) +
      geom_alluvium(aes(fill = input_mode_trips_with_frac)) +
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = input_mode), size = 3) +
      ggrepel::geom_text_repel(
        aes(label = output_mode),
        stat = "stratum", size = 3, direction = "y", nudge_x = .35
      ) +
      scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      theme_bw() +
      theme(panel.border = element_blank()) +
      labs(
        title = "What modes did all trips transition from?",
        subtitle = scenario_labels[[scenario_plot]],  # Custom subtitle
        y = "Number of trips",
        fill = "Initial mode \n(Initial mode share)",
        caption = paste0("DRT fleet size = ", fleet_size_plot)
      )

    print(p)

    ggsave(filename = paste0("plots/mode_share/sankey_", scenario_plot, "_", fleet_size_plot, ".png"),
           plot = p)
  }
}

# ----- Same plot but combine all DRT (no distinction with feeder or specific fleets)
demand_compare_summary_all <- demand_compare_summary %>%
  mutate(output_mode_drt = if_else(str_detect(output_mode, regex("drt", ignore_case = TRUE)),
                                   "drt",
                                   output_mode)) %>%
  # get total trips by DRT (we need one row per unique OD pair)
  group_by(input_mode, output_mode_drt, input_mode_trips_with_frac, scenario, fleet_size) %>%
  summarise(trips = sum(trips)) %>%
  ungroup() %>%
  filter(!is.na(output_mode_drt))


for (scenario_plot in scenarios) {
  for (fleet_size_plot in fleet_sizes) {

    # Filter the data
    plot_data <- demand_compare_summary_all %>%
      filter(fleet_size == fleet_size_plot, scenario == scenario_plot)

    # Generate the plot
    p <- ggplot(data = plot_data,
                aes(axis1 = input_mode, axis2 = output_mode_drt, y = trips)) +
      geom_alluvium(aes(fill = input_mode_trips_with_frac)) +
      geom_stratum() +
      geom_text(stat = "stratum", aes(label = input_mode), size = 3) +
      ggrepel::geom_text_repel(
        aes(label = output_mode_drt),
        stat = "stratum", size = 3, direction = "y", nudge_x = .35
      ) +
      scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
      scale_y_continuous(labels = scales::label_comma()) +
      scale_fill_brewer(type = "qual", palette = "Set1") +
      theme_bw() +
      theme(panel.border = element_blank()) +
      labs(
        title = "What modes did all trips transition from?",
        subtitle = scenario_labels[[scenario_plot]],
        y = "Number of trips",
        fill = "Initial mode \n(Initial mode share)",
        caption = paste0("DRT fleet size = ", fleet_size_plot)
      )

    print(p)

    ggsave(
      filename = paste0("plots/mode_share/mode_share_sankey_", scenario_plot, "_", fleet_size_plot, "_sum_drt.png"),
      plot = p)
  }
}



# Let's focus only on trips that came out as DRT
demand_compare_summary_drt = demand_compare_summary %>%
  filter(str_detect(output_mode, "drt")) %>%
  mutate(trips_moved_drt_frac = round((trips / input_mode_trips) * 100, 2)) %>% # % of Trips for a particular mode that shifted to DRT
  group_by(input_mode, fleet_size, scenario) %>%
  mutate(total_drt_frac = sum(trips_moved_drt_frac), # total % of trips that transition to DRT (regardless of standalone or feeder)
         mode_with_trips_moved_drt_frac = glue::glue("{input_mode} ({total_drt_frac} {'%'})")) %>% # initial mode + initial mode share (%)
  ungroup()

# save
write_csv(demand_compare_summary_drt, "plots/mode_share/mode_shift_drt.csv")



for (scenario_plot in scenarios) {
  for (fleet_size_plot in fleet_sizes) {

    # Filter the data
    plot_data <- demand_compare_summary_drt %>%
      filter(fleet_size == fleet_size_plot, scenario == scenario_plot)

    # Generate the plot
    p <- ggplot(data = plot_data,
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
        title = "What modes did DRT user trips transition from?",
        subtitle = scenario_labels[[scenario_plot]],
        y = "Number of trips",  # Change the y-axis label here
        fill = "Initial mode (% \nof trips that \nshifted to DRT)",  # Change the legend title here
        caption = paste0("DRT fleet size = ", fleet_size_plot)
      )


    print(p)

    ggsave(
      filename = paste0("plots/mode_share/mode_share_sankey_", scenario_plot, "_", fleet_size_plot, "_drt.png"),
      plot = p)
  }
}









# --- Facet plot --- What % of DRT trips is coming from each mode?
demand_compare_summary_drt_facet = demand_compare_summary_drt %>%
  mutate(scenario = case_when(
    str_detect(output_mode, "drtNE") ~ "drtNE",
    str_detect(output_mode, "drtNW") ~ "drtNW",
    # for consistency
    str_detect(scenario, "innerBUA") ~ "drtInner",
    str_detect(scenario, "all") ~ "drtAll",
    TRUE ~ scenario
  )) %>%
  group_by(fleet_size, scenario) %>%
  mutate(pct_drt_from_mode = trips / sum(trips)) %>%
  # rename output mode to drt | feeder
  mutate(output_mode = case_when(
    str_detect(output_mode, "feeder") ~ "feeder",
    TRUE ~ "standalone"
  )) %>%
  ungroup()



ggplot(data = demand_compare_summary_drt_facet %>%
         filter(input_mode != "bike"),
       aes(axis1 = input_mode, axis2 = output_mode, y = pct_drt_from_mode)) +
  geom_alluvium(aes(fill = input_mode_trips_with_frac)) +
  geom_stratum() +
  ggrepel::geom_text_repel(
    aes(label = input_mode),
    stat = "stratum", size = 3, direction = "y", nudge_x = -.35
  ) +
  # geom_text(stat = "stratum", aes(label = input_mode), size = 3) +
  ggrepel::geom_text_repel(
    aes(label = output_mode),
    stat = "stratum", size = 3, direction = "y", nudge_x = .35
  ) +
  scale_x_discrete(limits = c("Before", "After"), expand = c(0.15, 0.15)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  theme_bw() +
  theme(panel.border = element_blank(),
        legend.position="bottom") +
  labs(
    title = "What modes did DRT trips transition from?",
    subtitle = "Proportion of DRT trips coming from each mode",
    y = "Number of trips",
    fill = "Initial mode \n(Initial mode share)",
    caption = paste0("Modes not shown: Bike (~2%), Car Passenger (~23%) ")
  ) +
  facet_grid(vars(fleet_size), vars(scenario))


ggsave(filename = "plots/mode_share/mode_share_sankey_drt_facet.png", width = 14, height = 10)
